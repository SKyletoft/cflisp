use std::{borrow::Cow, marker::PhantomData};

use crate::*;

pub fn parse<'a>(source: &'a str, move_first: bool) -> Result<Vec<LanguageElement<'a>>> {
	let tokens: Vec<Token<'a>> = Token::by_byte(source)?;
	let mut res = Vec::<LanguageElement>::parse_with_no_tail(&tokens)?;
	if move_first {
		language_element::move_declarations_first(&mut res);
	}
	Ok(res)
}

//Non pointer types are also handled here ~~by accident~~ and are treated as 0-deep pointers. Whoops
///Tries to construct pointer variables, pointer structs or pointer assignments
fn parse_variable_and_function_declarations<'a, 'b>(
	tokens: TokenSlice<'a, 'b>,
) -> Result<(LanguageElement<'a>, TokenSlice<'a, 'b>)> {
	let (scv, scv_tail) = StaticConstVolatile::parse(tokens)?;
	let (var, var_tail) = Variable::parse(scv_tail).map_err(|_| error!(None))?;
	let res = match var_tail {
		// type var = {..};
		[Token::Assign, Token::Block(fields), Token::NewLine, tail @ ..] => {
			//parse tail is hardcoded to be []
			let (fields_parsed, _) = Vec::<StatementElement>::parse(fields)?;
			let elem = match &var.typ {
				Type::Struct(_) => LanguageElement::StructDeclarationAssignment {
					typ: var.typ,
					name: Cow::Borrowed(var.name),
					value: fields_parsed,
					is_static: scv.is_static,
					is_const: scv.is_const,
					is_volatile: scv.is_volatile,
				},
				Type::Ptr(_) | Type::Arr(_, _) => LanguageElement::VariableDeclarationAssignment {
					typ: var.typ,
					name: Cow::Borrowed(var.name),
					value: StatementElement::Array(fields_parsed),
					is_static: scv.is_static,
					is_const: scv.is_const,
					is_volatile: scv.is_volatile,
				},
				_ => return Err(error!(BadType, var_tail)),
			};
			(elem, tail)
		}

		// type var = ..
		[Token::Assign, tail @ ..] => {
			let (statement, rest) = StatementElement::parse(tail)?;
			(
				LanguageElement::VariableDeclarationAssignment {
					typ: var.typ,
					name: Cow::Borrowed(var.name),
					value: statement,
					is_static: scv.is_static,
					is_const: scv.is_const,
					is_volatile: scv.is_volatile,
				},
				rest,
			)
		}

		// type var;
		[Token::NewLine, tail @ ..] => (
			LanguageElement::VariableDeclaration {
				typ: var.typ,
				name: Cow::Borrowed(var.name),
				is_static: scv.is_static,
				is_const: scv.is_const,
				is_volatile: scv.is_volatile,
			},
			tail,
		),

		// type fun() {}
		[Token::Parentheses(args), Token::Block(code), tail @ ..] => {
			//Check type to refuse arrays
			if matches!(var.typ, Type::Arr(_, _)) {
				return Err(error!(BadType, var_tail));
			}
			//No static/const/volatile allowed on functions
			if tokens != scv_tail {
				return Err(error!(InvalidToken, var_tail));
			}
			let args = parse_argument_declaration(args)?;
			let block = Vec::<LanguageElement>::parse_with_no_tail(code)?;

			(
				LanguageElement::FunctionDeclaration {
					typ: var.typ,
					name: Cow::Borrowed(var.name),
					args,
					block,
				},
				tail,
			)
		}

		_ => return Err(error!(None)),
	};
	Ok(res)
}

///Matches a line of `Token`s into a `LanguageElement`, defaulting to a statement if no match can be made.
/// Does recursively parse all contained parts so a returned LanguageElement can be trusted as valid, apart
/// from type checking. (Struct types and fields count as type checking).
/// Struct and pointer patterns are not in this function.
fn parse_language_pattern<'a, 'b>(
	tokens: TokenSlice<'a, 'b>,
) -> Result<(LanguageElement<'a>, TokenSlice<'a, 'b>)> {
	let res: (LanguageElement<'a>, TokenSlice<'a, 'b>) = match tokens {
		//Struct member assignment
		[Token::Name(n), Token::FieldAccess, Token::Name(field), Token::Assign, tail @ ..] => {
			let name = helper::merge_name_and_field(n, field);
			let (rhs, rest) = StatementElement::parse(tail)?;
			(
				LanguageElement::VariableAssignment { name, value: rhs },
				rest,
			)
		}

		//Struct member assignment through pointer
		[Token::Name(n), Token::FieldPointerAccess, Token::Name(field), Token::Assign, tail @ ..] =>
		{
			let (rhs, rest) = StatementElement::parse(tail)?;
			(
				LanguageElement::StructFieldPointerAssignment {
					name: Cow::Borrowed(n),
					field: Cow::Borrowed(field),
					value: rhs,
				},
				rest,
			)
		}

		//Variable assignment
		[Token::Name(n), Token::Assign, tail @ ..] => {
			let (rhs, rest) = StatementElement::parse(tail)?;
			(
				LanguageElement::VariableAssignment {
					name: Cow::Borrowed(n),
					value: rhs,
				},
				rest,
			)
		}

		//Array assignment
		[Token::Name(n), Token::ArrayAccess(idx), Token::Assign, tail @ ..] => {
			let (mut idx_arr, _) = Vec::<StatementElement>::parse(idx)?;
			//yes, C interprets [0,1,2] as just [2]
			let lhs = idx_arr
				.pop()
				.ok_or_else(|| error!(IncompleteStatement, tokens))?;
			if !idx_arr.is_empty() {
				eprintln!("Warning: multiple statements separated by commas evaluate to just the last one");
			}
			let (rhs, rest) = StatementElement::parse(tail)?;
			(
				LanguageElement::PointerAssignment {
					ptr: StatementElement::Add {
						lhs: Box::new(lhs),
						rhs: Box::new(StatementElement::Var(Cow::Borrowed(n))),
					},
					value: rhs,
				},
				rest,
			)
		}

		//If else if
		[Token::If, Token::Parentheses(cond_tokens), Token::Block(then_code), Token::Else, Token::If, ..] =>
		{
			let condition = StatementElement::parse_with_no_tail(cond_tokens)?;
			let then = Vec::<LanguageElement>::parse_with_no_tail(then_code)?;
			let (else_then, rest) = LanguageElement::parse(&tokens[4..])?; // tail[-1..]

			(
				LanguageElement::IfStatement {
					condition,
					then,
					else_then: Some(vec![else_then]),
				},
				rest,
			)
		}

		//If else
		[Token::If, Token::Parentheses(cond_tokens), Token::Block(then_code), Token::Else, Token::Block(else_code), tail @ ..] =>
		{
			let condition = StatementElement::parse_with_no_tail(cond_tokens)?;
			let then = Vec::<LanguageElement>::parse_with_no_tail(then_code)?;
			let else_then = Vec::<LanguageElement>::parse_with_no_tail(else_code)?;

			(
				LanguageElement::IfStatement {
					condition,
					then,
					else_then: Some(else_then),
				},
				tail,
			)
		}

		//If
		[Token::If, Token::Parentheses(cond_tokens), Token::Block(code), tail @ ..] => {
			let condition = StatementElement::parse_with_no_tail(cond_tokens)?;
			let then = Vec::<LanguageElement>::parse_with_no_tail(code)?;

			(
				LanguageElement::IfStatement {
					condition,
					then,
					else_then: None,
				},
				tail,
			)
		}

		//For
		[Token::For, Token::Parentheses(init_cond_post), Token::Block(code), tail @ ..] => {
			let split = init_cond_post
				.split(|t| t == &Token::NewLine)
				.collect::<Vec<_>>();
			if split.len() != 3 {
				return Err(error!(BrokenForLoop, init_cond_post.as_slice()));
			}

			let condition = StatementElement::parse_with_no_tail(split[1])?;
			let init = Vec::<LanguageElement>::parse_with_no_tail(split[0])?;
			let post = Vec::<LanguageElement>::parse_with_no_tail(split[2])?;
			let body = Vec::<LanguageElement>::parse_with_no_tail(code)?;

			(
				LanguageElement::For {
					init,
					condition,
					post,
					body,
				},
				tail,
			)
		}

		//While
		[Token::While, Token::Parentheses(cond_tokens), Token::Block(code), tail @ ..] => {
			let condition = StatementElement::parse_with_no_tail(cond_tokens)?;
			let body = Vec::<LanguageElement>::parse_with_no_tail(code)?;

			(LanguageElement::While { condition, body }, tail)
		}

		[Token::Return, Token::NewLine, tail @ ..] => (LanguageElement::Return(None), tail),

		[Token::Return, tail @ ..] => {
			let (return_statement, rest) = StatementElement::parse(tail)?;
			(LanguageElement::Return(Some(return_statement)), rest)
		}

		[Token::TypeDef, Token::Struct, Token::Name(name), Token::Block(members), Token::Name(name2), Token::NewLine, tail @ ..] =>
		{
			if name != name2 {
				return Err(error!(BadStructName, &tokens[..6]));
			}
			//Reuse second definition
			// There will never be a tail, so we can throw it away
			let (res, _) = LanguageElement::parse(&[
				Token::Struct,
				Token::Name(name),
				Token::Block(members.clone()),
				Token::NewLine,
			])?;
			(res, tail)
		}

		[Token::Struct, Token::Name(name), Token::Block(fields), Token::NewLine, tail @ ..] => {
			let fields_variable = parse_struct_fields_declaration(fields)?;
			if !fields_variable
				.iter()
				.all(|Variable { typ, .. }| typ.is_native())
			{
				let fields: &[Token<'a>] = fields;
				return Err(error!(BadStructFields, fields));
			}
			let fields_native_variable = fields_variable
				.into_iter()
				.map(Variable::into)
				.collect::<Vec<NativeVariable>>();
			(
				LanguageElement::StructDefinition {
					name: Cow::Borrowed(name),
					members: fields_native_variable,
				},
				tail,
			)
		}

		[Token::Switch, ..] => return Err(error!(SwitchStatement, tokens)),
		[Token::Case | Token::Continue | Token::Default, ..] => {
			return Err(error!(BreakContinue, tokens));
		}

		_ => {
			let (statement, rest) = StatementElement::parse(tokens)?;
			(LanguageElement::Statement(statement), rest)
		}
	};
	Ok(res)
}

//Separate because it can't be easily implemented in a the giant match statement of `parse_language_pattern`
///Tries to match the exceptional case of pointer assignment
fn parse_pointer_assignment<'a, 'b>(
	tokens: TokenSlice<'a, 'b>,
) -> Result<(LanguageElement<'a>, TokenSlice<'a, 'b>)> {
	if !matches!(tokens.first(), Some(Token::Mul)) {
		return Err(error!(None));
	}
	let tokens = &tokens[1..];
	let assign_position = tokens
		.iter()
		.position(|t| t == &Token::Assign)
		.ok_or(error!(None))?;
	let lhs = StatementElement::parse_with_no_tail(&tokens[..assign_position])?;
	let (rhs, rhs_rest) = StatementElement::parse(&tokens[assign_position + 1..])?;
	Ok((
		LanguageElement::PointerAssignment {
			ptr: lhs,
			value: rhs,
		},
		rhs_rest,
	))
}

impl<'a, 'b> Parsable<'a, 'b> for LanguageElement<'a> {
	fn parse(tokens: TokenSlice<'a, 'b>) -> Result<(Self, TokenSlice<'a, 'b>)> {
		//Note: Don't miss that it only matches None errors, not any error
		match parse_pointer_assignment(tokens) {
			Err(CflispError {
				error: ErrorTypes::None,
				..
			}) => match parse_variable_and_function_declarations(tokens) {
				Err(CflispError {
					error: ErrorTypes::None,
					..
				}) => match parse_language_pattern(tokens) {
					Err(CflispError {
						error: ErrorTypes::None,
						..
					}) => Err(error!(MatchFail, tokens)),
					otherwise => otherwise,
				},
				otherwise => otherwise,
			},
			otherwise => otherwise,
		}
	}
}

impl<'a, 'b> Parsable<'a, 'b> for Vec<LanguageElement<'a>> {
	///Runs until stream is exhausted
	fn parse(tokens: TokenSlice<'a, 'b>) -> Result<(Self, TokenSlice<'a, 'b>)> {
		let mut vec = Vec::new();
		let mut tail = tokens;
		while !tail.is_empty() {
			let (item, rest) = LanguageElement::parse(tail)?;
			vec.push(item);
			tail = rest;
		}
		Ok((vec, tail))
	}
}

///Removes all comments from the source code.
/// Recursive multiline comments are treated properly.
pub fn remove_comments(s: &str) -> Cow<str> {
	let cow_str = Cow::Borrowed(s);
	let no_multilines = remove_multiline_comments(cow_str);
	remove_single_line_comments(no_multilines)
}

///Takes the entire source code and removes the rest of the line for each line with a `//`.
fn remove_single_line_comments(mut s: Cow<str>) -> Cow<str> {
	//Could be sped up if we didn't start the search over each time,
	// but apparently I can't write code that doesn't crash
	while let Some(idx) = s.find("//") {
		let end = s[idx..]
			.find('\n')
			.map(|v| v + idx)
			.unwrap_or_else(|| s.len());
		match &mut s {
			Cow::Owned(slice) => slice.replace_range(idx..end, ""),
			Cow::Borrowed(slice) => s = Cow::Owned(slice[..idx].to_string() + &slice[end..]),
		}
	}
	s
}

///Takes the entire source code and removes everything between `/*` and `*/`.
/// Works with recursive comments.
/// Should probably be run before the single line version?
fn remove_multiline_comments(mut s: Cow<str>) -> Cow<str> {
	const OPEN: &str = "/*";
	const CLOSE: &str = "*/";
	loop {
		//Find the first comment ending and find the last start before it,
		// remove everything between them, repeat till there are no comments.
		if s.is_empty() {
			return s;
		}
		let first_end = s.find(CLOSE);
		//No end: return early, even if it might be unbalanced
		if first_end.is_none() {
			return s;
		}
		let first_end = first_end.unwrap();
		let mut latest_start = 0;
		let mut maybe_latest_start = s[latest_start..first_end].find(OPEN);
		//No start: return as it is, even though we know it's unbalanced
		if maybe_latest_start.is_none() {
			return s;
		}
		while let Some(start) = maybe_latest_start {
			latest_start += start + 2;
			maybe_latest_start = s[latest_start..first_end].find(OPEN);
		}
		if let Cow::Owned(s) = &mut s {
			//Replace with `remove_range` is something like that ever gets stabilised?
			let from = latest_start - 2;
			let to = first_end + 2;
			s.replace_range(from..to, "");
		} else {
			s = Cow::Owned(s[..latest_start - 2].to_string() + &s[first_end + 2..]);
		}
	}
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub(crate) struct StaticConstVolatile {
	pub is_static: bool,
	pub is_const: bool,
	pub is_volatile: bool,
}

impl From<(&bool, &bool, &bool)> for StaticConstVolatile {
	fn from((s, c, v): (&bool, &bool, &bool)) -> Self {
		StaticConstVolatile {
			is_static: *s,
			is_const: *c,
			is_volatile: *v,
		}
	}
}

impl StaticConstVolatile {
	fn parse_single_static_const_volatile<'a, 'b>(
		mut self,
		tokens: TokenSlice<'a, 'b>,
	) -> Result<Option<(StaticConstVolatile, TokenSlice<'a, 'b>)>> {
		match tokens.split_first() {
			Some((Token::Static, rest)) => {
				if self.is_static {
					return Err(error!(InternalFailedStatic, tokens));
				}
				self.is_static = true;
				Ok(Some((self, rest)))
			}
			Some((Token::Const, rest)) => {
				if self.is_const {
					return Err(error!(InternalFailedConst, tokens));
				}
				self.is_const = true;
				Ok(Some((self, rest)))
			}
			Some((Token::Volatile, rest)) => {
				if self.is_volatile {
					return Err(error!(InternalFailedVolatile, tokens));
				}
				self.is_volatile = true;
				Ok(Some((self, rest)))
			}
			_ => Ok(None),
		}
	}
}

impl<'a, 'b> Parsable<'a, 'b> for StaticConstVolatile {
	fn parse(mut tokens: TokenSlice<'a, 'b>) -> Result<(Self, TokenSlice<'a, 'b>)> {
		let mut scv = StaticConstVolatile::default();
		while let Some((new_scv, tail)) = scv.parse_single_static_const_volatile(tokens)? {
			scv = new_scv;
			tokens = tail;
		}
		Ok((scv, tokens))
	}
}

///Gets the base type (primitive or struct) and the tail of the stream
fn parse_base_type<'a, 'b>(tokens: TokenSlice<'a, 'b>) -> Option<(Type<'a>, TokenSlice<'a, 'b>)> {
	match tokens {
		[Token::Decl(t), rest @ ..] => Some((t.into(), rest)),
		[Token::Name(n), rest @ ..] | [Token::Struct, Token::Name(n), rest @ ..] => {
			Some((Type::Struct(n), rest))
		}
		_ => None,
	}
}

///Adds a single pointer to `base_type` if there is one available
fn parse_pointers<'a, 'b>(
	tokens: TokenSlice<'a, 'b>,
	base_type: Type<'a>,
) -> Option<(Type<'a>, TokenSlice<'a, 'b>)> {
	match tokens.split_first() {
		Some((Token::Mul, rest)) => Some((Type::ptr(base_type), rest)),
		_ => None,
	}
}

impl<'a, 'b> Parsable<'a, 'b> for Type<'a> {
	///Reads a type from the token stream, returns the parsed type and the stream tail
	fn parse(tokens: TokenSlice<'a, 'b>) -> Result<(Self, TokenSlice<'a, 'b>)> {
		let (mut base, mut rest) = parse_base_type(tokens).ok_or(error!(BadType, tokens))?;
		while let Some((typ, tail)) = parse_pointers(rest, base.clone()) {
			base = typ;
			rest = tail;
		}
		Ok((base, rest))
	}
}

impl<'a, 'b> Parsable<'a, 'b> for Variable<'a> {
	fn parse(tokens: TokenSlice<'a, 'b>) -> Result<(Self, TokenSlice<'a, 'b>)> {
		let (typ, tail) = Type::parse(tokens)?;
		let (name, tail) = match tail.split_first() {
			Some((Token::Name(n), rest)) => (n, rest),
			_ => return Err(error!(MissingName, tail)),
		};
		let (typ, tail) = match tail.split_first() {
			Some((Token::ArrayAccess(len), rest)) => {
				let len = match len.as_slice() {
					[Token::Num(Number { val, .. })] => Ok(*val),
					_ => return Err(error!(NonConstantArrayLen, len.as_slice())),
				}?;
				(Type::Arr(Box::new(typ), len), rest)
			}
			_ => (typ, tail),
		};
		Ok((Variable { name, typ }, tail))
	}
}

fn parse_split_variables<'a, 'b>(
	tokens: TokenSlice<'a, 'b>,
	split: &Token<'a>,
) -> Result<Vec<Variable<'a>>> {
	let mut vec = Vec::new();

	if tokens.is_empty() {
		return Ok(vec);
	}

	let mut tail = tokens;
	while !tail.is_empty() {
		if let Ok((var, rest)) = Variable::parse(tail) {
			vec.push(var);
			tail = match rest {
				[token, rest @ ..] if token == split => rest,
				[] => break,
				_ => return Err(error!(ExcessTokens, rest)),
			};
		} else {
			return Err(error!(InvalidArguments, tail));
		}
	}

	Ok(vec)
}

fn parse_argument_declaration<'a, 'b>(tokens: TokenSlice<'a, 'b>) -> Result<Vec<Variable<'a>>> {
	parse_split_variables(tokens, &Token::Comma)
}

fn parse_struct_fields_declaration<'a, 'b>(
	tokens: TokenSlice<'a, 'b>,
) -> Result<Vec<Variable<'a>>> {
	parse_split_variables(tokens, &Token::NewLine)
}

pub(crate) fn parse_tokens_to_statements<'a, 'b>(
	tokens: TokenSlice<'a, 'b>,
) -> Result<Vec<Statement<'a>>> {
	//Without this we return Ok([[]]) instead of Ok([])
	// Adding a filter in the iterator would mean accepting double commas
	if tokens.is_empty() {
		return Ok(Vec::new());
	}
	tokens
		.split(|t| t == &Token::Comma)
		.map(|slice| StatementToken::from_tokens(slice))
		.collect::<Result<Vec<_>>>()
}

//This trait exists as a trait and not just a bunch of standalone functions so it can be used once for `ParserIterator`
pub trait Parsable<'a, 'b>
where
	Self: Sized,
{
	fn parse(tokens: TokenSlice<'a, 'b>) -> Result<(Self, TokenSlice<'a, 'b>)>;
	fn parse_with_no_tail(tokens: TokenSlice<'a, 'b>) -> Result<Self> {
		let (res, tail) = Self::parse(tokens)?;
		if !tail.is_empty() {
			return Err(error!(ExcessTokens, tokens));
		}
		Ok(res)
	}
}

pub struct ParserIterator<'a, 'b, T: Parsable<'a, 'b>> {
	token_stream: TokenSlice<'a, 'b>,
	_marker: PhantomData<T>,
}

impl<'a, 'b, T: Parsable<'a, 'b>> ParserIterator<'a, 'b, T> {
	pub fn new(token_stream: TokenSlice<'a, 'b>) -> Self {
		ParserIterator {
			token_stream,
			_marker: PhantomData,
		}
	}
}

impl<'a, 'b, T: Parsable<'a, 'b>> Iterator for ParserIterator<'a, 'b, T> {
	type Item = Result<T>;

	fn next(&mut self) -> Option<Self::Item> {
		let (out, tail) = match T::parse(self.token_stream) {
			Ok(ok) => ok,
			Err(e) => return Some(Err(e)),
		};
		self.token_stream = tail;
		Some(Ok(out))
	}
}

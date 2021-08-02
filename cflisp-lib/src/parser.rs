use std::{borrow::Cow, marker::PhantomData};

use crate::*;

pub fn parse<'a>(
	source: &'a str,
	move_first: bool,
) -> Result<Vec<LanguageElement<'a>>, ParseError> {
	let tokens: Vec<Token<'a>> = Token::by_byte(source)?;
	let original = construct_block(&tokens, move_first)?;
	let (res, tail) = Vec::<LanguageElement>::parse(&tokens)?;
	if !tail.is_empty() {
		return Err(ParseError::ExcessTokens(line!()));
	}
	assert_eq!(&res, &original);
	Ok(res)
}

///Wrapper around `construct_structure_from_tokens` to construct an entire block.
/// Does its own line split
fn construct_block<'a>(
	tokens: &[Token<'a>],
	move_first: bool,
) -> Result<Vec<LanguageElement<'a>>, ParseError> {
	let mut res = split_token_lines(tokens)
		.into_iter()
		.map(|token| construct_structure_from_tokens(token, move_first))
		.collect::<Result<Vec<_>, _>>()?;
	if move_first {
		statement_element::move_declarations_first(&mut res);
	}
	Ok(res)
}

fn construct_structure_from_tokens<'a>(
	tokens: &[Token<'a>],
	move_first: bool,
) -> Result<LanguageElement<'a>, ParseError> {
	construct_structure_with_pointers_from_tokens(tokens, move_first)
		.unwrap_or_else(|| construct_structure_from_tokens_via_pattern(tokens, move_first))
}

///Matches a line of `Token`s into a `LanguageElement`, defaulting to a statement if no match can be made.
/// Does recursively parse all contained parts so a returned LanguageElement can be trusted as valid, apart
/// from type checking. (Struct types and fields count as type checking).
/// Struct and pointer patterns are not in this function.
fn construct_structure_from_tokens_via_pattern<'a>(
	tokens: &[Token<'a>],
	move_first: bool,
) -> Result<LanguageElement<'a>, ParseError> {
	let element = {
		match tokens {
			//Struct member assignment
			[Token::Name(n), Token::FieldAccess, Token::Name(field), Token::Assign, ..] => {
				let name = helper::merge_name_and_field(n, field);
				let rhs = StatementElement::from_tokens(&tokens[4..])?;
				LanguageElement::VariableAssignment { name, value: rhs }
			}

			//Struct member assignment through pointer
			[Token::Name(n), Token::FieldPointerAccess, Token::Name(field), Token::Assign, ..] => {
				let rhs = StatementElement::from_tokens(&tokens[4..])?;
				LanguageElement::StructFieldPointerAssignment {
					name: Cow::Borrowed(n),
					field: Cow::Borrowed(field),
					value: rhs,
				}
			}

			//Variable assignment
			[Token::Name(n), Token::Assign, ..] => {
				let rhs = StatementElement::from_tokens(&tokens[2..])?;
				LanguageElement::VariableAssignment {
					name: Cow::Borrowed(n),
					value: rhs,
				}
			}

			//Array assignment
			[Token::Name(n), Token::ArrayAccess(idx), Token::Assign, ..] => {
				let lhs = StatementElement::from_tokens(idx)?;
				let rhs = StatementElement::from_tokens(&tokens[3..])?;
				LanguageElement::PointerAssignment {
					ptr: StatementElement::Add {
						lhs: Box::new(lhs),
						rhs: Box::new(StatementElement::Var(Cow::Borrowed(n))),
					},
					value: rhs,
				}
			}

			//Switch statement
			[Token::Switch, Token::Parentheses(expr), Token::Block(cases)] => {
				let _expr = StatementElement::from_tokens(expr)?;
				let _cases_parsed = construct_structure_from_tokens(cases, move_first)?;
				return Err(ParseError::SwitchStatement(line!()));
			}

			//Case
			[Token::Case, constant, Token::Colon, ..]
				if matches!(constant, Token::Bool(_) | Token::Char(_) | Token::Num(_)) =>
			{
				return Err(ParseError::SwitchStatement(line!()));
			}

			[Token::Break, ..] | [Token::Continue, ..] => {
				return Err(ParseError::BreakContinue(line!()));
			}

			//If else if
			[Token::If, Token::Parentheses(cond), Token::Block(then_code), Token::Else, Token::If, ..] =>
			{
				let condition_parsed = StatementElement::from_tokens(cond)?;
				let then_parsed = construct_block(then_code, move_first)?;
				let else_if_tokens = &tokens[4..];
				let else_if_parsed = construct_structure_from_tokens(else_if_tokens, move_first)?;
				LanguageElement::IfStatement {
					condition: condition_parsed,
					then: then_parsed,
					else_then: Some(vec![else_if_parsed]),
				}
			}

			//If else
			[Token::If, Token::Parentheses(cond), Token::Block(then_code), Token::Else, Token::Block(else_code)] =>
			{
				let condition_parsed = StatementElement::from_tokens(cond)?;
				let then_parsed = construct_block(then_code, move_first)?;
				let else_parsed = construct_block(else_code, move_first)?;
				LanguageElement::IfStatement {
					condition: condition_parsed,
					then: then_parsed,
					else_then: Some(else_parsed),
				}
			}

			//If
			[Token::If, Token::Parentheses(cond), Token::Block(code)] => {
				let condition_parsed = StatementElement::from_tokens(cond)?;
				let then_parsed = construct_block(code, move_first)?;
				LanguageElement::IfStatement {
					condition: condition_parsed,
					then: then_parsed,
					else_then: None,
				}
			}

			//For
			[Token::For, Token::Parentheses(init_cond_post), Token::Block(code)] => {
				let split = init_cond_post
					.split(|t| t == &Token::NewLine)
					.collect::<Vec<_>>();
				if split.len() != 3 {
					return Err(ParseError::BrokenForLoop(line!()));
				}

				let condition = StatementElement::from_tokens(split[1])?;

				let init = construct_block(split[0], move_first)?;
				let post = construct_block(split[2], move_first)?;
				let body = construct_block(code, move_first)?;

				LanguageElement::For {
					init,
					condition,
					post,
					body,
				}
			}

			//While
			[Token::While, Token::Parentheses(cond), Token::Block(code)] => {
				let condition_parsed = StatementElement::from_tokens(cond)?;
				let body_parsed = construct_block(code, move_first)?;
				LanguageElement::While {
					condition: condition_parsed,
					body: body_parsed,
				}
			}

			[Token::Return] => LanguageElement::Return(None),

			[Token::Return, ..] => {
				let return_statement = StatementElement::from_tokens(&tokens[1..])?;
				LanguageElement::Return(Some(return_statement))
			}

			[Token::TypeDef, Token::Struct, Token::Name(name), Token::Block(members), Token::Name(name2)] =>
			{
				if name != name2 {
					return Err(ParseError::BadStructName(line!()));
				}
				//Reuse second definition
				construct_structure_from_tokens(
					&[
						Token::Struct,
						Token::Name(name),
						Token::Block(members.clone()),
					],
					move_first,
				)?
			}

			[Token::Struct, Token::Name(name), Token::Block(fields)] => {
				let fields_variable = parse_struct_fields_declaration(fields)?;
				if !fields_variable
					.iter()
					.all(|Variable { typ, .. }| typ.is_native())
				{
					return Err(ParseError::BadStructFields(line!()));
				}
				let fields_native_variable = fields_variable
					.into_iter()
					.map(Variable::into)
					.collect::<Vec<NativeVariable>>();
				LanguageElement::StructDefinition {
					name: Cow::Borrowed(name),
					members: fields_native_variable,
				}
			}

			_ if !tokens.contains(&Token::Assign) => {
				let element = StatementElement::from_tokens(tokens)?;
				LanguageElement::Statement(element)
			}

			_ => {
				let len = 10usize.min(tokens.len());
				dbg!(&tokens[..len]);
				return Err(ParseError::MatchFail(line!()));
			}
		}
	};
	Ok(element)
}

//Non pointer types are also handled here ~~by accident~~ and are treated as 0-deep pointers. Whoops
///Tries to construct pointer variables, pointer structs or pointer assignments
fn construct_structure_with_pointers_from_tokens<'a>(
	tokens: &[Token<'a>],
	move_first: bool,
) -> Option<Result<LanguageElement<'a>, ParseError>> {
	if tokens.is_empty() {
		return None;
	}
	/*
	 *		type* name;
	 *		type* name =
	 *
	 *		*(expr) =
	 *
	 *		struct_name* name =
	 *		struct_name* name;
	 *		struct struct_name* name =
	 *		struct struct_name* name;
	 *
	 *		static type* name =
	 *		static type* name;
	 *		static struct struct_name* name =
	 *		static struct struct_name* name;
	 *		static struct_name* name =
	 *		static struct_name* name;
	 */
	match &tokens[0] {
		Token::Static => construct_structure_with_pointers_from_tokens(&tokens[1..], move_first)
			.map(|res| res.and_then(LanguageElement::make_static)),
		Token::Const => construct_structure_with_pointers_from_tokens(&tokens[1..], move_first)
			.map(|res| res.and_then(LanguageElement::make_const)),
		Token::Volatile => construct_structure_with_pointers_from_tokens(&tokens[1..], move_first)
			.map(|res| res.and_then(LanguageElement::make_volatile)),
		//	`type* name` and `type* name` =
		Token::Decl(t) => {
			let mut tokens_slice = &tokens[1..];
			let mut t = t.into();
			while let Some(Token::Mul) = tokens_slice.get(0) {
				tokens_slice = &tokens_slice[1..];
				t = Type::ptr(t);
			}
			match tokens_slice {
				[Token::Name(n)] => Some(Ok(LanguageElement::VariableDeclaration {
					typ: t,
					name: Cow::Borrowed(n),
					is_static: false,
					is_const: false,
					is_volatile: false,
				})),
				[Token::Name(n), Token::ArrayAccess(len)] => {
					let res = match len.as_slice() {
						[Token::Num(Number { val, .. })] => Ok(*val),
						_ => Err(ParseError::NonConstantArrayLen(line!())),
					}
					.map(|len| {
						t = Type::Arr(Box::new(t), len);
						LanguageElement::VariableDeclaration {
							typ: t,
							name: Cow::Borrowed(n),
							is_static: false,
							is_const: false,
							is_volatile: false,
						}
					});
					Some(res)
				}
				[Token::Name(n), Token::Assign, ..] => {
					let res = StatementElement::from_tokens(&tokens_slice[2..]).map(|statement| {
						LanguageElement::VariableDeclarationAssignment {
							typ: t,
							name: Cow::Borrowed(n),
							value: statement,
							is_static: false,
							is_const: false,
							is_volatile: false,
						}
					});
					Some(res)
				}
				[Token::Name(n), Token::ArrayAccess(len), Token::Assign, ..] => {
					let res = match len.as_slice() {
						[Token::Num(Number { val, .. })] => Ok(*val),
						_ => Err(ParseError::NonConstantArrayLen(line!())),
					}
					.and_then(|len| {
						t = Type::Arr(Box::new(t), len);
						StatementElement::from_tokens(&tokens_slice[3..]).map(|statement| {
							LanguageElement::VariableDeclarationAssignment {
								typ: t,
								name: Cow::Borrowed(n),
								value: statement,
								is_static: false,
								is_const: false,
								is_volatile: false,
							}
						})
					});
					Some(res)
				}
				//Function
				[Token::Name(n), Token::Parentheses(args_tokens), Token::Block(code)] => {
					let args = match parse_argument_declaration(args_tokens) {
						Ok(res) => res,
						Err(e) => return Some(Err(e)),
					};
					let block = match construct_block(code, move_first) {
						Ok(res) => res,
						Err(e) => return Some(Err(e)),
					};
					let res = LanguageElement::FunctionDeclaration {
						typ: t,
						name: Cow::Borrowed(n),
						args,
						block,
					};
					Some(Ok(res))
				}
				_ => None,
			}
		}
		//Struct type name
		Token::Name(n) => {
			let mut tokens_slice = &tokens[1..];
			let mut t = Type::Struct(n);
			while let Some(Token::Mul) = tokens_slice.get(0) {
				tokens_slice = &tokens_slice[1..];
				t = Type::ptr(t);
			}
			match tokens_slice {
				[Token::Name(n)] => Some(Ok(LanguageElement::VariableDeclaration {
					typ: t,
					name: Cow::Borrowed(n),
					is_static: false,
					is_const: false,
					is_volatile: false,
				})),
				[Token::Name(n), Token::Assign, Token::Block(fields)] => {
					let res = parse_tokens_to_statements(fields).and_then(|tokens| {
						tokens
							.into_iter()
							.map(StatementElement::from_statement_tokens)
							.collect::<Result<Vec<_>, _>>()
							.map(|members| LanguageElement::StructDeclarationAssignment {
								typ: t,
								name: Cow::Borrowed(n),
								value: members,
								is_static: false,
								is_const: false,
								is_volatile: false,
							})
					});
					Some(res)
				}
				[Token::Name(n), Token::Assign, ..] => {
					let res = StatementElement::from_tokens(&tokens_slice[2..]).map(|rhs| {
						LanguageElement::VariableDeclarationAssignment {
							typ: t,
							name: Cow::Borrowed(n),
							value: rhs,
							is_static: false,
							is_const: false,
							is_volatile: false,
						}
					});
					Some(res)
				}
				//Function
				[Token::Name(n), Token::Parentheses(args_tokens), Token::Block(code)] => {
					let args = match parse_argument_declaration(args_tokens) {
						Ok(res) => res,
						Err(e) => return Some(Err(e)),
					};
					let block = match construct_block(code, move_first) {
						Ok(res) => res,
						Err(e) => return Some(Err(e)),
					};
					let res = LanguageElement::FunctionDeclaration {
						typ: t,
						name: Cow::Borrowed(n),
						args,
						block,
					};
					Some(Ok(res))
				}
				_ => None,
			}
		}
		Token::Mul => {
			let assign_idx = tokens.iter().position(|t| t == &Token::Assign);
			assign_idx.map(|assign_idx| {
				//Skip the first deref as the LanaguageElement::PointerAssignment::ptr already expects a pointer
				let lhs = StatementElement::from_tokens(&tokens[1..assign_idx]);
				let rhs = StatementElement::from_tokens(&tokens[assign_idx + 1..]);
				lhs.and_then(|lhs| {
					rhs.map(|rhs| LanguageElement::PointerAssignment {
						ptr: lhs,
						value: rhs,
					})
				})
			})
		}
		Token::Struct => {
			if !matches!(tokens.get(1), Some(Token::Name(_))) {
				return None;
			}
			construct_structure_with_pointers_from_tokens(&tokens[1..], move_first)
		}
		_ => None,
	}
}

fn parse_variable_and_function_declarations<'a, 'b>(
	tokens: TokenSlice<'a, 'b>,
) -> Result<(LanguageElement<'a>, TokenSlice<'a, 'b>), ParseError> {
	let (scv, scv_tail) = StaticConstVolatile::parse(tokens)?;
	let (var, var_tail) = match Variable::parse(scv_tail) {
		Ok(ok) => ok,
		Err(_) => return Err(ParseError::None),
	};
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
				_ => return Err(ParseError::BadType(line!())),
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
				return Err(ParseError::BadType(line!()));
			}
			//No static/const/volatile allowed on functions
			if tokens != scv_tail {
				return Err(ParseError::InvalidToken(line!()));
			}
			let args = parse_argument_declaration(args)?;
			let block = construct_block(code, false)?;
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

		_ => return Err(ParseError::None),
	};
	Ok(res)
}

fn parse_language_pattern<'a, 'b>(
	tokens: TokenSlice<'a, 'b>,
) -> Result<(LanguageElement<'a>, TokenSlice<'a, 'b>), ParseError> {
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
				.ok_or(ParseError::IncompleteStatement(line!()))?;
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
			let (condition, cond_rest) = StatementElement::parse(cond_tokens)?;
			let (then, then_rest) = Vec::<LanguageElement>::parse(then_code)?;
			let (else_then, rest) = LanguageElement::parse(&tokens[4..])?; // tail[-1..]
			if !cond_rest.is_empty() || !then_rest.is_empty() {
				return Err(ParseError::ExcessTokens(line!()));
			}
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
			let (condition, cond_rest) = StatementElement::parse(cond_tokens)?;
			let (then, then_rest) = Vec::<LanguageElement>::parse(then_code)?;
			let (else_then, else_rest) = Vec::<LanguageElement>::parse(else_code)?;
			if !cond_rest.is_empty() || !then_rest.is_empty() || !else_rest.is_empty() {
				return Err(ParseError::ExcessTokens(line!()));
			}
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
			let (condition, cond_rest) = StatementElement::parse(cond_tokens)?;
			let (then, then_rest) = Vec::<LanguageElement>::parse(code)?;
			if !cond_rest.is_empty() || !then_rest.is_empty() {
				return Err(ParseError::ExcessTokens(line!()));
			}
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
				return Err(ParseError::BrokenForLoop(line!()));
			}

			let (condition, cond_rest) = StatementElement::parse(split[1])?;
			let (init, init_rest) = Vec::<LanguageElement>::parse(split[0])?;
			let (post, post_rest) = Vec::<LanguageElement>::parse(split[2])?;
			let (body, body_rest) = Vec::<LanguageElement>::parse(code)?;

			if !cond_rest.is_empty()
				|| !init_rest.is_empty()
				|| !post_rest.is_empty()
				|| !body_rest.is_empty()
			{
				return Err(ParseError::ExcessTokens(line!()));
			}

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
			let (condition, cond_rest) = StatementElement::parse(cond_tokens)?;
			let (body, body_rest) = Vec::<LanguageElement>::parse(code)?;
			if !cond_rest.is_empty() || !body_rest.is_empty() {
				return Err(ParseError::ExcessTokens(line!()));
			}
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
				return Err(ParseError::BadStructName(line!()));
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
				return Err(ParseError::BadStructFields(line!()));
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

		_ if !tokens.contains(&Token::Assign) => {
			let (statement, rest) = StatementElement::parse(tokens)?;
			(LanguageElement::Statement(statement), rest)
		}

		[Token::Switch, ..] => return Err(ParseError::SwitchStatement(line!())),
		[Token::Case | Token::Continue | Token::Default, ..] => {
			return Err(ParseError::BreakContinue(line!()))
		}

		_ => {
			let len = 10usize.min(tokens.len());
			dbg!(&tokens[..len]);
			return Err(ParseError::MatchFail(line!()));
		}
	};
	Ok(res)
}

fn parse_pointer_assignment<'a, 'b>(
	tokens: TokenSlice<'a, 'b>,
) -> Result<(LanguageElement<'a>, TokenSlice<'a, 'b>), ParseError> {
	if !matches!(tokens.first(), Some(Token::Mul)) {
		return Err(ParseError::None);
	}
	let tokens = &tokens[1..];
	let assign_position = tokens
		.iter()
		.position(|t| t == &Token::Assign)
		.ok_or(ParseError::None)?;
	let (lhs, lhs_rest) = StatementElement::parse(&tokens[..assign_position])?;
	let (rhs, rhs_rest) = StatementElement::parse(&tokens[assign_position + 1..])?;
	if !lhs_rest.is_empty() {
		return Err(ParseError::ExcessTokens(line!()));
	}
	let elem = LanguageElement::PointerAssignment {
		ptr: lhs,
		value: rhs,
	};
	Ok((elem, rhs_rest))
}

impl<'a, 'b> Parsable<'a, 'b> for LanguageElement<'a> {
	fn parse(tokens: TokenSlice<'a, 'b>) -> Result<(Self, TokenSlice<'a, 'b>), ParseError> {
		match parse_pointer_assignment(tokens) {
			Ok(ok) => Ok(ok),
			Err(ParseError::None) => match parse_variable_and_function_declarations(tokens) {
				Ok(ok) => Ok(ok),
				Err(ParseError::None) => match parse_language_pattern(tokens) {
					Ok(ok) => Ok(ok),
					Err(ParseError::None) => Err(ParseError::MatchFail(line!())),
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
	fn parse(tokens: TokenSlice<'a, 'b>) -> Result<(Self, TokenSlice<'a, 'b>), ParseError> {
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

///Splits tokens at NewLines and after `Block`s that are *not* followed by `Else`
fn split_token_lines<'a, 'b>(tokens: &'a [Token<'b>]) -> Vec<&'a [Token<'b>]> {
	let mut vec = Vec::new();
	let mut last = 0;
	for idx in 0..tokens.len() {
		if matches!(tokens[idx], Token::NewLine) {
			let slice = &tokens[last..idx];
			if !slice.is_empty() {
				vec.push(slice);
			}
			last = idx + 1;
		}
		if matches!(tokens[idx], Token::Block(_))
			&& !matches!(tokens.get(idx + 1), Some(Token::Else))
			&& !matches!(
				(tokens.get(idx + 1), tokens.get(idx + 2)),
				(Some(Token::Name(_)), Some(Token::NewLine))
			) {
			let slice = &tokens[last..=idx];
			if !slice.is_empty() {
				vec.push(slice);
			}
			last = idx + 1;
		}
	}
	//And add whatever remains
	let slice = &tokens[last..];
	if !slice.is_empty() {
		vec.push(slice);
	}
	vec
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
struct StaticConstVolatile {
	pub is_static: bool,
	pub is_const: bool,
	pub is_volatile: bool,
}

impl StaticConstVolatile {
	fn parse_single_static_const_volatile<'a, 'b>(
		mut self,
		tokens: TokenSlice<'a, 'b>,
	) -> Result<Option<(StaticConstVolatile, TokenSlice<'a, 'b>)>, ParseError> {
		match tokens.split_first() {
			Some((Token::Static, rest)) => {
				if self.is_static {
					return Err(ParseError::InternalFailedStatic(line!()));
				}
				self.is_static = true;
				Ok(Some((self, rest)))
			}
			Some((Token::Const, rest)) => {
				if self.is_const {
					return Err(ParseError::InternalFailedConst(line!()));
				}
				self.is_const = true;
				Ok(Some((self, rest)))
			}
			Some((Token::Volatile, rest)) => {
				if self.is_volatile {
					return Err(ParseError::InternalFailedVolatile(line!()));
				}
				self.is_volatile = true;
				Ok(Some((self, rest)))
			}
			_ => Ok(None),
		}
	}
}

impl<'a, 'b> Parsable<'a, 'b> for StaticConstVolatile {
	fn parse(mut tokens: TokenSlice<'a, 'b>) -> Result<(Self, TokenSlice<'a, 'b>), ParseError> {
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
	fn parse(tokens: TokenSlice<'a, 'b>) -> Result<(Self, TokenSlice<'a, 'b>), ParseError> {
		let (mut base, mut rest) = parse_base_type(tokens).ok_or(ParseError::BadType(line!()))?;
		while let Some((typ, tail)) = parse_pointers(rest, base.clone()) {
			base = typ;
			rest = tail;
		}
		Ok((base, rest))
	}
}

impl<'a, 'b> Parsable<'a, 'b> for Variable<'a> {
	fn parse(tokens: TokenSlice<'a, 'b>) -> Result<(Self, TokenSlice<'a, 'b>), ParseError> {
		let (typ, tail) = Type::parse(tokens)?;
		let (name, tail) = match tail.split_first() {
			Some((Token::Name(n), rest)) => (n, rest),
			_ => return Err(ParseError::MissingName(line!())),
		};
		let (typ, tail) = match tail.split_first() {
			Some((Token::ArrayAccess(len), rest)) => {
				let len = match len.as_slice() {
					[Token::Num(Number { val, .. })] => Ok(*val),
					_ => Err(ParseError::NonConstantArrayLen(line!())),
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
) -> Result<Vec<Variable<'a>>, ParseError> {
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
				_ => return Err(ParseError::ExcessTokens(line!())),
			};
		} else {
			dbg!(tail, split);
			return Err(ParseError::InvalidArguments(line!()));
		}
	}

	Ok(vec)
}

fn parse_argument_declaration<'a, 'b>(
	tokens: TokenSlice<'a, 'b>,
) -> Result<Vec<Variable<'a>>, ParseError> {
	parse_split_variables(tokens, &Token::Comma)
}

fn parse_struct_fields_declaration<'a, 'b>(
	tokens: TokenSlice<'a, 'b>,
) -> Result<Vec<Variable<'a>>, ParseError> {
	parse_split_variables(tokens, &Token::NewLine)
}

pub(crate) fn parse_tokens_to_statements<'a, 'b>(
	tokens: TokenSlice<'a, 'b>,
) -> Result<Vec<Statement<'a>>, ParseError> {
	//Without this we return Ok([[]]) instead of Ok([])
	// Adding a filter in the iterator would mean accepting double commas
	if tokens.is_empty() {
		return Ok(Vec::new());
	}
	tokens
		.split(|t| t == &Token::Comma)
		.map(|slice| StatementToken::from_tokens(slice))
		.collect::<Result<Vec<_>, _>>()
}

//This trait exists as a trait and not just a bunch of standalone functions so it can be used once for `ParserIterator`
pub trait Parsable<'a, 'b>
where
	Self: Sized,
{
	fn parse(tokens: TokenSlice<'a, 'b>) -> Result<(Self, TokenSlice<'a, 'b>), ParseError>;
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
	type Item = Result<T, ParseError>;

	fn next(&mut self) -> Option<Self::Item> {
		let (out, tail) = match T::parse(self.token_stream) {
			Ok(ok) => ok,
			Err(e) => return Some(Err(e)),
		};
		self.token_stream = tail;
		Some(Ok(out))
	}
}

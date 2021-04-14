use crate::*;

///All possible tokens in the source (after comments have been removed)
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
	Add,
	AdrOf,
	AlignAs,
	AlignOf,
	Assign,
	Atomic,
	Auto,
	BitAnd,
	BitNot,
	BitOr,
	Bool(bool),
	BoolAnd,
	BoolNot,
	BoolOr,
	Break,
	Case,
	Char(char),
	Cmp,
	Colon,
	Comma,
	Complex,
	Const,
	Continue,
	Decl(NativeType),
	Default,
	Deref,
	Div,
	Do,
	Double,
	Else,
	Enum,
	Extern,
	FieldAccess,
	FieldPointerAccess,
	Float,
	For,
	Generic,
	Goto,
	GreaterThan,
	GreaterThanEqual,
	If,
	Imaginary,
	Inline,
	LessThan,
	LessThanEqual,
	Long,
	LShift,
	Mod,
	Mul,
	Name(&'a str),
	NewLine,
	NoReturn,
	NotCmp,
	Num(isize),
	Register,
	Restrict,
	Return,
	RShift,
	Short,
	Signed,
	SizeOf,
	Static,
	StaticAssert,
	Struct,
	Sub,
	Switch,
	ThreadLocal,
	Ternary(&'a str),
	TypeDef,
	Union,
	UnparsedArrayAccess(&'a str),
	UnparsedBlock(&'a str),
	UnparsedParentheses(&'a str),
	UnparsedSource(&'a str),
	Unsigned,
	Volatile,
	While,
	Xor,
}

impl<'a> Token<'a> {
	///Splits a string into a list of tokens by matching pattern by pattern instead of
	/// splitting and then converting into tokens
	pub(crate) fn by_byte(source: &'a str) -> Result<Vec<Token<'a>>, ParseError> {
		let mut src = source.trim_start();
		let mut vec = Vec::new();
		while !src.is_empty() {
			let (token, rest) = lexer::get_token(src)?;
			vec.push(token);
			src = rest.trim_start();
		}
		//vec = Token::fix_deref(vec);
		Ok(vec)
	}

	///Parses string from `Token::UnparsedBlock`. Allows multiple lines
	pub(crate) fn parse_block_tokens(s: &'a str) -> Result<Vec<Token<'a>>, ParseError> {
		if s.is_empty() {
			return Ok(Vec::new());
		}
		//Token::parse_str_to_vec(s)
		Token::by_byte(s)
	}

	///Converts string from `Token::UnparsedBlock` into `StatementToken`s
	pub(crate) fn parse_statement_tokens(
		s: &'a str,
	) -> Result<Vec<StatementToken<'a>>, ParseError> {
		//let res = Token::parse_str_to_vec(s)?;
		let res = Token::by_byte(s)?;
		if res.contains(&NewLine) {
			return Err(ParseError(line!(), "Statement ended early?"));
		}
		StatementToken::from_tokens(&res)
	}

	///Parses `Token::UnparsedParentheses` as a list of statements. (Function *call*, not declaration)
	pub(crate) fn parse_arguments_tokens(s: &'a str) -> Result<Vec<Statement<'a>>, ParseError> {
		if s.is_empty() {
			Ok(vec![])
		} else {
			s.split(',')
				//.map(|slice| Token::parse_str_to_vec(slice).map(|t| StatementToken::from_tokens(&t)))
				.map(|slice| Token::by_byte(slice).map(|t| StatementToken::from_tokens(&t)))
				.collect::<Result<Result<Vec<Statement<'a>>, _>, _>>()?
		}
	}

	///Parses string from `Token::UnparsedParentheses` as a list of types and names. (Function *declaration*, not call)
	pub(crate) fn parse_argument_list_tokens(s: &'a str) -> Result<Vec<Variable<'a>>, ParseError> {
		if s.is_empty() {
			return Ok(Vec::new());
		}
		let tokens = Token::by_byte(s)?;
		if tokens.contains(&NewLine) {
			dbg!(s, tokens);
			return Err(ParseError(line!(), "Statement ended early?"));
		}

		let mut arguments = Vec::new();
		let mut remaining_slice = tokens.as_slice();

		while !remaining_slice.is_empty() {
			let (var, rest) = get_pattern(remaining_slice)?;
			remaining_slice = rest;
			if remaining_slice.get(0) == Some(&Comma) {
				remaining_slice = &remaining_slice[1..];
			}
			arguments.push(var);
		}
		if !remaining_slice.is_empty() {
			dbg!(&tokens, s, arguments, remaining_slice);
			return Err(ParseError(
				line!(),
				"Argument list wasn't empty when done? Trailing comma?",
			));
		}

		Ok(arguments)
	}

	///Parses string from `Token::UnparsedParentheses` as a list of types and names. (Struct *declaration*, not construction)
	pub(crate) fn parse_struct_member_tokens(s: &'a str) -> Result<Vec<Variable<'a>>, ParseError> {
		if s.is_empty() {
			return Ok(Vec::new());
		}
		let tokens = Token::by_byte(s)?;
		let mut arguments = Vec::new();
		for slice in tokens.windows(3).step_by(3) {
			if let [Decl(t), Name(n), NewLine] = slice {
				arguments.push(Variable {
					typ: t.into(),
					name: *n,
				})
			} else {
				dbg!(s, tokens);
				return Err(ParseError(line!(), "Couldn't struct members"));
			}
		}
		Ok(arguments)
	}
}

fn get_pattern<'a, 'b>(
	slice: &'b [Token<'a>],
) -> Result<(Variable<'a>, &'b [Token<'a>]), ParseError> {
	let mut typ: Type;
	let mut reduced = &slice[1..];
	match slice.get(0) {
		Some(Decl(t)) => typ = t.into(),
		Some(Name(t)) => typ = Type::Struct(t),
		_ => {
			dbg!(slice);
			return Err(ParseError(line!(), "Couldn't parse argument list"));
		}
	}
	while reduced.get(0) == Some(&Mul) {
		typ = Type::ptr(typ);
		reduced = &reduced[1..];
	}
	if let Some(Name(n)) = reduced.get(0) {
		Ok((Variable { typ, name: *n }, &reduced[1..]))
	} else {
		Err(ParseError(line!(), "Couldn't parse argument list"))
	}
}

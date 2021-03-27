use crate::*;

///All possible tokens in the source (after comments have been removed)
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Token<'a> {
	Decl(NativeType),
	If,
	Else,
	Assign,
	Add,
	Sub,
	Mul,
	Div,
	Mod,
	Cmp,
	NotCmp,
	GreaterThan,
	GreaterThanEqual,
	LessThan,
	LessThanEqual,
	RShift,
	LShift,
	Char(char),
	Bool(bool),
	AdrOf,
	Deref,
	Name(&'a str),
	BitAnd,
	BitOr,
	BoolAnd,
	BoolOr,
	Xor,
	BoolNot,
	BitNot,
	Return,
	Num(isize),
	For,
	While,
	Break,
	Continue,
	Switch,
	Static,
	Struct,
	TypeDef,
	FieldAccess,
	FieldPointerAccess,
	UnparsedSource(&'a str),
	UnparsedBlock(&'a str),
	UnparsedParentheses(&'a str),
	UnparsedArrayAccess(&'a str),
	NewLine,
	Comma,
	Union,
	Complex,
	Long,
	Short,
	Float,
	Double,
	Volatile,
	Const,
	Case,
	Default,
	Extern,
	Signed,
	Unsigned,
	Goto,
	Inline,
	Register,
	Restrict,
	Enum,
	Do,
	SizeOf,
	AlignAs,
	AlignOf,
	Atomic,
	Generic,
	Imaginary,
	NoReturn,
	StaticAssert,
	ThreadLocal,
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
		s.split(',')
			//.map(|slice| Token::parse_str_to_vec(slice).map(|t| StatementToken::from_tokens(&t)))
			.map(|slice| Token::by_byte(slice).map(|t| StatementToken::from_tokens(&t)))
			.collect::<Result<Result<Vec<Statement<'a>>, _>, _>>()?
	}

	///Parses string from `Token::UnparsedParentheses` as a list of types and names. (Function *declaration*, not call)
	pub(crate) fn parse_argument_list_tokens(s: &'a str) -> Result<Vec<Variable<'a>>, ParseError> {
		if s.is_empty() {
			return Ok(Vec::new());
		}
		let tokens = Token::by_byte(s)?;
		if tokens.contains(&NewLine) {
			return Err(ParseError(line!(), "Statement ended early?"));
		}
		if tokens.len() % 2 != 0 {
			return Err(ParseError(
				line!(),
				"Wrong amount of tokens in function argument declaration",
			));
		}
		let mut arguments = Vec::new();
		for slice in tokens.windows(2).step_by(2) {
			if let [Decl(t), Name(n)] = slice {
				arguments.push(Variable {
					typ: t.clone(),
					name: *n,
				})
			} else {
				return Err(ParseError(line!(), "Couldn't parse argument list"));
			}
		}
		Ok(arguments)
	}

	///Parses string from `Token::UnparsedParentheses` as a list of types and names. (Struct *declaration*, not construction)
	pub(crate) fn parse_struct_member_tokens(s: &'a str) -> Result<Vec<Variable<'a>>, ParseError> {
		if s.is_empty() {
			return Ok(Vec::new());
		}
		let tokens = Token::by_byte(s)?;
		if tokens.len() % 3 != 0 {
			return Err(ParseError(
				line!(),
				"Wrong amount of tokens in struct declaration",
			));
		}
		let mut arguments = Vec::new();
		for slice in tokens.windows(3).step_by(3) {
			if let [Decl(t), Name(n), NewLine] = slice {
				arguments.push(Variable {
					typ: t.clone(),
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

use std::borrow::Cow;

use crate::*;

///All possible tokens in the source (after comments have been removed)
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
	Add,
	AdrOf,
	AlignAs,
	AlignOf,
	AnyCast,
	Assign,
	Atomic,
	Auto,
	BitAnd,
	BitNot,
	BitOr,
	Bool(bool),
	BoolAnd,
	BoolCast,
	BoolNot,
	BoolOr,
	Break,
	Case,
	Char(char),
	CharCast,
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
	IntCast,
	LessThan,
	LessThanEqual,
	Long,
	LShift,
	Mod,
	Mul,
	Name(&'a str),
	Namespace,
	NamespaceSplitter,
	NewLine,
	NoReturn,
	NotCmp,
	Num(Number),
	Register,
	Restrict,
	Return,
	RShift,
	Short,
	Signed,
	SizeOf,
	Static,
	StaticAssert,
	StringLiteral(&'a str),
	Struct,
	Sub,
	Switch,
	ThreadLocal,
	Ternary(&'a str),
	TypeDef,
	UintCast,
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
		if source.is_empty() {
			return Ok(Vec::new());
		}
		let mut src = source.trim_start();
		let mut vec = Vec::new();
		while !src.is_empty() {
			let (token, rest) = lexer::get_token(src)?;
			vec.push(token);
			src = rest.trim_start();
		}
		Ok(vec)
	}

	///Parses string from `Token::UnparsedBlock`. Allows multiple lines
	pub(crate) fn parse_block_tokens(s: &'a str) -> Result<Vec<Token<'a>>, ParseError> {
		if s.is_empty() {
			return Ok(Vec::new());
		}
		Token::by_byte(s)
	}

	///Converts string from `Token::UnparsedBlock` into `StatementToken`s
	pub(crate) fn parse_statement_tokens(
		s: &'a str,
	) -> Result<Vec<StatementToken<'a>>, ParseError> {
		let res = Token::by_byte(s)?;
		if res.contains(&NewLine) {
			return Err(ParseError::IncompleteStatement(line!()));
		}
		StatementToken::from_tokens(&res)
	}

	///Parses `Token::UnparsedParentheses` as a list of statements. (Function *call*, not declaration)
	pub(crate) fn parse_arguments_tokens(s: &'a str) -> Result<Vec<Statement<'a>>, ParseError> {
		if s.is_empty() {
			Ok(vec![])
		} else {
			s.split(',')
				.map(|slice| Token::by_byte(slice).map(|t| StatementToken::from_tokens(&t)))
				.collect::<Result<Result<Vec<Statement<'a>>, _>, _>>()?
		}
	}

	///Parses string from `Token::UnparsedParentheses` as a list of types and names. (Function *declaration*, not call)
	pub(crate) fn parse_argument_list_tokens(s: &'a str) -> Result<Vec<Variable<'a>>, ParseError> {
		let get_pattern = |slice: &[Token<'a>]| -> Result<Variable<'a>, ParseError> {
			let mut typ = match slice.get(0) {
				Some(Decl(t)) => t.into(),
				Some(Name(t)) => Type::Struct(t),
				_ => {
					dbg!(slice);
					return Err(ParseError::InvalidArguments(line!()));
				}
			};
			let mut reduced = &slice[1..];
			while reduced.get(0) == Some(&Mul) {
				typ = Type::ptr(typ);
				reduced = &reduced[1..];
			}
			if let [Token::Name(n)] = reduced {
				Ok(Variable { typ, name: *n })
			} else {
				return Err(ParseError::InvalidArguments(line!()));
			}
		};
		if s.is_empty() {
			return Ok(Vec::new());
		}
		Token::by_byte(s)?
			.split(|t| t == &Token::Comma)
			.map(get_pattern)
			.collect()
	}

	///Parses string from `Token::UnparsedParentheses` as a list of types and names. (Struct *declaration*, not construction)
	pub(crate) fn parse_struct_member_tokens(
		s: &'a str,
	) -> Result<Vec<NativeVariable<'a>>, ParseError> {
		if s.is_empty() {
			return Ok(Vec::new());
		}
		let tokens = Token::by_byte(s)?;
		let mut arguments = Vec::new();
		for slice in tokens.windows(3).step_by(3) {
			if let [Decl(t), Name(n), NewLine] = slice {
				arguments.push(NativeVariable {
					typ: t.clone(),
					name: Cow::Borrowed(n),
				})
			} else {
				dbg!(s, tokens);
				return Err(ParseError::BadStructFields(line!()));
			}
		}
		Ok(arguments)
	}
}

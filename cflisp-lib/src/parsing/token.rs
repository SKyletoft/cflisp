use super::*;
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
	Ternary(Vec<Token<'a>>),
	TypeDef,
	UintCast,
	Union,
	ArrayAccess(Vec<Token<'a>>),
	Block(Vec<Token<'a>>),
	Parentheses(Vec<Token<'a>>),
	Source(Vec<Token<'a>>),
	Unsigned,
	Volatile,
	While,
	Xor,
}

impl<'a> Token<'a> {
	///Splits a string into a list of tokens by matching pattern by pattern instead of
	/// splitting and then converting into tokens
	pub fn by_byte(source: &'a str) -> Result<Vec<Token<'a>>> {
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
}

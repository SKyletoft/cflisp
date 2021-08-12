use super::*;
use crate::*;

///All possible tokens in the source (after comments have been removed)
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
	Add,
	AddAssign,
	AdrOf,
	AlignAs,
	AlignOf,
	AnyCast,
	Assign,
	Atomic,
	Auto,
	BitAnd,
	BitAndAssign,
	BitNot,
	BitOr,
	BitOrAssign,
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
	Decrement,
	Default,
	Deref,
	Div,
	DivAssign,
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
	Increment,
	Inline,
	IntCast,
	LessThan,
	LessThanEqual,
	Long,
	LShift,
	LShiftAssign,
	Mod,
	ModAssign,
	Mul,
	MulAssign,
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
	RShiftAssign,
	Short,
	Signed,
	SizeOf,
	Static,
	StaticAssert,
	StringLiteral(&'a str),
	Struct,
	Sub,
	SubAssign,
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
	XorAssign,
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

	///Matches against the different assignment types
	///
	/// `=` `+=` `-=` `*=` `/=` `%=` `&=` `|=` `^=` `<<=` `>>=`
	pub(crate) fn is_assign(&self) -> bool {
		matches!(
			self,
			&Token::Assign
				| &Token::AddAssign
				| &Token::SubAssign
				| &Token::MulAssign
				| &Token::DivAssign
				| &Token::BitAndAssign
				| &Token::BitOrAssign
				| &Token::XorAssign
				| &Token::LShiftAssign
				| &Token::RShiftAssign
				| &Token::ModAssign
		)
	}
}

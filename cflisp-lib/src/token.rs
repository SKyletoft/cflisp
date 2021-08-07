use std::fmt;

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
	pub(crate) fn by_byte(source: &'a str) -> Result<Vec<Token<'a>>> {
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

impl fmt::Display for Token<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use Token::*;
		match self {
			Add => write!(f, "+"),
			AdrOf => write!(f, "&",),
			AlignAs => write!(f, "_Alignas",),
			AlignOf => write!(f, "_Alignof",),
			AnyCast => write!(f, "(any)",),
			Assign => write!(f, "=",),
			Atomic => write!(f, "_Atomic",),
			Auto => write!(f, "auto",),
			BitAnd => write!(f, "&",),
			BitNot => write!(f, "~",),
			BitOr => write!(f, "|",),
			Bool(b) => write!(f, "{}", b),
			BoolAnd => write!(f, "&&",),
			BoolCast => write!(f, "(bool)",),
			BoolNot => write!(f, "!"),
			BoolOr => write!(f, "||"),
			Break => write!(f, "break"),
			Case => write!(f, "case"),
			Char(c) => write!(f, "{}", c),
			CharCast => write!(f, "(char)"),
			Cmp => write!(f, "=="),
			Colon => write!(f, ":"),
			Comma => write!(f, ","),
			Complex => write!(f, "_Complex"),
			Const => write!(f, "const"),
			Continue => write!(f, "continue"),
			Decl(t) => write!(f, "{}", t),
			Default => write!(f, "default"),
			Deref => write!(f, "*"),
			Div => write!(f, "/"),
			Do => write!(f, "do"),
			Double => write!(f, "double"),
			Else => write!(f, "else"),
			Enum => write!(f, "enum"),
			Extern => write!(f, "extern"),
			FieldAccess => write!(f, "."),
			FieldPointerAccess => write!(f, "->"),
			Float => write!(f, "float"),
			For => write!(f, "for"),
			Generic => write!(f, "_Generic"),
			Goto => write!(f, "goto"),
			GreaterThan => write!(f, ">"),
			GreaterThanEqual => write!(f, ">="),
			If => write!(f, "if"),
			Imaginary => write!(f, "_Imaginary"),
			Inline => write!(f, "inline"),
			IntCast => write!(f, "(int)"),
			LessThan => write!(f, "<"),
			LessThanEqual => write!(f, "<="),
			Long => write!(f, "long"),
			LShift => write!(f, "<<"),
			Mod => write!(f, "%"),
			Mul => write!(f, "*"),
			Name(n) => write!(f, "{}", n),
			Namespace => write!(f, "namespace"),
			NamespaceSplitter => write!(f, "::"),
			NewLine => write!(f, ";"),
			NoReturn => write!(f, "_Noreturn"),
			NotCmp => write!(f, "!="),
			Num(n) => write!(f, "{}", n),
			Register => write!(f, "register"),
			Restrict => write!(f, "restrict"),
			Return => write!(f, "return"),
			RShift => write!(f, ">>"),
			Short => write!(f, "short"),
			Signed => write!(f, "signed"),
			SizeOf => write!(f, "sizeof"),
			Static => write!(f, "static"),
			StaticAssert => write!(f, "static_assert"),
			StringLiteral(s) => write!(f, "\"{}\"", s),
			Struct => write!(f, "struct"),
			Sub => write!(f, "-"),
			Switch => write!(f, "switch"),
			ThreadLocal => write!(f, "Thread_local"),
			TypeDef => write!(f, "typedef"),
			UintCast => write!(f, "(uint)"),
			Union => write!(f, "union"),
			Unsigned => write!(f, "unsigned"),
			Volatile => write!(f, "volatile"),
			While => write!(f, "while"),
			Xor => write!(f, "^"),
			Parentheses(block) => {
				write!(f, "(")?;
				helper::write_token_slice(block, f, " ")?;
				write!(f, ")")
			}
			Source(block) | Block(block) => {
				write!(f, "{{")?;
				helper::write_token_slice(block, f, " ")?;
				write!(f, "}}")
			}
			ArrayAccess(block) => {
				write!(f, "[")?;
				helper::write_token_slice(block, f, " ")?;
				write!(f, "]")
			}
			Ternary(block) => {
				write!(f, "? ")?;
				helper::write_token_slice(block, f, " ")?;
				write!(f, " :")
			}
		}
	}
}

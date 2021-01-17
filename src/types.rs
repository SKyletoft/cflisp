use crate::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) struct Variable<'a> {
	pub(crate) typ: Type,
	pub(crate) name: &'a str,
}
///A statement is a list of tokens. It must evaluate to a value when processed
pub(crate) type Statement<'a> = Vec<StatementToken<'a>>;
///A block is a list of valid LanguageElements
pub(crate) type Block<'a> = Vec<LanguageElement<'a>>;

///The types that are currently supported by the compiler and their pointer types
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum Type {
	Uint,
	Int,
	Char,
	Bool,
	UintPtr,
	IntPtr,
	CharPtr,
	BoolPtr,
	Void,
	VoidPtr,
}

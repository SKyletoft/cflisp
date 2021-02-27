use std::borrow::Cow;

use crate::*;

///A type and a name
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Variable<'a> {
	pub(crate) typ: Type,
	pub(crate) name: Cow<'a, str>,
}

///Return type, name and argument list
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Function<'a> {
	pub(crate) return_type: Type,
	pub(crate) name: Cow<'a, str>,
	pub(crate) parametres: Vec<Variable<'a>>,
}
///A statement is a list of tokens. It must evaluate to a value when processed
///
///`Vec<StatementToken<'a>>`
pub(crate) type Statement<'a> = Vec<StatementToken<'a>>;
///A block is a list of valid LanguageElements
///
///`Vec<LanguageElement<'a>>`
pub(crate) type Block<'a> = Vec<LanguageElement<'a>>;

///The types that are currently supported by the compiler and their pointer types
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Type {
	Uint,
	Int,
	Char,
	Bool,
	Void,
	Ptr(Box<Type>),
}

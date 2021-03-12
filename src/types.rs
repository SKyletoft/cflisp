use crate::*;
use std::borrow::Cow;

///A type and a name
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Variable<'a> {
	pub(crate) typ: Type,
	pub(crate) name: &'a str,
}

///Return type, name and argument list
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Function<'a> {
	pub(crate) return_type: Type,
	pub(crate) name: &'a str,
	pub(crate) parametres: Vec<Variable<'a>>,
}

///A struct definition is a name for the type and a list of member variables
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Struct<'a> {
	pub(crate) name: Cow<'a, str>,
	pub(crate) members: Vec<Variable<'a>>,
}

///A statement is a list of tokens. It must evaluate to a value when processed
pub(crate) type Statement<'a> = Vec<StatementToken<'a>>;
///A block is a list of valid LanguageElements
pub(crate) type Block<'a> = Vec<LanguageElement<'a>>;
///A block is a list of valid LanguageElementsStructless
pub(crate) type BlockStructless<'a> = Vec<LanguageElementStructless<'a>>;

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

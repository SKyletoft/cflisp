use crate::*;
use std::borrow::Cow;

///A type and a name
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Variable<'a> {
	pub(crate) typ: NativeType,
	pub(crate) name: &'a str,
}

///Return type, name and argument list
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Function<'a> {
	pub(crate) return_type: NativeType,
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

///The types that are currently supported by the compiler and their pointer types.
/// Can also hold the name of a struct
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Type<'a> {
	Uint,
	Int,
	Char,
	Bool,
	Void,
	Struct(&'a str),
	Ptr(Box<Type<'a>>),
}

///The types that are currently supported by the compiler and their pointer types.
/// No structs
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum NativeType {
	Uint,
	Int,
	Char,
	Bool,
	Void,
	Ptr(Box<NativeType>),
}
impl<'a> From<Type<'a>> for NativeType {
	fn from(other: Type) -> Self {
		(&other).into()
	}
}

impl<'a> From<&Type<'a>> for NativeType {
	fn from(other: &Type) -> Self {
		match other {
			Type::Uint => NativeType::Uint,
			Type::Int => NativeType::Int,
			Type::Char => NativeType::Char,
			Type::Bool => NativeType::Bool,
			Type::Void => NativeType::Void,
			Type::Struct(_) => NativeType::Void,
			Type::Ptr(target) => NativeType::Ptr(Box::new(target.as_ref().into())),
		}
	}
}

impl<'a> From<&NativeType> for Type<'a> {
	fn from(other: &NativeType) -> Self {
		match other {
			NativeType::Uint => Type::Uint,
			NativeType::Int => Type::Int,
			NativeType::Char => Type::Char,
			NativeType::Bool => Type::Bool,
			NativeType::Void => Type::Void,
			NativeType::Ptr(target) => Type::Ptr(Box::new((target.as_ref()).into())),
		}
	}
}

impl<'a> Type<'a> {
	pub(crate) fn get_struct_type(&self) -> Option<&'a str> {
		match self {
			Type::Uint | Type::Int | Type::Char | Type::Bool | Type::Void => None,
			Type::Struct(n) => Some(n),
			Type::Ptr(inner) => inner.get_struct_type(),
		}
	}
}

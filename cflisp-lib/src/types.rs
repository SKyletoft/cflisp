use std::{borrow::Cow, collections::HashMap};

use crate::*;

///A type and a name
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable<'a> {
	pub typ: Type<'a>,
	pub name: &'a str,
}

impl<'a> Variable<'a> {
	pub(crate) fn split_into_native(
		self,
		struct_defs: &HashMap<Cow<'a, str>, Vec<Variable<'a>>>,
	) -> Result<Vec<NativeVariable<'a>>, ParseError> {
		let res = if let Type::Struct(struct_type) = self.typ {
			let mut vec = vec![NativeVariable {
				name: Cow::Borrowed(self.name),
				typ: NativeType::Void,
			}];
			struct_defs
				.get(struct_type)
				.ok_or(ParseError(line!(), "Undefined struct type"))?
				.iter()
				.map(
					|Variable {
					     typ: var_typ,
					     name: var_name,
					 }| NativeVariable {
						typ: var_typ.into(),
						name: helper::merge_name_and_field(self.name, var_name),
					},
				)
				.for_each(|field| vec.push(field));
			vec
		} else {
			vec![NativeVariable {
				typ: self.typ.into(),
				name: Cow::Borrowed(self.name),
			}]
		};
		Ok(res)
	}
}

///A type and a name
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NativeVariable<'a> {
	pub typ: NativeType,
	pub name: Cow<'a, str>,
}

///Return type, name and argument list
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function<'a> {
	pub return_type: NativeType,
	pub name: &'a str,
	pub parametres: Vec<Variable<'a>>,
}

///A struct definition is a name for the type and a list of member variables
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Struct<'a> {
	pub name: Cow<'a, str>,
	pub members: Vec<Variable<'a>>,
}

///A statement is a list of tokens. It must evaluate to a value when processed
pub type Statement<'a> = Vec<StatementToken<'a>>;
///A block is a list of valid LanguageElements
pub type Block<'a> = Vec<LanguageElement<'a>>;
///A block is a list of valid LanguageElementsStructless
pub type BlockStructless<'a> = Vec<StructlessLanguage<'a>>;

///The types that are currently supported by the compiler and their pointer types.
/// Can also hold the name of a struct
#[derive(Debug, Clone, Hash)]
pub enum Type<'a> {
	Uint,
	Int,
	Char,
	Bool,
	Void,
	Struct(&'a str),
	Ptr(Box<Type<'a>>),
	Arr(Box<Type<'a>>, isize),
}

impl PartialEq for Type<'_> {
	fn eq(&self, other: &Self) -> bool {
		match self {
			Type::Ptr(_) | Type::Uint | Type::Int => {
				matches!(other, Type::Ptr(_) | Type::Uint | Type::Int)
			}
			Type::Char => matches!(other, Type::Char),
			Type::Bool => matches!(other, Type::Bool),
			Type::Void => matches!(other, Type::Void),
			Type::Struct(s) => matches!(other, Type::Struct(ss) if s == ss),
			Type::Arr(a, _) => {
				matches!(other, Type::Ptr(_)) || matches!(other, Type::Arr(b, _) if a == b)
			}
		}
	}
}

impl Eq for Type<'_> {}

impl<'a> Type<'a> {
	pub(crate) fn ptr(target: Type<'a>) -> Type<'a> {
		Type::Ptr(Box::new(target))
	}
}

///The types that are currently supported by the compiler and their pointer types.
/// No structs
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NativeType {
	Uint,
	Int,
	Char,
	Bool,
	Void,
	Ptr(Box<NativeType>),
}

impl NativeType {
	pub(crate) fn ptr(target: NativeType) -> NativeType {
		NativeType::Ptr(Box::new(target))
	}
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
			Type::Ptr(target) => NativeType::ptr(target.as_ref().into()),
			Type::Arr(target, _) => NativeType::ptr(target.as_ref().into()),
		}
	}
}

impl<'a> From<NativeType> for Type<'a> {
	fn from(other: NativeType) -> Self {
		(&other).into()
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
			NativeType::Ptr(target) => Type::ptr((target.as_ref()).into()),
		}
	}
}

impl<'a> Type<'a> {
	///Get struct type name, even when nested in a pointer or array
	pub(crate) fn get_struct_type(&self) -> Option<&'a str> {
		match self {
			Type::Uint | Type::Int | Type::Char | Type::Bool | Type::Void => None,
			Type::Struct(n) => Some(n),
			Type::Ptr(inner) | Type::Arr(inner, _) => inner.get_struct_type(),
		}
	}
}

impl<'a> PartialEq<Type<'a>> for NativeType {
	fn eq(&self, other: &Type<'a>) -> bool {
		match other {
			Type::Uint => self == &NativeType::Uint,
			Type::Int => self == &NativeType::Int,
			Type::Char => self == &NativeType::Char,
			Type::Bool => self == &NativeType::Bool,
			Type::Void => self == &NativeType::Void,
			Type::Struct(_) => false,
			Type::Ptr(inner) => {
				if let NativeType::Ptr(self_inner) = self {
					*self_inner.as_ref() == *inner.as_ref()
				} else {
					false
				}
			}
			Type::Arr(inner, _) => {
				if let NativeType::Ptr(self_inner) = self {
					*self_inner.as_ref() == *inner.as_ref()
				} else {
					false
				}
			}
		}
	}
}

impl<'a> PartialEq<NativeType> for Type<'a> {
	fn eq(&self, other: &NativeType) -> bool {
		other.eq(self)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NumberType {
	Signed,
	Unsigned,
	Unknown,
}

struct Number {
	val: isize,
	signedness: NumberType,
}

impl From<isize> for NumberType {
	fn from(n: isize) -> Self {
		if n > i8::MAX as isize {
			NumberType::Unsigned
		} else if n < 0 {
			NumberType::Signed
		} else {
			NumberType::Unknown
		}
	}
}

impl NumberType {
	pub fn promote(self, other: Self) -> Self {
		if self == other {
			return self;
		}
		if self == NumberType::Unknown {
			return other;
		}
		if other == NumberType::Unknown {
			return self;
		}
		if self != other {
			return NumberType::Signed;
		}
		unreachable!()
	}
}

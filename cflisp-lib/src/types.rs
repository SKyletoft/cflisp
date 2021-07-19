use std::{borrow::Cow, cmp, collections::HashMap, default::Default, fmt, ops};

use crate::*;

///A type and a name
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable<'a> {
	pub typ: Type<'a>,
	pub name: &'a str,
}

impl<'a> Variable<'a> {
	///Get the inner field list with each member prepended with the struct type name, e.g. `[Foo::A, Foo::B, Foo::C]`
	pub(crate) fn split_into_native(
		self,
		struct_defs: &HashMap<Cow<'a, str>, Vec<NativeVariable<'a>>>,
	) -> Result<Vec<NativeVariable<'a>>, ParseError> {
		let res = if let Type::Struct(struct_type) = self.typ {
			let mut vec = struct_defs
				.get(struct_type)
				.ok_or(ParseError(line!(), "Undefined struct type"))?
				.clone();
			for NativeVariable { name, .. } in vec.iter_mut() {
				*name = helper::merge_name_and_field(self.name, name);
			}
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
pub enum NumberType {
	Signed,
	Unsigned,
	Unknown,
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

impl From<&NativeType> for NumberType {
	fn from(typ: &NativeType) -> Self {
		match typ {
			NativeType::Uint => NumberType::Unsigned,
			NativeType::Int => NumberType::Signed,
			NativeType::Char => NumberType::CHAR_SIGNEDNESS,
			NativeType::Bool => NumberType::BOOL_SIGNEDNESS,
			NativeType::Void => NumberType::Unknown,
			NativeType::Ptr(_) => NumberType::PTR_SIGNEDNESS,
		}
	}
}

impl Default for NumberType {
	fn default() -> Self {
		NumberType::Unknown
	}
}

impl NumberType {
	pub const BOOL_SIGNEDNESS: Self = NumberType::Unsigned;
	pub const CHAR_SIGNEDNESS: Self = NumberType::Unknown;
	pub const PTR_SIGNEDNESS: Self = NumberType::Unsigned;

	pub(crate) fn promote(self, other: Self) -> Self {
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

	fn make_in_range(&self, n: isize) -> isize {
		match self {
			NumberType::Signed => n % (i8::MAX as isize + 1),
			NumberType::Unsigned => (n + u8::MAX as isize + 1) % u8::MAX as isize,
			NumberType::Unknown => {
				assert!(n >= 0 && n <= i8::MAX as isize + 1, "{}", n);
				n
			}
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Number {
	pub val: isize,
	pub signedness: NumberType,
}

impl From<isize> for Number {
	fn from(n: isize) -> Self {
		Self {
			val: n,
			signedness: n.into(),
		}
	}
}

impl ops::Add for Number {
	type Output = Self;

	fn add(self, rhs: Self) -> Self::Output {
		let signedness = self.signedness.promote(rhs.signedness);
		let raw_val = self.val + rhs.val;
		let val = signedness.make_in_range(raw_val);
		Self { val, signedness }
	}
}

impl ops::Sub for Number {
	type Output = Self;

	fn sub(self, rhs: Self) -> Self::Output {
		let signedness = self.signedness.promote(rhs.signedness);
		let raw_val = self.val - rhs.val;
		let val = signedness.make_in_range(raw_val);
		Self { val, signedness }
	}
}

impl ops::Mul for Number {
	type Output = Self;

	fn mul(self, rhs: Self) -> Self::Output {
		let signedness = self.signedness.promote(rhs.signedness);
		let raw_val = self.val * rhs.val;
		let val = signedness.make_in_range(raw_val);
		Self { val, signedness }
	}
}

impl ops::Div for Number {
	type Output = Self;

	fn div(self, rhs: Self) -> Self::Output {
		let signedness = self.signedness.promote(rhs.signedness);
		let raw_val = self.val / rhs.val;
		let val = signedness.make_in_range(raw_val);
		Self { val, signedness }
	}
}

impl ops::Rem for Number {
	type Output = Self;

	fn rem(self, rhs: Self) -> Self::Output {
		let signedness = self.signedness.promote(rhs.signedness);
		let raw_val = self.val % rhs.val;
		let val = signedness.make_in_range(raw_val);
		Self { val, signedness }
	}
}

impl ops::Shl for Number {
	type Output = Self;

	fn shl(self, rhs: Self) -> Self::Output {
		let val = self.val << rhs.val;
		Self {
			val,
			signedness: self.signedness,
		}
	}
}

impl ops::Shr for Number {
	type Output = Self;

	fn shr(self, rhs: Self) -> Self::Output {
		let val = self.val >> rhs.val;
		Self {
			val,
			signedness: self.signedness,
		}
	}
}

impl ops::BitAnd for Number {
	type Output = Self;

	fn bitand(self, rhs: Self) -> Self::Output {
		let val = self.val & rhs.val;
		Self {
			val,
			signedness: self.signedness,
		}
	}
}

impl ops::BitOr for Number {
	type Output = Self;

	fn bitor(self, rhs: Self) -> Self::Output {
		let val = self.val | rhs.val;
		Self {
			val,
			signedness: self.signedness,
		}
	}
}

impl ops::BitXor for Number {
	type Output = Self;

	fn bitxor(self, rhs: Self) -> Self::Output {
		let val = self.val ^ rhs.val;
		Self {
			val,
			signedness: self.signedness,
		}
	}
}

impl ops::Not for Number {
	type Output = Self;

	fn not(self) -> Self::Output {
		Number {
			val: !self.val,
			signedness: self.signedness,
		}
	}
}

impl cmp::PartialOrd for Number {
	fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
		self.val.partial_cmp(&other.val)
	}
}

impl fmt::Display for Number {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.val)
	}
}

impl Number {
	pub const ZERO: Self = Number {
		val: 0,
		signedness: NumberType::Unknown,
	};

	pub fn new(val: isize, signedness: NumberType) -> Self {
		Self { val, signedness }
	}
}

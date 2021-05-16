use std::{borrow::Cow, collections::HashMap};

use crate::*;

///Tree structure to represent a statement. Boolean and bitwise logic are combined
#[derive(Debug, Clone, PartialEq)]
pub enum StructlessStatement<'a> {
	Add {
		lhs: Box<StructlessStatement<'a>>,
		rhs: Box<StructlessStatement<'a>>,
	},
	Sub {
		lhs: Box<StructlessStatement<'a>>,
		rhs: Box<StructlessStatement<'a>>,
	},
	Mul {
		lhs: Box<StructlessStatement<'a>>,
		rhs: Box<StructlessStatement<'a>>,
	},
	Div {
		lhs: Box<StructlessStatement<'a>>,
		rhs: Box<StructlessStatement<'a>>,
	},
	Mod {
		lhs: Box<StructlessStatement<'a>>,
		rhs: Box<StructlessStatement<'a>>,
	},
	LShift {
		lhs: Box<StructlessStatement<'a>>,
		rhs: Box<StructlessStatement<'a>>,
	},
	RShift {
		lhs: Box<StructlessStatement<'a>>,
		rhs: Box<StructlessStatement<'a>>,
	},
	And {
		lhs: Box<StructlessStatement<'a>>,
		rhs: Box<StructlessStatement<'a>>,
	},
	Or {
		lhs: Box<StructlessStatement<'a>>,
		rhs: Box<StructlessStatement<'a>>,
	},
	Xor {
		lhs: Box<StructlessStatement<'a>>,
		rhs: Box<StructlessStatement<'a>>,
	},
	GreaterThan {
		lhs: Box<StructlessStatement<'a>>,
		rhs: Box<StructlessStatement<'a>>,
	},
	LessThan {
		lhs: Box<StructlessStatement<'a>>,
		rhs: Box<StructlessStatement<'a>>,
	},
	GreaterThanEqual {
		lhs: Box<StructlessStatement<'a>>,
		rhs: Box<StructlessStatement<'a>>,
	},
	LessThanEqual {
		lhs: Box<StructlessStatement<'a>>,
		rhs: Box<StructlessStatement<'a>>,
	},
	Cmp {
		lhs: Box<StructlessStatement<'a>>,
		rhs: Box<StructlessStatement<'a>>,
	},
	NotCmp {
		lhs: Box<StructlessStatement<'a>>,
		rhs: Box<StructlessStatement<'a>>,
	},
	FunctionCall {
		name: Cow<'a, str>,
		parametres: Vec<StructlessStatement<'a>>,
	},
	Not(Box<StructlessStatement<'a>>),
	Var(Cow<'a, str>),
	Num(isize),
	Char(char),
	Bool(bool),
	Array(Vec<StructlessStatement<'a>>),
	Deref(Box<StructlessStatement<'a>>),
	AdrOf(Cow<'a, str>),
}

impl<'a> StructlessStatement<'a> {
	pub(crate) fn from(
		other: &StatementElement<'a>,
		struct_types: &HashMap<Cow<'a, str>, Vec<Variable<'a>>>,
		structs_and_struct_pointers: &HashMap<Cow<'a, str>, &'a str>,
		functions: &HashMap<Cow<'a, str>, Vec<Variable<'a>>>,
	) -> Result<Self, ParseError> {
		macro_rules! bin_op {
			($i:ident, $lhs:expr, $rhs:expr) => {
				StructlessStatement::$i {
					lhs: Box::new(StructlessStatement::from(
						$lhs.as_ref(),
						struct_types,
						structs_and_struct_pointers,
						functions,
					)?),
					rhs: Box::new(StructlessStatement::from(
						$rhs.as_ref(),
						struct_types,
						structs_and_struct_pointers,
						functions,
					)?),
				}
			};
		}
		macro_rules! un_op {
			($i:ident, $lhs:expr) => {
				StructlessStatement::$i(Box::new(StructlessStatement::from(
					$lhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
					functions,
				)?))
			};
		}

		let res = match other {
			StatementElement::Add { lhs, rhs } => bin_op!(Add, lhs, rhs),
			StatementElement::Sub { lhs, rhs } => bin_op!(Sub, lhs, rhs),
			StatementElement::Mul { lhs, rhs } => bin_op!(Mul, lhs, rhs),
			StatementElement::Div { lhs, rhs } => bin_op!(Div, lhs, rhs),
			StatementElement::Mod { lhs, rhs } => bin_op!(Mod, lhs, rhs),
			StatementElement::LShift { lhs, rhs } => bin_op!(LShift, lhs, rhs),
			StatementElement::RShift { lhs, rhs } => bin_op!(RShift, lhs, rhs),
			StatementElement::Xor { lhs, rhs } => bin_op!(Xor, lhs, rhs),
			StatementElement::GreaterThan { lhs, rhs } => bin_op!(GreaterThan, lhs, rhs),
			StatementElement::LessThan { lhs, rhs } => bin_op!(LessThan, lhs, rhs),
			StatementElement::GreaterThanEqual { lhs, rhs } => bin_op!(GreaterThanEqual, lhs, rhs),
			StatementElement::LessThanEqual { lhs, rhs } => bin_op!(LessThanEqual, lhs, rhs),
			StatementElement::Cmp { lhs, rhs } => bin_op!(Cmp, lhs, rhs),
			StatementElement::NotCmp { lhs, rhs } => bin_op!(NotCmp, lhs, rhs),
			StatementElement::BoolAnd { lhs, rhs } | StatementElement::BitAnd { lhs, rhs } => {
				bin_op!(And, lhs, rhs)
			}
			StatementElement::BoolOr { lhs, rhs } | StatementElement::BitOr { lhs, rhs } => {
				bin_op!(Or, lhs, rhs)
			}
			StatementElement::BoolNot(lhs) | StatementElement::BitNot(lhs) => {
				un_op!(Not, lhs)
			}

			StatementElement::FunctionCall { name, parametres } => {
				let mut new_parametres = Vec::new();
				let arguments = functions
					.get(name)
					.ok_or(ParseError(line!(), "Undefined function"))?;
				for (arg, param) in arguments.iter().zip(parametres.iter()) {
					if let Type::Struct(struct_type) = arg.typ {
						let fields = struct_types
							.get(struct_type)
							.ok_or(ParseError(line!(), "Undefined struct type"))?;
						let var_name = if let StatementElement::Var(var_name) = param {
							Ok(var_name)
						} else {
							Err(ParseError(
								line!(),
								"Only struct variables can be passed into functions with struct \
								 arguments (not literals)",
							))
						}?;
						for field in fields.iter() {
							let param_name = helper::merge_name_and_field(var_name, field.name);
							new_parametres.push(StructlessStatement::Var(param_name))
						}
					} else {
						new_parametres.push(StructlessStatement::from(
							param,
							struct_types,
							structs_and_struct_pointers,
							functions,
						)?);
					}
				}
				StructlessStatement::FunctionCall {
					name: name.clone(),
					parametres: new_parametres,
				}
			}
			StatementElement::Var(v) => StructlessStatement::Var(v.clone()),
			StatementElement::Num(n) => StructlessStatement::Num(*n),
			StatementElement::Char(c) => StructlessStatement::Char(*c),
			StatementElement::Bool(b) => StructlessStatement::Bool(*b),
			StatementElement::Array(arr) => StructlessStatement::Array(
				arr.iter()
					.map(|parametre| {
						StructlessStatement::from(
							parametre,
							struct_types,
							structs_and_struct_pointers,
							functions,
						)
					})
					.collect::<Result<_, _>>()?,
			),
			StatementElement::Deref(n) => un_op!(Deref, n),
			StatementElement::AdrOf(n) => StructlessStatement::AdrOf(n.clone()),

			StatementElement::FieldPointerAccess(name, field) => {
				let struct_type = structs_and_struct_pointers.get(name).ok_or_else(|| {
					dbg!(name, structs_and_struct_pointers);
					ParseError(line!(), "Variable wasn't of struct or struct pointer type")
				})?;
				let fields = struct_types
					.get(*struct_type)
					.ok_or(ParseError(line!(), "Undefined struct type"))?;
				let idx = fields
					.iter()
					.position(|field_name| field_name.name == field)
					.ok_or(ParseError(line!(), "Unknown field type"))? as isize;
				StructlessStatement::Deref(Box::new(StructlessStatement::Add {
					lhs: Box::new(StructlessStatement::Num(idx)),
					rhs: Box::new(StructlessStatement::Var(name.clone())),
				}))
			}

			StatementElement::Cast {
				typ: NativeType::Bool,
				value,
			} => StructlessStatement::FunctionCall {
				name: Cow::Borrowed("__tb__"),
				parametres: vec![StructlessStatement::from(
					value,
					struct_types,
					structs_and_struct_pointers,
					functions,
				)?],
			},

			StatementElement::Cast {
				typ: NativeType::Char,
				value,
			}
			| StatementElement::Cast {
				typ: NativeType::Int,
				value,
			} => StructlessStatement::from(
				value,
				struct_types,
				structs_and_struct_pointers,
				functions,
			)?,

			StatementElement::Cast { .. } | StatementElement::Ternary { .. } => todo!(),
		};
		Ok(res)
	}
}

impl<'a> StructlessStatement<'a> {
	pub(crate) fn depth(&self) -> usize {
		let rest = match self {
			StructlessStatement::Add { lhs, rhs }
			| StructlessStatement::Sub { lhs, rhs }
			| StructlessStatement::Mul { lhs, rhs }
			| StructlessStatement::Div { lhs, rhs }
			| StructlessStatement::Mod { lhs, rhs }
			| StructlessStatement::LShift { lhs, rhs }
			| StructlessStatement::RShift { lhs, rhs }
			| StructlessStatement::And { lhs, rhs }
			| StructlessStatement::Or { lhs, rhs }
			| StructlessStatement::Xor { lhs, rhs }
			| StructlessStatement::GreaterThan { lhs, rhs }
			| StructlessStatement::LessThan { lhs, rhs }
			| StructlessStatement::GreaterThanEqual { lhs, rhs }
			| StructlessStatement::LessThanEqual { lhs, rhs }
			| StructlessStatement::Cmp { lhs, rhs }
			| StructlessStatement::NotCmp { lhs, rhs } => lhs.as_ref().depth().max(rhs.as_ref().depth()),
			StructlessStatement::Not(lhs) => lhs.as_ref().depth(),
			StructlessStatement::Array(n) => n.iter().map(|e| e.depth()).max().unwrap_or(0),
			StructlessStatement::Deref(n) => n.as_ref().depth(),
			StructlessStatement::Var(_)
			| StructlessStatement::Num(_)
			| StructlessStatement::Char(_)
			| StructlessStatement::Bool(_)
			| StructlessStatement::AdrOf(_) => 0,
			StructlessStatement::FunctionCall { .. } => 1, //Each parametre is its own memory alloc but can still require 1 if the function call is on the rhs
		};
		rest + 1
	}

	pub(crate) fn internal_ref(&self) -> Option<(&StructlessStatement, &StructlessStatement)> {
		match self {
			StructlessStatement::Add { lhs, rhs }
			| StructlessStatement::Sub { lhs, rhs }
			| StructlessStatement::Mul { lhs, rhs }
			| StructlessStatement::Div { lhs, rhs }
			| StructlessStatement::Mod { lhs, rhs }
			| StructlessStatement::LShift { lhs, rhs }
			| StructlessStatement::RShift { lhs, rhs }
			| StructlessStatement::And { lhs, rhs }
			| StructlessStatement::Or { lhs, rhs }
			| StructlessStatement::Xor { lhs, rhs }
			| StructlessStatement::GreaterThan { lhs, rhs }
			| StructlessStatement::LessThan { lhs, rhs }
			| StructlessStatement::GreaterThanEqual { lhs, rhs }
			| StructlessStatement::LessThanEqual { lhs, rhs }
			| StructlessStatement::NotCmp { lhs, rhs }
			| StructlessStatement::Cmp { lhs, rhs } => Some((lhs.as_ref(), rhs.as_ref())),
			StructlessStatement::Not(_)
			| StructlessStatement::FunctionCall { .. }
			| StructlessStatement::Var(_)
			| StructlessStatement::Num(_)
			| StructlessStatement::Char(_)
			| StructlessStatement::Bool(_)
			| StructlessStatement::Array(_)
			| StructlessStatement::Deref(_)
			| StructlessStatement::AdrOf(_) => None,
		}
	}
}

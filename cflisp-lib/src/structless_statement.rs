use std::borrow::Cow;

use crate::{structless_language::State, *};

///Tree structure to represent a statement. Boolean and bitwise logic are combined
#[derive(Debug, Clone, PartialEq)]
pub enum StructlessStatement<'a> {
	BinOp {
		op: BinOp,
		lhs: Box<StructlessStatement<'a>>,
		rhs: Box<StructlessStatement<'a>>,
		signedness: NumberType,
	},
	FunctionCall {
		name: Cow<'a, str>,
		parametres: Vec<StructlessStatement<'a>>,
	},
	Not(Box<StructlessStatement<'a>>),
	Var(Cow<'a, str>),
	Num(Number),
	Char(char),
	Bool(bool),
	Array(Vec<StructlessStatement<'a>>),
	Deref(Box<StructlessStatement<'a>>),
	AdrOf(Cow<'a, str>),
}

impl<'a> StructlessStatement<'a> {
	pub(crate) fn from(
		other: &StatementElement<'a>,
		state: &State<'a, '_, '_, '_, '_>,
	) -> Result<Self, ParseError> {
		let bin_op = |op, lhs, rhs| {
			Ok(StructlessStatement::BinOp {
				op,
				lhs: Box::new(StructlessStatement::from(lhs, state)?),
				rhs: Box::new(StructlessStatement::from(rhs, state)?),
				signedness: lhs
					.signedness(state.symbols)?
					.promote(rhs.signedness(state.symbols)?),
			})
		};
		macro_rules! un_op {
			($i: ident, $lhs: expr) => {
				StructlessStatement::$i(Box::new(StructlessStatement::from($lhs.as_ref(), state)?))
			};
		}

		let res = match other {
			StatementElement::Add { lhs, rhs } => bin_op(BinOp::Add, lhs, rhs)?,
			StatementElement::Sub { lhs, rhs } => bin_op(BinOp::Sub, lhs, rhs)?,
			StatementElement::Mul { lhs, rhs } => bin_op(BinOp::Mul, lhs, rhs)?,
			StatementElement::Div { lhs, rhs } => bin_op(BinOp::Div, lhs, rhs)?,
			StatementElement::Mod { lhs, rhs } => bin_op(BinOp::Mod, lhs, rhs)?,
			StatementElement::LShift { lhs, rhs } => bin_op(BinOp::LShift, lhs, rhs)?,
			StatementElement::RShift { lhs, rhs } => bin_op(BinOp::RShift, lhs, rhs)?,
			StatementElement::Xor { lhs, rhs } => bin_op(BinOp::Xor, lhs, rhs)?,
			StatementElement::GreaterThan { lhs, rhs } => bin_op(BinOp::GreaterThan, lhs, rhs)?,
			StatementElement::LessThan { lhs, rhs } => bin_op(BinOp::LessThan, lhs, rhs)?,
			StatementElement::GreaterThanEqual { lhs, rhs } => {
				bin_op(BinOp::GreaterThanEqual, lhs, rhs)?
			}
			StatementElement::LessThanEqual { lhs, rhs } => bin_op(BinOp::LessThanEqual, lhs, rhs)?,
			StatementElement::Cmp { lhs, rhs } => bin_op(BinOp::Cmp, lhs, rhs)?,
			StatementElement::NotCmp { lhs, rhs } => bin_op(BinOp::NotCmp, lhs, rhs)?,
			StatementElement::BoolAnd { lhs, rhs } | StatementElement::BitAnd { lhs, rhs } => {
				bin_op(BinOp::And, lhs, rhs)?
			}
			StatementElement::BoolOr { lhs, rhs } | StatementElement::BitOr { lhs, rhs } => {
				bin_op(BinOp::Or, lhs, rhs)?
			}
			StatementElement::BoolNot(lhs) | StatementElement::BitNot(lhs) => {
				un_op!(Not, lhs)
			}

			StatementElement::FunctionCall { name, parametres } => {
				let mut new_parametres = Vec::new();
				let arguments = state
					.functions
					.get(name)
					.ok_or(ParseError::UndefinedFunction(line!()))?;
				for (arg, param) in arguments.iter().zip(parametres.iter()) {
					if let Type::Struct(struct_type) = arg.typ {
						let fields = state
							.struct_types
							.get(struct_type)
							.ok_or(ParseError::UndefinedType(line!()))?;
						let var_name = if let StatementElement::Var(var_name) = param {
							Ok(var_name)
						} else {
							Err(ParseError::IllegalStructLiteral(line!()))
						}?;
						for field in fields.iter() {
							let param_name = helper::merge_name_and_field(var_name, &field.name);
							new_parametres.push(StructlessStatement::Var(param_name))
						}
					} else {
						new_parametres.push(StructlessStatement::from(param, state)?);
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
					.map(|parametre| StructlessStatement::from(parametre, state))
					.collect::<Result<_, _>>()?,
			),
			StatementElement::Deref(n) => un_op!(Deref, n),
			StatementElement::AdrOf(n) => StructlessStatement::AdrOf(n.clone()),

			StatementElement::FieldPointerAccess(name, field) => {
				let struct_type = state.structs_and_struct_pointers.get(name).ok_or_else(|| {
					dbg!(name, &state.structs_and_struct_pointers);
					ParseError::WrongTypeWasNative(line!())
				})?;
				let fields = state
					.struct_types
					.get(*struct_type)
					.ok_or(ParseError::UndefinedType(line!()))?;
				let idx = fields
					.iter()
					.position(|field_name| &field_name.name == field)
					.ok_or(ParseError::UndefinedStructField(line!()))?;
				let signedness = NumberType::from(&fields[idx].typ);
				StructlessStatement::Deref(Box::new(StructlessStatement::BinOp {
					op: BinOp::Add,
					lhs: Box::new(StructlessStatement::Num((idx as isize).into())),
					rhs: Box::new(StructlessStatement::Var(name.clone())),
					signedness,
				}))
			}

			StatementElement::Cast {
				typ: NativeType::Bool,
				value,
			} => StructlessStatement::FunctionCall {
				name: Cow::Borrowed("__tb__"),
				parametres: vec![StructlessStatement::from(value, state)?],
			},

			StatementElement::Cast {
				typ: NativeType::Char,
				value,
			}
			| StatementElement::Cast {
				typ: NativeType::Int,
				value,
			} => StructlessStatement::from(value, state)?,

			StatementElement::Cast { .. } | StatementElement::Ternary { .. } => todo!(),
		};
		Ok(res)
	}

	pub(crate) fn depth(&self) -> usize {
		let rest = match self {
			StructlessStatement::BinOp { lhs, rhs, .. } => {
				lhs.as_ref().depth().max(rhs.as_ref().depth())
			}
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
			StructlessStatement::BinOp { lhs, rhs, .. } => Some((lhs.as_ref(), rhs.as_ref())),
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

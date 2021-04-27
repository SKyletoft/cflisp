use crate::*;
use std::{borrow::Cow, collections::HashMap};

///Simplified internal representation of a program.
/// Doesn't contain structs or for loops
#[derive(Debug, Clone, PartialEq)]
pub enum LanguageElementStructless<'a> {
	VariableDeclaration {
		typ: NativeType,
		name: Cow<'a, str>,
		is_static: bool,
		is_const: bool,
		is_volatile: bool,
	},
	VariableAssignment {
		name: Cow<'a, str>,
		value: StatementElementStructless<'a>,
	},
	VariableDeclarationAssignment {
		typ: NativeType,
		name: Cow<'a, str>,
		value: StatementElementStructless<'a>,
		is_static: bool,
		is_const: bool,
		is_volatile: bool,
	},
	PointerAssignment {
		ptr: StatementElementStructless<'a>,
		value: StatementElementStructless<'a>,
	},
	FunctionDeclaration {
		typ: NativeType,
		name: Cow<'a, str>,
		args: Vec<NativeVariable<'a>>,
		block: BlockStructless<'a>,
	},
	IfStatement {
		condition: StatementElementStructless<'a>,
		then: Box<LanguageElementStructless<'a>>,
		else_then: Option<Box<LanguageElementStructless<'a>>>,
	},
	Loop {
		condition: StatementElementStructless<'a>,
		body: Box<LanguageElementStructless<'a>>,
	},
	Return(Option<StatementElementStructless<'a>>),
	Statement(StatementElementStructless<'a>),
	StructDeclaration {
		name: Cow<'a, str>,
		is_static: bool,
		is_const: bool,
		is_volatile: bool,
	},
	Block {
		block: BlockStructless<'a>,
		scope_name: Cow<'a, str>,
	},
}

impl<'a> LanguageElementStructless<'a> {
	///Destructures structs into normal language elements
	pub fn from_language_elements(
		elements: Vec<LanguageElement<'a>>,
	) -> Result<Vec<LanguageElementStructless<'a>>, ParseError> {
		let mut struct_types = HashMap::new();
		let mut structs_and_struct_pointers = HashMap::new();
		let mut functions = HashMap::new();
		LanguageElementStructless::from_language_elements_internal(
			elements,
			&mut struct_types,
			&mut structs_and_struct_pointers,
			&mut functions,
		)
	}

	fn from_language_elements_internal(
		elements: Vec<LanguageElement<'a>>,
		struct_types: &mut HashMap<Cow<'a, str>, Vec<Variable<'a>>>,
		structs_and_struct_pointers: &mut HashMap<Cow<'a, str>, &'a str>,
		functions: &mut HashMap<Cow<'a, str>, Vec<Variable<'a>>>,
	) -> Result<Vec<LanguageElementStructless<'a>>, ParseError> {
		let mut new_elements = Vec::new();
		let mut structs: HashMap<Cow<str>, Cow<str>> = HashMap::new();
		for element in elements {
			match element {
				LanguageElement::StructDefinition { name, members } => {
					struct_types.insert(name, members);
				}

				LanguageElement::VariableDeclaration {
					typ,
					name,
					is_static,
					is_const,
					is_volatile,
				} => {
					match typ {
						Type::Struct(n) => {
							let fields = struct_types
								.get(n)
								.ok_or(ParseError(line!(), "Undefined struct type"))?;
							structs_and_struct_pointers.insert(name.clone(), n);
							new_elements.push(LanguageElementStructless::StructDeclaration {
								name: name.clone(),
								is_static,
								is_const,
								is_volatile,
							});
							for field in fields {
								new_elements.push(LanguageElementStructless::VariableDeclaration {
									name: helper::merge_name_and_field(&name, field.name),
									typ: (&field.typ).into(), //This is also such a hack
									is_static,
									is_const,
									is_volatile,
								});
							}
						}
						Type::Arr(t, len) => {
							let t = t.as_ref();
							for idx in (0..len).rev() {
								new_elements.push(LanguageElementStructless::VariableDeclaration {
									typ: t.into(),
									name: Cow::Owned(format!("{}[{}]", &name, idx)),
									is_static,
									is_const,
									is_volatile,
								});
							}
							let target_name = Cow::Owned(format!("{}[0]", &name));
							new_elements.push(
								LanguageElementStructless::VariableDeclarationAssignment {
									typ: NativeType::ptr(t.into()),
									name,
									value: StatementElementStructless::AdrOf(target_name),
									is_static,
									is_const,
									is_volatile,
								},
							)
						}
						_ => {
							new_elements.push(LanguageElementStructless::VariableDeclaration {
								typ: typ.into(),
								name,
								is_static,
								is_const,
								is_volatile,
							});
						}
					}
				}

				LanguageElement::VariableAssignment { name, value } => {
					new_elements.push(LanguageElementStructless::VariableAssignment {
						name,
						value: StatementElementStructless::from(
							&value,
							struct_types,
							structs_and_struct_pointers,
							functions,
						)?,
					})
				}

				LanguageElement::StructAssignment { name, value } => {
					let name: &str = &name;
					let struct_type = structs
						.get(name)
						.ok_or(ParseError(line!(), "Struct variable missing!"))?;
					let fields = struct_types
						.get(struct_type)
						.ok_or(ParseError(line!(), "Undefined struct type"))?;
					for (val, field) in value.into_iter().zip(fields.iter()) {
						new_elements.push(LanguageElementStructless::VariableAssignment {
							name: helper::merge_name_and_field(name, field.name),
							value: StatementElementStructless::from(
								&val,
								struct_types,
								structs_and_struct_pointers,
								functions,
							)?,
						});
					}
				}

				LanguageElement::VariableDeclarationAssignment {
					typ: Type::Arr(t, _),
					name,
					value: StatementElement::Array(arr),
					is_static: true,
					is_const,
					is_volatile,
				} => {
					new_elements.push(LanguageElementStructless::VariableDeclarationAssignment {
						typ: t.as_ref().into(),
						name,
						value: StatementElementStructless::from(
							&StatementElement::Array(arr),
							struct_types,
							structs_and_struct_pointers,
							functions,
						)?,
						is_static: true,
						is_const,
						is_volatile,
					});
				}

				LanguageElement::VariableDeclarationAssignment {
					typ,
					name,
					value,
					is_static,
					is_const,
					is_volatile,
				} => {
					let value = StatementElementStructless::from(
						&value,
						struct_types,
						structs_and_struct_pointers,
						functions,
					)?;
					if let Some(n) = typ.get_struct_type() {
						structs_and_struct_pointers.insert(name.clone(), n);
					}
					if let Type::Arr(t, _) = typ {
						let t = t.as_ref();
						let alloc_name: Cow<'a, str> = Cow::Owned(name.to_string() + "_alloc");
						new_elements.push(
							LanguageElementStructless::VariableDeclarationAssignment {
								typ: t.into(),
								name: alloc_name.clone(),
								value,
								is_static,
								is_const,
								is_volatile,
							},
						);
						new_elements.push(
							LanguageElementStructless::VariableDeclarationAssignment {
								typ: NativeType::ptr(t.into()),
								name,
								value: StatementElementStructless::AdrOf(alloc_name),
								is_static,
								is_const,
								is_volatile,
							},
						)
					} else {
						new_elements.push(
							LanguageElementStructless::VariableDeclarationAssignment {
								typ: typ.into(),
								name,
								value,
								is_static,
								is_const,
								is_volatile,
							},
						);
					}
				}

				LanguageElement::StructDeclarationAssignment {
					typ,
					name,
					value,
					is_static,
					is_const,
					is_volatile,
				} => {
					let struct_type = if let Type::Struct(struct_type) = typ {
						Ok(struct_type)
					} else {
						dbg!(&typ);
						Err(ParseError(line!(), "Internal error: Type is not struct"))
					}?;
					structs_and_struct_pointers.insert(name.clone(), struct_type);
					let fields = if let Some(fields) = struct_types.get(struct_type) {
						fields
					} else {
						dbg!(LanguageElement::StructDeclarationAssignment {
							typ,
							name,
							value,
							is_static,
							is_const,
							is_volatile,
						});
						dbg!(struct_types, structs_and_struct_pointers);
						return Err(ParseError(line!(), "Undefined struct type"));
					};
					new_elements.push(LanguageElementStructless::StructDeclaration {
						name: name.clone(),
						is_static,
						is_const,
						is_volatile,
					});
					for (val, field) in value.into_iter().zip(fields.iter()) {
						new_elements.push(
							LanguageElementStructless::VariableDeclarationAssignment {
								name: helper::merge_name_and_field(&name, field.name),
								value: StatementElementStructless::from(
									&val,
									struct_types,
									structs_and_struct_pointers,
									functions,
								)?,
								typ: (&field.typ).into(), //Such a hack
								is_static,
								is_const,
								is_volatile,
							},
						);
					}
					structs.insert(name, Cow::Borrowed(struct_type));
				}

				LanguageElement::PointerAssignment { ptr, value } => {
					new_elements.push(LanguageElementStructless::PointerAssignment {
						ptr: StatementElementStructless::from(
							&ptr,
							struct_types,
							structs_and_struct_pointers,
							functions,
						)?,
						value: StatementElementStructless::from(
							&value,
							struct_types,
							structs_and_struct_pointers,
							functions,
						)?,
					})
				}

				LanguageElement::StructFieldPointerAssignment { name, field, value } => {
					let name_borrowed: &str = &name;
					let struct_type_name = *structs_and_struct_pointers
						.get(name_borrowed)
						.ok_or_else(|| {
							eprintln!("{}->{} = {:?}", name, field, value);
							dbg!(name_borrowed, &structs_and_struct_pointers);
							ParseError(line!(), "Variable wasn't of struct or struct pointer type")
						})?;
					let fields = struct_types
						.get(struct_type_name)
						.ok_or(ParseError(line!(), "Undefined struct type"))?;
					let idx = fields
						.iter()
						.position(|&Variable { name, .. }| name == field)
						.ok_or(ParseError(line!(), "Unknown field name"))?;
					let new_ptr = if idx == 0 {
						StatementElementStructless::Var(name)
					} else {
						StatementElementStructless::Add {
							lhs: Box::new(StatementElementStructless::Num(idx as isize)),
							rhs: Box::new(StatementElementStructless::Var(name)),
						}
					};
					new_elements.push(LanguageElementStructless::PointerAssignment {
						ptr: new_ptr,
						value: StatementElementStructless::from(
							&value,
							struct_types,
							structs_and_struct_pointers,
							functions,
						)?,
					});
				}

				LanguageElement::FunctionDeclaration {
					typ,
					name,
					args,
					block,
				} => {
					if matches!(typ, Type::Struct(_)) {
						return Err(ParseError(
							line!(),
							"Cannot return struct from function due to ABI limitation. \
							Maybe try having an out pointer parametre instead? (Sorry)",
						));
					}
					functions.insert(name.clone(), args.clone());
					let new_args = args
						.into_iter()
						.map(|v| v.split_into_native(struct_types))
						.collect::<Result<Vec<_>, _>>()? //There must be a better way
						.into_iter()
						.flat_map(|vec| vec.into_iter())
						.collect();
					new_elements.push(LanguageElementStructless::FunctionDeclaration {
						typ: typ.into(),
						name,
						args: new_args,
						block: LanguageElementStructless::from_language_elements_internal(
							block,
							struct_types,
							structs_and_struct_pointers,
							functions,
						)?,
					})
				}

				LanguageElement::IfStatement {
					condition,
					then,
					else_then,
				} => new_elements.push(LanguageElementStructless::IfStatement {
					condition: StatementElementStructless::from(
						&condition,
						struct_types,
						structs_and_struct_pointers,
						functions,
					)?,
					then: Box::new(LanguageElementStructless::Block {
						block: LanguageElementStructless::from_language_elements_internal(
							then,
							struct_types,
							structs_and_struct_pointers,
							functions,
						)?,
						scope_name: Cow::Borrowed("if_then"),
					}),
					//no, clippy, this can't be replaced with a Option::map
					else_then: if let Some(else_then) = else_then {
						Some(Box::new(LanguageElementStructless::Block {
							block: LanguageElementStructless::from_language_elements_internal(
								else_then,
								struct_types,
								structs_and_struct_pointers,
								functions,
							)?,
							scope_name: Cow::Borrowed("if_else"),
						}))
					} else {
						None
					},
				}),

				LanguageElement::For {
					init,
					condition,
					post,
					body,
				} => {
					let mut init_block =
						LanguageElementStructless::from_language_elements_internal(
							init,
							struct_types,
							structs_and_struct_pointers,
							functions,
						)?;
					let condition = StatementElementStructless::from(
						&condition,
						struct_types,
						structs_and_struct_pointers,
						functions,
					)?;
					let mut body = LanguageElementStructless::from_language_elements_internal(
						body,
						struct_types,
						structs_and_struct_pointers,
						functions,
					)?;
					let mut post = LanguageElementStructless::from_language_elements_internal(
						post,
						struct_types,
						structs_and_struct_pointers,
						functions,
					)?;

					body.append(&mut post);
					init_block.push(LanguageElementStructless::Loop {
						condition,
						body: Box::new(LanguageElementStructless::Block {
							scope_name: Cow::Borrowed("for_body"),
							block: body,
						}),
					});

					new_elements.push(LanguageElementStructless::Block {
						scope_name: Cow::Borrowed("for_init"),
						block: init_block,
					})
				}

				LanguageElement::While { condition, body } => {
					new_elements.push(LanguageElementStructless::Loop {
						condition: StatementElementStructless::from(
							&condition,
							struct_types,
							structs_and_struct_pointers,
							functions,
						)?,
						body: Box::new(LanguageElementStructless::Block {
							block: LanguageElementStructless::from_language_elements_internal(
								body,
								struct_types,
								structs_and_struct_pointers,
								functions,
							)?,
							scope_name: Cow::Borrowed("while_body"),
						}),
					})
				}

				LanguageElement::Return(ret) => {
					let ret = if let Some(value) = ret {
						Some(StatementElementStructless::from(
							&value,
							struct_types,
							structs_and_struct_pointers,
							functions,
						)?)
					} else {
						None
					};
					new_elements.push(LanguageElementStructless::Return(ret))
				}

				LanguageElement::Statement(stat) => new_elements.push(
					LanguageElementStructless::Statement(StatementElementStructless::from(
						&stat,
						struct_types,
						structs_and_struct_pointers,
						functions,
					)?),
				),
			}
		}
		Ok(new_elements)
	}
}

///Tree structure to represent a statement. Boolean and bitwise logic are combined
#[derive(Debug, Clone, PartialEq)]
pub enum StatementElementStructless<'a> {
	Add {
		lhs: Box<StatementElementStructless<'a>>,
		rhs: Box<StatementElementStructless<'a>>,
	},
	Sub {
		lhs: Box<StatementElementStructless<'a>>,
		rhs: Box<StatementElementStructless<'a>>,
	},
	Mul {
		lhs: Box<StatementElementStructless<'a>>,
		rhs: Box<StatementElementStructless<'a>>,
	},
	Div {
		lhs: Box<StatementElementStructless<'a>>,
		rhs: Box<StatementElementStructless<'a>>,
	},
	Mod {
		lhs: Box<StatementElementStructless<'a>>,
		rhs: Box<StatementElementStructless<'a>>,
	},
	LShift {
		lhs: Box<StatementElementStructless<'a>>,
		rhs: Box<StatementElementStructless<'a>>,
	},
	RShift {
		lhs: Box<StatementElementStructless<'a>>,
		rhs: Box<StatementElementStructless<'a>>,
	},
	And {
		lhs: Box<StatementElementStructless<'a>>,
		rhs: Box<StatementElementStructless<'a>>,
	},
	Or {
		lhs: Box<StatementElementStructless<'a>>,
		rhs: Box<StatementElementStructless<'a>>,
	},
	Xor {
		lhs: Box<StatementElementStructless<'a>>,
		rhs: Box<StatementElementStructless<'a>>,
	},
	GreaterThan {
		lhs: Box<StatementElementStructless<'a>>,
		rhs: Box<StatementElementStructless<'a>>,
	},
	LessThan {
		lhs: Box<StatementElementStructless<'a>>,
		rhs: Box<StatementElementStructless<'a>>,
	},
	GreaterThanEqual {
		lhs: Box<StatementElementStructless<'a>>,
		rhs: Box<StatementElementStructless<'a>>,
	},
	LessThanEqual {
		lhs: Box<StatementElementStructless<'a>>,
		rhs: Box<StatementElementStructless<'a>>,
	},
	Cmp {
		lhs: Box<StatementElementStructless<'a>>,
		rhs: Box<StatementElementStructless<'a>>,
	},
	NotCmp {
		lhs: Box<StatementElementStructless<'a>>,
		rhs: Box<StatementElementStructless<'a>>,
	},
	FunctionCall {
		name: Cow<'a, str>,
		parametres: Vec<StatementElementStructless<'a>>,
	},
	Not(Box<StatementElementStructless<'a>>),
	Var(Cow<'a, str>),
	Num(isize),
	Char(char),
	Bool(bool),
	Array(Vec<StatementElementStructless<'a>>),
	Deref(Box<StatementElementStructless<'a>>),
	AdrOf(Cow<'a, str>),
}

impl<'a> StatementElementStructless<'a> {
	fn from(
		other: &StatementElement<'a>,
		struct_types: &HashMap<Cow<'a, str>, Vec<Variable<'a>>>,
		structs_and_struct_pointers: &HashMap<Cow<'a, str>, &'a str>,
		functions: &HashMap<Cow<'a, str>, Vec<Variable<'a>>>,
	) -> Result<Self, ParseError> {
		macro_rules! bin_op {
			($i:ident, $lhs:expr, $rhs:expr) => {
				StatementElementStructless::$i {
					lhs: Box::new(StatementElementStructless::from(
						$lhs.as_ref(),
						struct_types,
						structs_and_struct_pointers,
						functions,
					)?),
					rhs: Box::new(StatementElementStructless::from(
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
				StatementElementStructless::$i(Box::new(StatementElementStructless::from(
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
							Err(ParseError(line!(), "Only struct variables can be passed into functions with struct arguments (not literals)"))
						}?;
						for field in fields.iter() {
							let param_name = helper::merge_name_and_field(var_name, field.name);
							new_parametres.push(StatementElementStructless::Var(param_name))
						}
					} else {
						new_parametres.push(StatementElementStructless::from(
							param,
							struct_types,
							structs_and_struct_pointers,
							functions,
						)?);
					}
				}
				StatementElementStructless::FunctionCall {
					name: name.clone(),
					parametres: new_parametres,
				}
			}
			StatementElement::Var(v) => StatementElementStructless::Var(v.clone()),
			StatementElement::Num(n) => StatementElementStructless::Num(*n),
			StatementElement::Char(c) => StatementElementStructless::Char(*c),
			StatementElement::Bool(b) => StatementElementStructless::Bool(*b),
			StatementElement::Array(arr) => StatementElementStructless::Array(
				arr.iter()
					.map(|parametre| {
						StatementElementStructless::from(
							parametre,
							struct_types,
							structs_and_struct_pointers,
							functions,
						)
					})
					.collect::<Result<_, _>>()?,
			),
			StatementElement::Deref(n) => un_op!(Deref, n),
			StatementElement::AdrOf(n) => StatementElementStructless::AdrOf(n.clone()),

			StatementElement::FieldPointerAccess(name, field) => {
				let struct_type = structs_and_struct_pointers.get(name).ok_or(ParseError(
					line!(),
					"Variable wasn't of struct or struct pointer type",
				))?;
				let fields = struct_types
					.get(*struct_type)
					.ok_or(ParseError(line!(), "Undefined struct type"))?;
				let idx = fields
					.iter()
					.position(|field_name| field_name.name == field)
					.ok_or(ParseError(line!(), "Unknown field type"))? as isize;
				StatementElementStructless::Deref(Box::new(StatementElementStructless::Add {
					lhs: Box::new(StatementElementStructless::Num(idx)),
					rhs: Box::new(StatementElementStructless::AdrOf(name.clone())),
				}))
			}

			StatementElement::Ternary { .. } => todo!(),
		};
		Ok(res)
	}
}

impl<'a> StatementElementStructless<'a> {
	pub(crate) fn depth(&self) -> usize {
		let rest = match self {
			StatementElementStructless::Add { lhs, rhs }
			| StatementElementStructless::Sub { lhs, rhs }
			| StatementElementStructless::Mul { lhs, rhs }
			| StatementElementStructless::Div { lhs, rhs }
			| StatementElementStructless::Mod { lhs, rhs }
			| StatementElementStructless::LShift { lhs, rhs }
			| StatementElementStructless::RShift { lhs, rhs }
			| StatementElementStructless::And { lhs, rhs }
			| StatementElementStructless::Or { lhs, rhs }
			| StatementElementStructless::Xor { lhs, rhs }
			| StatementElementStructless::GreaterThan { lhs, rhs }
			| StatementElementStructless::LessThan { lhs, rhs }
			| StatementElementStructless::GreaterThanEqual { lhs, rhs }
			| StatementElementStructless::LessThanEqual { lhs, rhs }
			| StatementElementStructless::Cmp { lhs, rhs }
			| StatementElementStructless::NotCmp { lhs, rhs } => {
				lhs.as_ref().depth().max(rhs.as_ref().depth())
			}
			StatementElementStructless::Not(lhs) => lhs.as_ref().depth(),
			StatementElementStructless::Array(n) => n.iter().map(|e| e.depth()).max().unwrap_or(0),
			StatementElementStructless::Deref(n) => n.as_ref().depth(),
			StatementElementStructless::Var(_)
			| StatementElementStructless::Num(_)
			| StatementElementStructless::Char(_)
			| StatementElementStructless::Bool(_)
			| StatementElementStructless::AdrOf(_) => 0,
			StatementElementStructless::FunctionCall { .. } => 1, //Each parametre is its own memory alloc but can still require 1 if the function call is on the rhs
		};
		rest + 1
	}

	pub(crate) fn internal_ref(
		&self,
	) -> Option<(&StatementElementStructless, &StatementElementStructless)> {
		match self {
			StatementElementStructless::Add { lhs, rhs }
			| StatementElementStructless::Sub { lhs, rhs }
			| StatementElementStructless::Mul { lhs, rhs }
			| StatementElementStructless::Div { lhs, rhs }
			| StatementElementStructless::Mod { lhs, rhs }
			| StatementElementStructless::LShift { lhs, rhs }
			| StatementElementStructless::RShift { lhs, rhs }
			| StatementElementStructless::And { lhs, rhs }
			| StatementElementStructless::Or { lhs, rhs }
			| StatementElementStructless::Xor { lhs, rhs }
			| StatementElementStructless::GreaterThan { lhs, rhs }
			| StatementElementStructless::LessThan { lhs, rhs }
			| StatementElementStructless::GreaterThanEqual { lhs, rhs }
			| StatementElementStructless::LessThanEqual { lhs, rhs }
			| StatementElementStructless::NotCmp { lhs, rhs }
			| StatementElementStructless::Cmp { lhs, rhs } => Some((lhs.as_ref(), rhs.as_ref())),
			StatementElementStructless::Not(_)
			| StatementElementStructless::FunctionCall { .. }
			| StatementElementStructless::Var(_)
			| StatementElementStructless::Num(_)
			| StatementElementStructless::Char(_)
			| StatementElementStructless::Bool(_)
			| StatementElementStructless::Array(_)
			| StatementElementStructless::Deref(_)
			| StatementElementStructless::AdrOf(_) => None,
		}
	}
}

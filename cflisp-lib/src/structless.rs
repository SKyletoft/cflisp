use crate::*;
use std::{borrow::Cow, collections::HashMap};

///Internal representation of the program.
/// Can represent any language pattern considered valid.
/// (language patterns are complete lines. Right hand side
/// statements are `StatementElement`s)
#[derive(Debug, Clone, PartialEq)]
pub enum LanguageElementStructless<'a> {
	VariableDeclaration {
		typ: NativeType,
		name: Cow<'a, str>,
		is_static: bool,
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
		then: BlockStructless<'a>,
		else_then: Option<BlockStructless<'a>>,
	},
	For {
		init: BlockStructless<'a>,
		condition: StatementElementStructless<'a>,
		post: BlockStructless<'a>,
		body: BlockStructless<'a>,
	},
	While {
		condition: StatementElementStructless<'a>,
		body: BlockStructless<'a>,
	},
	Return(Option<StatementElementStructless<'a>>),
	Statement(StatementElementStructless<'a>),
	StructDeclaration {
		name: Cow<'a, str>,
		is_static: bool,
	},
	Block(BlockStructless<'a>),
}

impl<'a> LanguageElementStructless<'a> {
	//refactor into doing it one by one to an option and then map over/collect?
	// but not until StatementElements have been unstructified
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
				} => {
					if let Some(n) = typ.get_struct_type() {
						let fields = struct_types
							.get(n)
							.ok_or(ParseError(line!(), "Undefined struct type"))?;
						structs_and_struct_pointers.insert(name.clone(), n);
						new_elements.push(LanguageElementStructless::StructDeclaration {
							name: name.clone(),
							is_static,
						});
						for field in fields {
							new_elements.push(LanguageElementStructless::VariableDeclaration {
								name: Cow::Owned(name.to_string() + "::" + field.name),
								typ: (&field.typ).into(), //This is also such a hack
								is_static,
							});
						}
					} else {
						new_elements.push(LanguageElementStructless::VariableDeclaration {
							typ: typ.into(),
							name,
							is_static,
						})
					}
				}
				LanguageElement::VariableAssignment { name, value } => {
					new_elements.push(LanguageElementStructless::VariableAssignment {
						name,
						value: StatementElementStructless::from(
							&value,
							struct_types,
							structs_and_struct_pointers,
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
							name: Cow::Owned(name.to_string() + "::" + field.name),
							value: StatementElementStructless::from(
								&val,
								struct_types,
								structs_and_struct_pointers,
							)?,
						});
					}
				}
				LanguageElement::VariableDeclarationAssignment {
					typ,
					name,
					value,
					is_static,
				} => {
					if let Some(n) = typ.get_struct_type() {
						structs_and_struct_pointers.insert(name.clone(), n);
					}
					new_elements.push(LanguageElementStructless::VariableDeclarationAssignment {
						typ: typ.into(),
						name,
						value: StatementElementStructless::from(
							&value,
							struct_types,
							structs_and_struct_pointers,
						)?,
						is_static,
					})
				}
				LanguageElement::StructDeclarationAssignment {
					typ,
					name,
					value,
					is_static,
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
							is_static
						});
						dbg!(struct_types, structs_and_struct_pointers);
						return Err(ParseError(line!(), "Undefined struct type"));
					};
					new_elements.push(LanguageElementStructless::StructDeclaration {
						name: name.clone(),
						is_static,
					});
					for (val, field) in value.into_iter().zip(fields.iter()) {
						new_elements.push(
							LanguageElementStructless::VariableDeclarationAssignment {
								name: Cow::Owned(name.to_string() + "::" + field.name),
								value: StatementElementStructless::from(
									&val,
									struct_types,
									structs_and_struct_pointers,
								)?,
								typ: (&field.typ).into(), //Such a hack
								is_static,
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
						)?,
						value: StatementElementStructless::from(
							&value,
							struct_types,
							structs_and_struct_pointers,
						)?,
					})
				}
				LanguageElement::StructFieldPointerAssignment { name, field, value } => {
					let name_borrowed: &str = &name;
					let struct_type_name =
						*structs_and_struct_pointers
							.get(name_borrowed)
							.ok_or(ParseError(
								line!(),
								"Variable wasn't of struct or struct pointer type",
							))?;
					let fields = struct_types
						.get(struct_type_name)
						.ok_or(ParseError(line!(), "Undefined struct type"))?;
					let idx = fields
						.iter()
						.position(|&Variable { typ: _, name }| name == field)
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
					)?,
					then: LanguageElementStructless::from_language_elements_internal(
						then,
						struct_types,
						structs_and_struct_pointers,
						functions,
					)?,
					//no, clippy, this can't be replaced with a Option::map
					else_then: if let Some(else_then) = else_then {
						Some(LanguageElementStructless::from_language_elements_internal(
							else_then,
							struct_types,
							structs_and_struct_pointers,
							functions,
						)?)
					} else {
						None
					},
				}),
				LanguageElement::For {
					init,
					condition,
					post,
					body,
				} => new_elements.push(LanguageElementStructless::For {
					init: LanguageElementStructless::from_language_elements_internal(
						init,
						struct_types,
						structs_and_struct_pointers,
						functions,
					)?,
					condition: StatementElementStructless::from(
						&condition,
						struct_types,
						structs_and_struct_pointers,
					)?,
					post: LanguageElementStructless::from_language_elements_internal(
						post,
						struct_types,
						structs_and_struct_pointers,
						functions,
					)?,
					body: LanguageElementStructless::from_language_elements_internal(
						body,
						struct_types,
						structs_and_struct_pointers,
						functions,
					)?,
				}),
				LanguageElement::While { condition, body } => {
					new_elements.push(LanguageElementStructless::While {
						condition: StatementElementStructless::from(
							&condition,
							struct_types,
							structs_and_struct_pointers,
						)?,
						body: LanguageElementStructless::from_language_elements_internal(
							body,
							struct_types,
							structs_and_struct_pointers,
							functions,
						)?,
					})
				}
				LanguageElement::Return(ret) => {
					let ret = if let Some(value) = ret {
						Some(StatementElementStructless::from(
							&value,
							struct_types,
							structs_and_struct_pointers,
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
	Not {
		lhs: Box<StatementElementStructless<'a>>,
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
	) -> Result<Self, ParseError> {
		//Todo! Figure out macro_rules for this repetition nonsense
		let res = match other {
			StatementElement::Add { lhs, rhs } => StatementElementStructless::Add {
				lhs: Box::new(StatementElementStructless::from(
					lhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
				rhs: Box::new(StatementElementStructless::from(
					rhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
			},
			StatementElement::Sub { lhs, rhs } => StatementElementStructless::Sub {
				lhs: Box::new(StatementElementStructless::from(
					lhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
				rhs: Box::new(StatementElementStructless::from(
					rhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
			},
			StatementElement::Mul { lhs, rhs } => StatementElementStructless::Mul {
				lhs: Box::new(StatementElementStructless::from(
					lhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
				rhs: Box::new(StatementElementStructless::from(
					rhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
			},
			StatementElement::Div { lhs, rhs } => StatementElementStructless::Div {
				lhs: Box::new(StatementElementStructless::from(
					lhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
				rhs: Box::new(StatementElementStructless::from(
					rhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
			},
			StatementElement::Mod { lhs, rhs } => StatementElementStructless::Mod {
				lhs: Box::new(StatementElementStructless::from(
					lhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
				rhs: Box::new(StatementElementStructless::from(
					rhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
			},
			StatementElement::LShift { lhs, rhs } => StatementElementStructless::LShift {
				lhs: Box::new(StatementElementStructless::from(
					lhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
				rhs: Box::new(StatementElementStructless::from(
					rhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
			},
			StatementElement::RShift { lhs, rhs } => StatementElementStructless::RShift {
				lhs: Box::new(StatementElementStructless::from(
					lhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
				rhs: Box::new(StatementElementStructless::from(
					rhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
			},
			StatementElement::And { lhs, rhs } => StatementElementStructless::And {
				lhs: Box::new(StatementElementStructless::from(
					lhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
				rhs: Box::new(StatementElementStructless::from(
					rhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
			},
			StatementElement::Or { lhs, rhs } => StatementElementStructless::Or {
				lhs: Box::new(StatementElementStructless::from(
					lhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
				rhs: Box::new(StatementElementStructless::from(
					rhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
			},
			StatementElement::Xor { lhs, rhs } => StatementElementStructless::Xor {
				lhs: Box::new(StatementElementStructless::from(
					lhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
				rhs: Box::new(StatementElementStructless::from(
					rhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
			},
			StatementElement::GreaterThan { lhs, rhs } => StatementElementStructless::GreaterThan {
				lhs: Box::new(StatementElementStructless::from(
					lhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
				rhs: Box::new(StatementElementStructless::from(
					rhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
			},
			StatementElement::LessThan { lhs, rhs } => StatementElementStructless::LessThan {
				lhs: Box::new(StatementElementStructless::from(
					lhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
				rhs: Box::new(StatementElementStructless::from(
					rhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
			},
			StatementElement::GreaterThanEqual { lhs, rhs } => {
				StatementElementStructless::GreaterThanEqual {
					lhs: Box::new(StatementElementStructless::from(
						lhs.as_ref(),
						struct_types,
						structs_and_struct_pointers,
					)?),
					rhs: Box::new(StatementElementStructless::from(
						rhs.as_ref(),
						struct_types,
						structs_and_struct_pointers,
					)?),
				}
			}
			StatementElement::LessThanEqual { lhs, rhs } => {
				StatementElementStructless::LessThanEqual {
					lhs: Box::new(StatementElementStructless::from(
						lhs.as_ref(),
						struct_types,
						structs_and_struct_pointers,
					)?),
					rhs: Box::new(StatementElementStructless::from(
						rhs.as_ref(),
						struct_types,
						structs_and_struct_pointers,
					)?),
				}
			}
			StatementElement::Cmp { lhs, rhs } => StatementElementStructless::Cmp {
				lhs: Box::new(StatementElementStructless::from(
					lhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
				rhs: Box::new(StatementElementStructless::from(
					rhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
			},
			StatementElement::NotCmp { lhs, rhs } => StatementElementStructless::NotCmp {
				lhs: Box::new(StatementElementStructless::from(
					lhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
				rhs: Box::new(StatementElementStructless::from(
					rhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
			},

			StatementElement::Not { lhs } => StatementElementStructless::Not {
				lhs: Box::new(StatementElementStructless::from(
					lhs.as_ref(),
					struct_types,
					structs_and_struct_pointers,
				)?),
			},

			StatementElement::FunctionCall { name, parametres } => {
				StatementElementStructless::FunctionCall {
					name: name.clone(),
					parametres: parametres
						.iter()
						.map(|parametre| {
							StatementElementStructless::from(
								parametre,
								struct_types,
								structs_and_struct_pointers,
							)
						})
						.collect::<Result<_, _>>()?,
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
						)
					})
					.collect::<Result<_, _>>()?,
			),
			StatementElement::Deref(n) => StatementElementStructless::Deref(Box::new(
				StatementElementStructless::from(n, struct_types, structs_and_struct_pointers)?,
			)),
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
		};
		Ok(res)
	}
}

impl<'a> StatementElementStructless<'a> {
	///Returns the corresponding instruction for the root element of the tree. Ignores branches.
	pub fn as_flisp_instruction(&self, adr: Addressing) -> Result<Instruction, CompileError> {
		let res = match self {
			StatementElementStructless::Add { lhs: _, rhs: _ } => Instruction::ADDA(adr),
			StatementElementStructless::Sub { lhs: _, rhs: _ } => Instruction::SUBA(adr),
			StatementElementStructless::Mul { lhs: _, rhs: _ }
			| StatementElementStructless::Div { lhs: _, rhs: _ }
			| StatementElementStructless::Mod { lhs: _, rhs: _ } => {
				return Err(CompileError(
					line!(),
					"Internal error: function call, not instruction?",
				));
			}
			StatementElementStructless::LShift { lhs: _, rhs: _ } => Instruction::LSLA,
			StatementElementStructless::RShift { lhs: _, rhs: _ } => Instruction::LSRA,
			StatementElementStructless::And { lhs: _, rhs: _ } => Instruction::ANDA(adr),
			StatementElementStructless::Or { lhs: _, rhs: _ } => Instruction::ORA(adr),
			StatementElementStructless::Xor { lhs: _, rhs: _ } => Instruction::EORA(adr),
			StatementElementStructless::Not { lhs: _ } => Instruction::COMA,
			StatementElementStructless::GreaterThan { lhs: _, rhs: _ } => Instruction::SUBA(adr),
			StatementElementStructless::LessThan { lhs: _, rhs: _ } => Instruction::SUBA(adr),
			StatementElementStructless::GreaterThanEqual { lhs: _, rhs: _ } => {
				Instruction::SUBA(adr)
			}
			StatementElementStructless::LessThanEqual { lhs: _, rhs: _ } => Instruction::SUBA(adr),
			StatementElementStructless::Cmp { lhs: _, rhs: _ } => Instruction::SUBA(adr),
			StatementElementStructless::NotCmp { lhs: _, rhs: _ } => Instruction::SUBA(adr),
			StatementElementStructless::Var(_)
			| StatementElementStructless::Num(_)
			| StatementElementStructless::Char(_)
			| StatementElementStructless::Bool(_)
			| StatementElementStructless::Array(_)
			| StatementElementStructless::Deref(_)
			| StatementElementStructless::AdrOf(_)
			| StatementElementStructless::FunctionCall {
				name: _,
				parametres: _,
			} => {
				return Err(CompileError(
					line!(),
					"Internal error: special cases and literals, not instructions?",
				))
			}
		};

		Ok(res)
	}

	pub fn depth(&self) -> usize {
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
			StatementElementStructless::Not { lhs } => lhs.as_ref().depth(),
			StatementElementStructless::Array(n) => n.iter().map(|e| e.depth()).max().unwrap_or(0),
			StatementElementStructless::Deref(n) => n.as_ref().depth(),
			StatementElementStructless::Var(_)
			| StatementElementStructless::Num(_)
			| StatementElementStructless::Char(_)
			| StatementElementStructless::Bool(_)
			| StatementElementStructless::AdrOf(_) => 0,
			StatementElementStructless::FunctionCall {
				name: _,
				parametres: _,
			} => 1, //Each parametre is its own memory alloc but can still require 1 if the function call is on the rhs
		};
		rest + 1
	}

	pub fn internal_ref(
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
			StatementElementStructless::Not { lhs: _ }
			| StatementElementStructless::FunctionCall {
				name: _,
				parametres: _,
			}
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

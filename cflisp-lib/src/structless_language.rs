use crate::*;
use std::{borrow::Cow, collections::HashMap};

///Simplified internal representation of a program.
/// Doesn't contain structs or for loops
#[derive(Debug, Clone, PartialEq)]
pub enum StructlessLanguage<'a> {
	VariableDeclaration {
		typ: NativeType,
		name: Cow<'a, str>,
		is_static: bool,
		is_const: bool,
		is_volatile: bool,
	},
	VariableAssignment {
		name: Cow<'a, str>,
		value: StructlessStatement<'a>,
	},
	VariableDeclarationAssignment {
		typ: NativeType,
		name: Cow<'a, str>,
		value: StructlessStatement<'a>,
		is_static: bool,
		is_const: bool,
		is_volatile: bool,
	},
	PointerAssignment {
		ptr: StructlessStatement<'a>,
		value: StructlessStatement<'a>,
	},
	FunctionDeclaration {
		typ: NativeType,
		name: Cow<'a, str>,
		args: Vec<NativeVariable<'a>>,
		block: BlockStructless<'a>,
	},
	IfStatement {
		condition: StructlessStatement<'a>,
		then: Box<StructlessLanguage<'a>>,
		else_then: Option<Box<StructlessLanguage<'a>>>,
	},
	Loop {
		condition: StructlessStatement<'a>,
		body: Box<StructlessLanguage<'a>>,
	},
	Return(Option<StructlessStatement<'a>>),
	Statement(StructlessStatement<'a>),
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

impl<'a> StructlessLanguage<'a> {
	///Destructures structs into normal language elements
	pub fn from_language_elements(
		elements: Vec<LanguageElement<'a>>,
	) -> Result<Vec<StructlessLanguage<'a>>, ParseError> {
		let mut struct_types = HashMap::new();
		let mut structs_and_struct_pointers = HashMap::new();
		let mut functions = HashMap::new();

		let mut upper = Vec::new();

		let mut res = StructlessLanguage::from_language_elements_internal(
			elements,
			&mut struct_types,
			&mut structs_and_struct_pointers,
			&mut functions,
			&mut upper,
			Cow::Borrowed("global"),
		)?;

		upper.append(&mut res);
		Ok(upper)
	}

	fn from_language_elements_internal(
		elements: Vec<LanguageElement<'a>>,
		struct_types: &mut HashMap<Cow<'a, str>, Vec<Variable<'a>>>,
		structs_and_struct_pointers: &mut HashMap<Cow<'a, str>, &'a str>,
		functions: &mut HashMap<Cow<'a, str>, Vec<Variable<'a>>>,
		upper: &mut Vec<StructlessLanguage<'a>>,
		scope: Cow<'a, str>,
	) -> Result<Vec<StructlessLanguage<'a>>, ParseError> {
		let mut new_elements = Vec::new();
		let mut structs: HashMap<Cow<str>, Cow<str>> = HashMap::new();
		for element in elements {
			match element {
				LanguageElement::StructDefinition { name, members } => {
					struct_types.insert(name, members);
				}

				LanguageElement::VariableDeclaration {
					typ: Type::Struct(n),
					name,
					is_static: true,
					is_const,
					is_volatile,
				} => {
					let fields = struct_types
						.get(n)
						.ok_or(ParseError(line!(), "Undefined struct type"))?;
					structs_and_struct_pointers.insert(name.clone(), n);
					upper.push(StructlessLanguage::StructDeclaration {
						name: name.clone(),
						is_static: true,
						is_const,
						is_volatile,
					});
					for field in fields {
						upper.push(StructlessLanguage::VariableDeclaration {
							name: Cow::Owned(format!("{}::{}::{}", &scope, &name, field.name)),
							typ: (&field.typ).into(), //This is also such a hack
							is_static: true,
							is_const,
							is_volatile,
						});
					}
				}

				LanguageElement::VariableDeclaration {
					typ: Type::Struct(n),
					name,
					is_static: false,
					is_const,
					is_volatile,
				} => {
					let fields = struct_types
						.get(n)
						.ok_or(ParseError(line!(), "Undefined struct type"))?;
					structs_and_struct_pointers.insert(name.clone(), n);
					new_elements.push(StructlessLanguage::StructDeclaration {
						name: name.clone(),
						is_static: false,
						is_const,
						is_volatile,
					});
					for field in fields {
						new_elements.push(StructlessLanguage::VariableDeclaration {
							name: helper::merge_name_and_field(&name, field.name),
							typ: (&field.typ).into(), //This is also such a hack
							is_static: false,
							is_const,
							is_volatile,
						});
					}
				}

				LanguageElement::VariableDeclaration {
					typ: Type::Arr(t, len),
					name,
					is_static: true,
					is_const,
					is_volatile,
				} => {
					let t = t.as_ref();
					for idx in (0..len).rev() {
						upper.push(StructlessLanguage::VariableDeclaration {
							typ: t.into(),
							name: Cow::Owned(format!("{}::{}[{}]", &scope, &name, idx)),
							is_static: true,
							is_const,
							is_volatile,
						});
					}
					let target_name = Cow::Owned(format!("{}::{}[0]", scope, &name));
					upper.push(StructlessLanguage::VariableDeclarationAssignment {
						typ: NativeType::ptr(t.into()),
						name: Cow::Owned(format!("{}::{}", scope, name)),
						value: StructlessStatement::AdrOf(target_name),
						is_static: true,
						is_const,
						is_volatile,
					});
				}

				LanguageElement::VariableDeclaration {
					typ: Type::Arr(t, len),
					name,
					is_static: false,
					is_const,
					is_volatile,
				} => {
					let t = t.as_ref();
					for idx in (0..len).rev() {
						new_elements.push(StructlessLanguage::VariableDeclaration {
							typ: t.into(),
							name: Cow::Owned(format!("{}[{}]", &name, idx)),
							is_static: false,
							is_const,
							is_volatile,
						});
					}
					let target_name = Cow::Owned(format!("{}[0]", &name));
					new_elements.push(StructlessLanguage::VariableDeclarationAssignment {
						typ: NativeType::ptr(t.into()),
						name,
						value: StructlessStatement::AdrOf(target_name),
						is_static: false,
						is_const,
						is_volatile,
					});
				}

				LanguageElement::VariableDeclaration {
					typ,
					name,
					is_static: true,
					is_const,
					is_volatile,
				} => {
					upper.push(StructlessLanguage::VariableDeclaration {
						typ: typ.into(),
						name: Cow::Owned(format!("{}::{}", scope, name)),
						is_static: true,
						is_const,
						is_volatile,
					});
				}

				LanguageElement::VariableDeclaration {
					typ,
					name,
					is_static: false,
					is_const,
					is_volatile,
				} => {
					new_elements.push(StructlessLanguage::VariableDeclaration {
						typ: typ.into(),
						name,
						is_static: false,
						is_const,
						is_volatile,
					});
				}

				LanguageElement::VariableAssignment { name, value } => {
					new_elements.push(StructlessLanguage::VariableAssignment {
						name,
						value: StructlessStatement::from(
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
						new_elements.push(StructlessLanguage::VariableAssignment {
							name: helper::merge_name_and_field(name, field.name),
							value: StructlessStatement::from(
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
					value,
					is_static: true,
					is_const,
					is_volatile,
				} => {
					let value = StructlessStatement::from(
						&value,
						struct_types,
						structs_and_struct_pointers,
						functions,
					)?;
					if let Some(n) = t.as_ref().get_struct_type() {
						structs_and_struct_pointers.insert(name.clone(), n);
					}
					let t = t.as_ref();
					let alloc_name: Cow<'a, str> = Cow::Owned(format!("{}::{}_alloc", scope, name));
					upper.push(StructlessLanguage::VariableDeclarationAssignment {
						typ: t.into(),
						name: alloc_name.clone(),
						value,
						is_static: true,
						is_const,
						is_volatile,
					});
					upper.push(StructlessLanguage::VariableDeclarationAssignment {
						typ: NativeType::ptr(t.into()),
						name,
						value: StructlessStatement::AdrOf(alloc_name),
						is_static: true,
						is_const,
						is_volatile,
					});
				}

				LanguageElement::VariableDeclarationAssignment {
					typ: Type::Arr(t, _),
					name,
					value,
					is_static: false,
					is_const,
					is_volatile,
				} => {
					let value = StructlessStatement::from(
						&value,
						struct_types,
						structs_and_struct_pointers,
						functions,
					)?;
					if let Some(n) = t.as_ref().get_struct_type() {
						structs_and_struct_pointers.insert(name.clone(), n);
					}
					let t = t.as_ref();
					let alloc_name: Cow<'a, str> = Cow::Owned(format!("{}_alloc", name));
					new_elements.push(StructlessLanguage::VariableDeclarationAssignment {
						typ: t.into(),
						name: alloc_name.clone(),
						value,
						is_static: false,
						is_const,
						is_volatile,
					});
					new_elements.push(StructlessLanguage::VariableDeclarationAssignment {
						typ: NativeType::ptr(t.into()),
						name,
						value: StructlessStatement::AdrOf(alloc_name),
						is_static: false,
						is_const,
						is_volatile,
					});
				}

				LanguageElement::VariableDeclarationAssignment {
					typ,
					name,
					value,
					is_static: true,
					is_const,
					is_volatile,
				} => {
					let value = StructlessStatement::from(
						&value,
						struct_types,
						structs_and_struct_pointers,
						functions,
					)?;
					if let Some(n) = typ.get_struct_type() {
						structs_and_struct_pointers.insert(name.clone(), n);
					}
					upper.push(StructlessLanguage::VariableDeclarationAssignment {
						typ: typ.into(),
						name,
						value,
						is_static: true,
						is_const,
						is_volatile,
					});
				}

				LanguageElement::VariableDeclarationAssignment {
					typ,
					name,
					value,
					is_static: false,
					is_const,
					is_volatile,
				} => {
					let value = StructlessStatement::from(
						&value,
						struct_types,
						structs_and_struct_pointers,
						functions,
					)?;
					if let Some(n) = typ.get_struct_type() {
						structs_and_struct_pointers.insert(name.clone(), n);
					}
					new_elements.push(StructlessLanguage::VariableDeclarationAssignment {
						typ: typ.into(),
						name,
						value,
						is_static: false,
						is_const,
						is_volatile,
					});
				}

				LanguageElement::StructDeclarationAssignment {
					typ,
					name,
					value,
					is_static: true,
					is_const,
					is_volatile,
				} => {
					let name: Cow<'a, str> = Cow::Owned(format!("{}::{}", scope, name));
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
							is_static: true,
							is_const,
							is_volatile,
						});
						dbg!(struct_types, structs_and_struct_pointers);
						return Err(ParseError(line!(), "Undefined struct type"));
					};
					upper.push(StructlessLanguage::StructDeclaration {
						name: name.clone(),
						is_static: true,
						is_const,
						is_volatile,
					});
					for (val, field) in value.into_iter().zip(fields.iter()) {
						upper.push(StructlessLanguage::VariableDeclarationAssignment {
							name: helper::merge_name_and_field(&name, field.name),
							value: StructlessStatement::from(
								&val,
								struct_types,
								structs_and_struct_pointers,
								functions,
							)?,
							typ: (&field.typ).into(), //Such a hack
							is_static: true,
							is_const,
							is_volatile,
						});
					}
					structs.insert(name, Cow::Borrowed(struct_type));
				}

				LanguageElement::StructDeclarationAssignment {
					typ,
					name,
					value,
					is_static: false,
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
							is_static: false,
							is_const,
							is_volatile,
						});
						dbg!(struct_types, structs_and_struct_pointers);
						return Err(ParseError(line!(), "Undefined struct type"));
					};
					new_elements.push(StructlessLanguage::StructDeclaration {
						name: name.clone(),
						is_static: false,
						is_const,
						is_volatile,
					});
					for (val, field) in value.into_iter().zip(fields.iter()) {
						new_elements.push(StructlessLanguage::VariableDeclarationAssignment {
							name: helper::merge_name_and_field(&name, field.name),
							value: StructlessStatement::from(
								&val,
								struct_types,
								structs_and_struct_pointers,
								functions,
							)?,
							typ: (&field.typ).into(), //Such a hack
							is_static: false,
							is_const,
							is_volatile,
						});
					}
					structs.insert(name, Cow::Borrowed(struct_type));
				}

				LanguageElement::PointerAssignment { ptr, value } => {
					new_elements.push(StructlessLanguage::PointerAssignment {
						ptr: StructlessStatement::from(
							&ptr,
							struct_types,
							structs_and_struct_pointers,
							functions,
						)?,
						value: StructlessStatement::from(
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
						StructlessStatement::Var(name)
					} else {
						StructlessStatement::Add {
							lhs: Box::new(StructlessStatement::Num(idx as isize)),
							rhs: Box::new(StructlessStatement::Var(name)),
						}
					};
					new_elements.push(StructlessLanguage::PointerAssignment {
						ptr: new_ptr,
						value: StructlessStatement::from(
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
					let new_scope = name.clone();
					new_elements.push(StructlessLanguage::FunctionDeclaration {
						typ: typ.into(),
						name,
						args: new_args,
						block: StructlessLanguage::from_language_elements_internal(
							block,
							struct_types,
							structs_and_struct_pointers,
							functions,
							upper,
							new_scope,
						)?,
					})
				}

				LanguageElement::IfStatement {
					condition,
					then,
					else_then,
				} => new_elements.push(StructlessLanguage::IfStatement {
					condition: StructlessStatement::from(
						&condition,
						struct_types,
						structs_and_struct_pointers,
						functions,
					)?,
					then: Box::new(StructlessLanguage::Block {
						block: StructlessLanguage::from_language_elements_internal(
							then,
							struct_types,
							structs_and_struct_pointers,
							functions,
							upper,
							scope.clone(),
						)?,
						scope_name: Cow::Borrowed("if_then"),
					}),
					//no, clippy, this can't be replaced with a Option::map
					else_then: if let Some(else_then) = else_then {
						Some(Box::new(StructlessLanguage::Block {
							block: StructlessLanguage::from_language_elements_internal(
								else_then,
								struct_types,
								structs_and_struct_pointers,
								functions,
								upper,
								scope.clone(),
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
					let mut init_block = StructlessLanguage::from_language_elements_internal(
						init,
						struct_types,
						structs_and_struct_pointers,
						functions,
						upper,
						scope.clone(),
					)?;
					let condition = StructlessStatement::from(
						&condition,
						struct_types,
						structs_and_struct_pointers,
						functions,
					)?;
					let mut body = StructlessLanguage::from_language_elements_internal(
						body,
						struct_types,
						structs_and_struct_pointers,
						functions,
						upper,
						scope.clone(),
					)?;
					let mut post = StructlessLanguage::from_language_elements_internal(
						post,
						struct_types,
						structs_and_struct_pointers,
						functions,
						upper,
						scope.clone(),
					)?;

					body.append(&mut post);
					init_block.push(StructlessLanguage::Loop {
						condition,
						body: Box::new(StructlessLanguage::Block {
							scope_name: Cow::Borrowed("for_body"),
							block: body,
						}),
					});

					new_elements.push(StructlessLanguage::Block {
						scope_name: Cow::Borrowed("for_init"),
						block: init_block,
					})
				}

				LanguageElement::While { condition, body } => {
					new_elements.push(StructlessLanguage::Loop {
						condition: StructlessStatement::from(
							&condition,
							struct_types,
							structs_and_struct_pointers,
							functions,
						)?,
						body: Box::new(StructlessLanguage::Block {
							block: StructlessLanguage::from_language_elements_internal(
								body,
								struct_types,
								structs_and_struct_pointers,
								functions,
								upper,
								scope.clone(),
							)?,
							scope_name: Cow::Borrowed("while_body"),
						}),
					})
				}

				LanguageElement::Return(ret) => {
					let ret = if let Some(value) = ret {
						Some(StructlessStatement::from(
							&value,
							struct_types,
							structs_and_struct_pointers,
							functions,
						)?)
					} else {
						None
					};
					new_elements.push(StructlessLanguage::Return(ret))
				}

				LanguageElement::Statement(stat) => {
					new_elements.push(StructlessLanguage::Statement(StructlessStatement::from(
						&stat,
						struct_types,
						structs_and_struct_pointers,
						functions,
					)?))
				}
			}
		}
		Ok(new_elements)
	}
}

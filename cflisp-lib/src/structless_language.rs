use std::{borrow::Cow, collections::HashMap, fmt};

use crate::*;

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
	VariableLabelTag {
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
	) -> Result<Vec<StructlessLanguage<'a>>> {
		let mut struct_types = HashMap::new();
		let mut structs_and_struct_pointers = HashMap::new();
		let mut functions = HashMap::new();
		let mut rename_map = HashMap::new();
		let mut symbols = HashMap::new();
		let scope = Cow::Borrowed("global");

		let mut upper = Vec::new();

		let mut res = StructlessLanguage::from_language_elements_internal(
			elements,
			&mut upper,
			scope,
			&mut rename_map,
			&mut State {
				struct_types: &mut struct_types,
				structs_and_struct_pointers: &mut structs_and_struct_pointers,
				functions: &mut functions,
				symbols: &mut symbols,
			},
		)?;

		upper.append(&mut res);
		Ok(upper)
	}

	fn from_language_elements_internal(
		elements: Vec<LanguageElement<'a>>,
		upper: &mut Vec<StructlessLanguage<'a>>,
		scope: Cow<'a, str>,
		rename_map: &mut HashMap<String, String>,
		state: &mut State<'a, '_, '_, '_, '_>,
	) -> Result<Vec<StructlessLanguage<'a>>> {
		let mut new_elements = Vec::new();
		let mut structs: HashMap<Cow<str>, Cow<str>> = HashMap::new();
		for mut element in elements.into_iter() {
			for (from, to) in rename_map.iter() {
				element.rename(from, to);
			}
			per_element(
				element,
				upper,
				scope.clone(),
				rename_map,
				state,
				&mut new_elements,
				&mut structs,
			)?;
		}
		Ok(new_elements)
	}
}

#[derive(Debug, PartialEq)]
pub struct State<'a, 'b, 'c, 'd, 'e> {
	pub struct_types: &'b mut HashMap<Cow<'a, str>, Vec<NativeVariable<'a>>>,
	pub structs_and_struct_pointers: &'c mut HashMap<Cow<'a, str>, &'a str>,
	pub functions: &'d mut HashMap<Cow<'a, str>, Vec<Variable<'a>>>,
	pub symbols: &'e mut HashMap<Cow<'a, str>, NativeType>,
}

fn per_element<'a>(
	element: LanguageElement<'a>,
	upper: &mut Vec<StructlessLanguage<'a>>,
	scope: Cow<'a, str>,
	rename_map: &mut HashMap<String, String>,
	state: &mut State<'a, '_, '_, '_, '_>,
	new_elements: &mut Vec<StructlessLanguage<'a>>,
	structs: &mut HashMap<Cow<'a, str>, Cow<'a, str>>,
) -> Result<()> {
	match element {
		LanguageElement::StructDefinition { name, members } => {
			state.struct_types.insert(name, members);
		}

		LanguageElement::VariableDeclaration {
			typ: Type::Struct(n),
			name,
			is_static: true,
			is_const,
			is_volatile,
		} => {
			let fields = state.struct_types.get(n).ok_or_else(|| {
				error!(
					UndefinedType,
					&LanguageElement::VariableDeclaration {
						typ: Type::Struct(n),
						name: name.clone(),
						is_static: true,
						is_const,
						is_volatile,
					}
				)
			})?;
			let new_name: Cow<'a, str> = Cow::Owned(format!("{}::{}", scope, name));
			rename_map.insert(name.to_string(), new_name.to_string());
			state.symbols.insert(new_name.clone(), NativeType::Void);
			state
				.structs_and_struct_pointers
				.insert(new_name.clone(), n);
			upper.push(StructlessLanguage::VariableLabelTag {
				name: new_name,
				is_static: true,
				is_const,
				is_volatile,
			});
			for field in fields {
				let new_field_name = format!("{}::{}::{}", &scope, &name, field.name);
				rename_map.insert(format!("{}::{}", &name, field.name), new_field_name.clone());
				state
					.symbols
					.insert(Cow::Owned(new_field_name.clone()), field.typ.clone());
				upper.push(StructlessLanguage::VariableDeclaration {
					name: Cow::Owned(new_field_name),
					typ: field.typ.clone(),
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
			let fields = if let Some(fields) = state.struct_types.get(n) {
				fields
			} else {
				return Err(error!(
					UndefinedType,
					&LanguageElement::VariableDeclaration {
						typ: Type::Struct(n),
						name,
						is_static: false,
						is_const,
						is_volatile,
					}
				));
			};
			state.structs_and_struct_pointers.insert(name.clone(), n);
			new_elements.push(StructlessLanguage::VariableLabelTag {
				name: name.clone(),
				is_static: false,
				is_const,
				is_volatile,
			});
			for field in fields {
				let new_field_name = helper::merge_name_and_field(&name, &field.name);
				state
					.symbols
					.insert(new_field_name.clone(), field.typ.clone());
				new_elements.push(StructlessLanguage::VariableDeclaration {
					name: new_field_name,
					typ: field.typ.clone(),
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
			let new_name = format!("{}::{}", &scope, &name);
			rename_map.insert(name.to_string(), new_name.clone());
			state
				.symbols
				.insert(new_name.clone().into(), t.as_ref().into());
			let t = t.as_ref();
			upper.push(StructlessLanguage::VariableLabelTag {
				name: Cow::Owned(new_name),
				is_static: true,
				is_const,
				is_volatile,
			});
			let converted_t = NativeType::from(t);
			for idx in 0..len {
				let new_element_name = format!("{}::{}[{}]", &scope, &name, idx);
				rename_map.insert(format!("{}[{}]", &name, idx), new_element_name.clone());
				state
					.symbols
					.insert(Cow::Owned(new_element_name.clone()), converted_t.clone());
				upper.push(StructlessLanguage::VariableDeclaration {
					typ: converted_t.clone(),
					name: Cow::Owned(new_element_name),
					is_static: true,
					is_const,
					is_volatile,
				});
			}
		}

		LanguageElement::VariableDeclaration {
			typ: Type::Arr(t, len),
			name,
			is_static: false,
			is_const,
			is_volatile,
		} => {
			let converted_t = NativeType::from(t.as_ref());
			state.symbols.insert(name.clone(), t.as_ref().into());
			for idx in (0..len).rev() {
				let new_element_name = format!("{}[{}]", &name, idx);
				state
					.symbols
					.insert(Cow::Owned(new_element_name.clone()), converted_t.clone());
				new_elements.push(StructlessLanguage::VariableDeclaration {
					typ: converted_t.clone(),
					name: Cow::Owned(new_element_name),
					is_static: false,
					is_const,
					is_volatile,
				});
			}
			let target_name = Cow::Owned(format!("{}[0]", &name));
			new_elements.push(StructlessLanguage::VariableDeclarationAssignment {
				typ: NativeType::ptr(converted_t),
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
			let new_name = format!("{}::{}", scope, name);
			rename_map.insert(name.to_string(), new_name.clone());
			state
				.symbols
				.insert(Cow::Owned(new_name.clone()), (&typ).into());
			upper.push(StructlessLanguage::VariableDeclaration {
				typ: typ.into(),
				name: Cow::Owned(new_name),
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
			state.symbols.insert(name.clone(), (&typ).into());
			new_elements.push(StructlessLanguage::VariableDeclaration {
				typ: typ.into(),
				name,
				is_static: false,
				is_const,
				is_volatile,
			});
		}

		LanguageElement::VariableAssignment { name, value } => {
			if let Some(struct_type) = state.structs_and_struct_pointers.get(&name) {
				let struct_type: &str = struct_type;
				let struct_fields = if let Some(struct_fields) = state.struct_types.get(struct_type)
				{
					struct_fields
				} else {
					return Err(error!(
						BadStructName,
						&LanguageElement::VariableAssignment { name, value }
					));
				};
				let rhs_name = if let StatementElement::Var(name) = value {
					name
				} else {
					return Err(error!(
						InternalNotStruct,
						&LanguageElement::VariableAssignment { name, value }
					));
				};
				for NativeVariable { name: field, .. } in struct_fields.iter() {
					let lhs = helper::merge_name_and_field(&name, field);
					let rhs = helper::merge_name_and_field(&rhs_name, field);
					new_elements.push(StructlessLanguage::VariableAssignment {
						name: lhs,
						value: StructlessStatement::Var(rhs),
					})
				}
			} else {
				new_elements.push(StructlessLanguage::VariableAssignment {
					name,
					value: StructlessStatement::from(&value, state)?,
				})
			}
		}

		LanguageElement::StructAssignment { name, value } => {
			let name: &str = &name;
			let struct_type = if let Some(struct_type) = structs.get(name) {
				struct_type
			} else {
				return Err(error!(
					UndefinedVariable,
					&LanguageElement::StructAssignment {
						name: Cow::Borrowed(name),
						value
					}
				));
			};
			let fields = if let Some(fields) = state.struct_types.get(struct_type) {
				fields
			} else {
				return Err(error!(
					UndefinedType,
					&LanguageElement::StructAssignment {
						name: Cow::Borrowed(name),
						value
					}
				));
			};
			for (val, field) in value.into_iter().zip(fields.iter()) {
				new_elements.push(StructlessLanguage::VariableAssignment {
					name: helper::merge_name_and_field(name, &field.name),
					value: StructlessStatement::from(&val, state)?,
				});
			}
		}

		LanguageElement::VariableDeclarationAssignment {
			typ: Type::Arr(t, len),
			name,
			value,
			is_static: true,
			is_const,
			is_volatile,
		} => {
			let new_name = format!("{}::{}", scope, name);
			rename_map.insert(name.to_string(), new_name.clone());
			state
				.symbols
				.insert(Cow::Owned(new_name.clone()), t.as_ref().into());
			let mut value = StructlessStatement::from(&value, state)?;
			if let StructlessStatement::Array(arr) = &mut value {
				while arr.len() < len as usize {
					arr.push(StructlessStatement::Num(Number::ZERO));
				}
				arr.truncate(len as usize);
			}
			if let Some(n) = t.as_ref().get_struct_recursive() {
				state.structs_and_struct_pointers.insert(name.clone(), n);
			}
			let typ = NativeType::ptr(t.as_ref().into());
			upper.push(StructlessLanguage::VariableDeclarationAssignment {
				typ,
				name: Cow::Owned(new_name),
				value,
				is_static: true,
				is_const,
				is_volatile,
			});
		}

		LanguageElement::VariableDeclarationAssignment {
			typ: Type::Arr(t, len),
			name,
			value,
			is_static: false,
			is_const,
			is_volatile,
		} => {
			let mut value = StructlessStatement::from(&value, state)?;
			if let StructlessStatement::Array(arr) = &mut value {
				while arr.len() < len as usize {
					arr.push(StructlessStatement::Num(Number::ZERO));
				}
				arr.truncate(len as usize);
			}
			if let Some(n) = t.as_ref().get_struct_recursive() {
				state.structs_and_struct_pointers.insert(name.clone(), n);
			}
			let typ = NativeType::ptr(t.as_ref().into());
			let alloc_name: Cow<'a, str> = Cow::Owned(format!("{}_alloc", name));
			state.symbols.insert(alloc_name.clone(), t.as_ref().into());
			state.symbols.insert(name.clone(), typ.clone());
			new_elements.push(StructlessLanguage::VariableDeclarationAssignment {
				typ: t.as_ref().into(),
				name: alloc_name.clone(),
				value,
				is_static: false,
				is_const,
				is_volatile,
			});
			new_elements.push(StructlessLanguage::VariableDeclarationAssignment {
				typ,
				name,
				value: StructlessStatement::AdrOf(alloc_name),
				is_static: false,
				is_const,
				is_volatile,
			});
		}

		//This is hit when a struct is declared with a non-literal value
		// (meaning an existing variable or a function call)
		LanguageElement::VariableDeclarationAssignment {
			typ: Type::Struct(struct_type),
			name,
			value,
			is_static: true,
			is_const,
			is_volatile,
		} => {
			let scoped_name = format!("{}::{}", scope, name);
			rename_map.insert(name.to_string(), scoped_name.clone());
			let fields = if let Some(fields) = state.struct_types.get(struct_type) {
				fields
			} else {
				return Err(error!(
					UndefinedType,
					&LanguageElement::VariableDeclarationAssignment {
						typ: Type::Struct(struct_type),
						name,
						value,
						is_static: true,
						is_const,
						is_volatile,
					}
				));
			};
			upper.push(StructlessLanguage::VariableLabelTag {
				name: Cow::Owned(scoped_name),
				is_static: true,
				is_const,
				is_volatile,
			});
			for NativeVariable {
				typ,
				name: field_name,
			} in fields.iter()
			{
				let new_field_name = helper::merge_name_and_field(&name, field_name);
				let scoped_field_name = format!("{}::{}", &scope, &new_field_name);
				rename_map.insert(new_field_name.to_string(), scoped_field_name.clone());
				state.symbols.insert(new_field_name.clone(), typ.clone());
				state
					.symbols
					.insert(Cow::Owned(scoped_field_name), typ.clone());
				new_elements.push(StructlessLanguage::VariableDeclaration {
					typ: typ.clone(),
					name: new_field_name,
					is_static: true,
					is_const,
					is_volatile,
				});
			}
			//Reuse pointer assignment handling instead of repeating code
			per_element(
				LanguageElement::PointerAssignment {
					ptr: StatementElement::Var(name),
					value,
				},
				upper,
				scope,
				rename_map,
				state,
				new_elements,
				structs,
			)?;
		}

		//This is hit when a struct is declared with a non-literal value
		// (meaning an existing variable or a function call)
		LanguageElement::VariableDeclarationAssignment {
			typ: Type::Struct(struct_type),
			name,
			value,
			is_static: false,
			is_const,
			is_volatile,
		} => {
			let fields = if let Some(fields) = state.struct_types.get(struct_type) {
				fields
			} else {
				return Err(error!(
					UndefinedType,
					&LanguageElement::VariableDeclarationAssignment {
						typ: Type::Struct(struct_type),
						name,
						value,
						is_static: false,
						is_const,
						is_volatile,
					}
				));
			};
			new_elements.push(StructlessLanguage::VariableLabelTag {
				name: name.clone(),
				is_static: false,
				is_const,
				is_volatile,
			});
			for NativeVariable {
				typ,
				name: field_name,
			} in fields.iter()
			{
				let new_field_name = helper::merge_name_and_field(&name, field_name);
				state.symbols.insert(new_field_name.clone(), typ.clone());
				new_elements.push(StructlessLanguage::VariableDeclaration {
					typ: typ.clone(),
					name: new_field_name,
					is_static: false,
					is_const,
					is_volatile,
				});
			}
			//Reuse pointer assignment handling instead of repeating code
			per_element(
				LanguageElement::PointerAssignment {
					ptr: StatementElement::Var(name),
					value,
				},
				upper,
				scope,
				rename_map,
				state,
				new_elements,
				structs,
			)?;
		}

		LanguageElement::VariableDeclarationAssignment {
			typ,
			name,
			value,
			is_static: true,
			is_const,
			is_volatile,
		} => {
			let value = StructlessStatement::from(&value, state)?;
			let new_name = format!("{}::{}", scope, name);
			rename_map.insert(name.to_string(), new_name.clone());
			state.symbols.insert(name.clone(), (&typ).into());
			state
				.symbols
				.insert(Cow::Owned(new_name.clone()), (&typ).into());
			upper.push(StructlessLanguage::VariableDeclarationAssignment {
				typ: typ.into(),
				name: Cow::Owned(new_name),
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
			let value = StructlessStatement::from(&value, state)?;
			state.symbols.insert(name.clone(), (&typ).into());
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
			let new_name: Cow<'a, str> = Cow::Owned(format!("{}::{}", scope, name));
			let struct_type = if let Type::Struct(struct_type) = typ {
				struct_type
			} else {
				return Err(error!(
					InternalNotStruct,
					&LanguageElement::StructDeclarationAssignment {
						typ,
						name,
						value,
						is_static: true,
						is_const,
						is_volatile,
					}
				));
			};
			state
				.structs_and_struct_pointers
				.insert(new_name.clone(), struct_type);
			rename_map.insert(name.to_string(), new_name.to_string());
			state.symbols.insert(name.clone(), (&typ).into());
			let fields = if let Some(fields) = state.struct_types.get(struct_type) {
				fields
			} else {
				return Err(error!(
					UndefinedType,
					&LanguageElement::StructDeclarationAssignment {
						typ,
						name,
						value,
						is_static: true,
						is_const,
						is_volatile,
					}
				));
			};
			upper.push(StructlessLanguage::VariableLabelTag {
				name: new_name.clone(),
				is_static: true,
				is_const,
				is_volatile,
			});
			for (val, field) in value.into_iter().zip(fields.iter()) {
				let new_field_name = helper::merge_name_and_field(&new_name, &field.name);
				state
					.symbols
					.insert(new_field_name.clone(), (&field.typ).clone());
				upper.push(StructlessLanguage::VariableDeclarationAssignment {
					name: new_field_name,
					value: StructlessStatement::from(&val, state)?,
					typ: field.typ.clone(),
					is_static: true,
					is_const,
					is_volatile,
				});
			}
			structs.insert(new_name, Cow::Borrowed(struct_type));
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
				struct_type
			} else {
				return Err(error!(
					InternalNotStruct,
					&LanguageElement::StructDeclarationAssignment {
						typ,
						name,
						value,
						is_static: false,
						is_const,
						is_volatile,
					}
				));
			};
			state
				.structs_and_struct_pointers
				.insert(name.clone(), struct_type);
			let fields = if let Some(fields) = state.struct_types.get(struct_type) {
				fields
			} else {
				return Err(error!(
					UndefinedType,
					&LanguageElement::StructDeclarationAssignment {
						typ,
						name,
						value,
						is_static: false,
						is_const,
						is_volatile,
					}
				));
			};
			new_elements.push(StructlessLanguage::VariableLabelTag {
				name: name.clone(),
				is_static: false,
				is_const,
				is_volatile,
			});
			for (val, field) in value.into_iter().zip(fields.iter()) {
				let new_field_name = helper::merge_name_and_field(&name, &field.name);
				state
					.symbols
					.insert(new_field_name.clone(), (&field.typ).clone());
				new_elements.push(StructlessLanguage::VariableDeclarationAssignment {
					name: new_field_name,
					value: StructlessStatement::from(&val, state)?,
					typ: field.typ.clone(),
					is_static: false,
					is_const,
					is_volatile,
				});
			}
			structs.insert(name, Cow::Borrowed(struct_type));
		}

		LanguageElement::PointerAssignment { ptr, value } => {
			let mut default = || {
				new_elements.push(StructlessLanguage::PointerAssignment {
					ptr: StructlessStatement::from(&ptr, state)?,
					value: StructlessStatement::from(&value, state)?,
				});
				Ok(())
			};
			if let StatementElement::Var(name) = &ptr {
				let borrowed_name: &str = name;
				// *out = in; (struct in, out)
				if let Some(struct_type) = state.structs_and_struct_pointers.get(borrowed_name) {
					let struct_type: &str = struct_type;
					let fields = if let Some(fields) = state.struct_types.get(struct_type) {
						fields
					} else {
						return Err(error!(
							UndefinedType,
							&LanguageElement::PointerAssignment { ptr, value }
						));
					};
					let rhs_name = if let StatementElement::Var(name) = &value {
						name
					} else {
						return Err(error!(
							WrongTypeWasNative,
							&LanguageElement::PointerAssignment { ptr, value }
						));
					};
					for (idx, NativeVariable { name: field, .. }) in fields.iter().enumerate() {
						new_elements.push(StructlessLanguage::PointerAssignment {
							ptr: StructlessStatement::BinOp {
								op: BinOp::Add,
								lhs: Box::new(StructlessStatement::Var(name.clone())),
								rhs: Box::new(StructlessStatement::Num((idx as isize).into())),
								signedness: NumberType::Unsigned,
							},
							value: StructlessStatement::Var(helper::merge_name_and_field(
								rhs_name, field,
							)),
						});
					}
				} else {
					default()?;
				}
			} else if let StatementElement::Var(name) = &value {
				let name: &str = name;
				// *(anything) = in; (struct in, out)
				if let Some(_struct_type) = state.structs_and_struct_pointers.get(name) {
					/*let struct_type: &str = struct_type;
					let _fields = state
					.struct_types
					.get(struct_type)
					.ok_or_else(|| error!(UndefinedType, &element))?;*/
					todo!("Todo: Assign struct to any non named pointer");
				} else {
					default()?;
				}
			} else {
				default()?;
			}
		}

		LanguageElement::StructFieldPointerAssignment { name, field, value } => {
			let name_borrowed: &str = &name;
			let struct_type_name = if let Some(struct_type_name) =
				state.structs_and_struct_pointers.get(name_borrowed)
			{
				*struct_type_name
			} else {
				return Err(error!(
					WrongTypeWasNative,
					&LanguageElement::StructFieldPointerAssignment { name, field, value }
				));
			};
			let fields = if let Some(fields) = state.struct_types.get(struct_type_name) {
				fields
			} else {
				return Err(error!(
					UndefinedType,
					&LanguageElement::StructFieldPointerAssignment { name, field, value }
				));
			};
			let idx = if let Some(idx) = fields
				.iter()
				.position(|NativeVariable { name, .. }| name == &field)
			{
				idx
			} else {
				return Err(error!(
					UndefinedStructField,
					&LanguageElement::StructFieldPointerAssignment { name, field, value }
				));
			};
			let signedness = NumberType::from(&fields[idx].typ);
			//Todo: Maybe remove this and tell users to just enable opt?
			// Does it even make a difference? Or will it just generate
			// LDA X, idx
			// regardless?
			let new_ptr = if idx == 0 {
				StructlessStatement::Var(name)
			} else {
				StructlessStatement::BinOp {
					op: BinOp::Add,
					lhs: Box::new(StructlessStatement::Num((idx as isize).into())),
					rhs: Box::new(StructlessStatement::Var(name)),
					signedness,
				}
			};
			new_elements.push(StructlessLanguage::PointerAssignment {
				ptr: new_ptr,
				value: StructlessStatement::from(&value, state)?,
			});
		}

		mut element @ LanguageElement::FunctionDeclaration {
			typ: Type::Struct(_),
			..
		} => {
			convert_return_struct_to_void(&mut element);
			per_element(
				element,
				upper,
				scope,
				rename_map,
				state,
				new_elements,
				structs,
			)?;
		}

		LanguageElement::FunctionDeclaration {
			typ,
			name,
			args,
			block,
		} => {
			state.functions.insert(name.clone(), args.clone());
			let mut new_structs_and_struct_pointers = state.structs_and_struct_pointers.clone();
			for (name, typ) in args
				.iter()
				.filter_map(|v| v.typ.get_struct_recursive().map(|t| (v.name, t)))
			{
				new_structs_and_struct_pointers.insert(Cow::Borrowed(name), typ);
			}
			let new_args = args
				.into_iter()
				.map(|v| v.split_into_native(state.struct_types))
				.collect::<Result<Vec<_>>>()? //There must be a better way
				.into_iter()
				.flat_map(|vec| vec.into_iter())
				.collect::<Vec<_>>();
			let mut new_symbols = state.symbols.clone();
			new_symbols.insert(name.clone(), (&typ).into());
			for NativeVariable { typ, name } in new_args.iter() {
				new_symbols.insert(name.clone(), typ.clone());
			}
			let new_scope = name.clone();
			let mut new_state = State {
				struct_types: state.struct_types,
				structs_and_struct_pointers: &mut new_structs_and_struct_pointers,
				functions: state.functions,
				symbols: &mut new_symbols,
			};
			let block = StructlessLanguage::from_language_elements_internal(
				block,
				upper,
				new_scope,
				rename_map,
				&mut new_state,
			)?;
			new_elements.push(StructlessLanguage::FunctionDeclaration {
				typ: typ.into(),
				name,
				args: new_args,
				block,
			})
		}

		LanguageElement::IfStatement {
			condition,
			then,
			else_then,
		} => {
			let condition = StructlessStatement::from(&condition, state)?;
			let mut inner_then_rename_map = rename_map.clone();
			let mut inner_then_structs_and_struct_pointers =
				state.structs_and_struct_pointers.clone();
			let mut then_state = State {
				struct_types: state.struct_types,
				structs_and_struct_pointers: &mut inner_then_structs_and_struct_pointers,
				functions: state.functions,
				symbols: state.symbols,
			};
			new_elements.push(StructlessLanguage::IfStatement {
				condition,
				then: Box::new(StructlessLanguage::Block {
					block: StructlessLanguage::from_language_elements_internal(
						then,
						upper,
						scope.clone(),
						&mut inner_then_rename_map,
						&mut then_state,
					)?,
					scope_name: Cow::Borrowed("if_then"),
				}),
				else_then: if let Some(else_then) = else_then {
					let mut inner_else_rename_map = rename_map.clone();
					let mut inner_else_structs_and_struct_pointers =
						state.structs_and_struct_pointers.clone();
					let mut else_state = State {
						struct_types: state.struct_types,
						structs_and_struct_pointers: &mut inner_else_structs_and_struct_pointers,
						functions: state.functions,
						symbols: state.symbols,
					};
					Some(Box::new(StructlessLanguage::Block {
						block: StructlessLanguage::from_language_elements_internal(
							else_then,
							upper,
							scope.clone(),
							&mut inner_else_rename_map,
							&mut else_state,
						)?,
						scope_name: Cow::Borrowed("if_else"),
					}))
				} else {
					None
				},
			})
		}

		LanguageElement::For {
			init,
			condition,
			post,
			body,
		} => {
			let mut inner_rename_map = rename_map.clone();
			let mut inner_structs_and_struct_pointers = state.structs_and_struct_pointers.clone();

			let mut new_state = State {
				struct_types: state.struct_types,
				structs_and_struct_pointers: &mut inner_structs_and_struct_pointers,
				functions: state.functions,
				symbols: state.symbols,
			};
			let mut init_block = StructlessLanguage::from_language_elements_internal(
				init,
				upper,
				scope.clone(),
				&mut inner_rename_map,
				&mut new_state,
			)?;
			//Can't be moved to be first so that `new_state` takes variables from `init_block` into consideration
			let condition = StructlessStatement::from(&condition, &new_state)?;
			let mut body = StructlessLanguage::from_language_elements_internal(
				body,
				upper,
				scope.clone(),
				&mut inner_rename_map,
				&mut new_state,
			)?;
			let mut post = StructlessLanguage::from_language_elements_internal(
				post,
				upper,
				scope.clone(),
				&mut inner_rename_map,
				&mut new_state,
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
			let condition = StructlessStatement::from(&condition, state)?;
			let mut inner_rename_map = rename_map.clone();
			let mut inner_structs_and_struct_pointers = state.structs_and_struct_pointers.clone();
			let mut inner_state = State {
				struct_types: state.struct_types,
				structs_and_struct_pointers: &mut inner_structs_and_struct_pointers,
				functions: state.functions,
				symbols: state.symbols,
			};
			new_elements.push(StructlessLanguage::Loop {
				condition,
				body: Box::new(StructlessLanguage::Block {
					block: StructlessLanguage::from_language_elements_internal(
						body,
						upper,
						scope.clone(),
						&mut inner_rename_map,
						&mut inner_state,
					)?,
					scope_name: Cow::Borrowed("while_body"),
				}),
			})
		}

		LanguageElement::Return(ret) => {
			let ret = if let Some(value) = ret {
				Some(StructlessStatement::from(&value, state)?)
			} else {
				None
			};
			new_elements.push(StructlessLanguage::Return(ret))
		}

		LanguageElement::Statement(stat) => new_elements.push(StructlessLanguage::Statement(
			StructlessStatement::from(&stat, state)?,
		)),
	}
	Ok(())
}

fn convert_return_struct_to_void(elem: &mut LanguageElement) {
	if let LanguageElement::FunctionDeclaration {
		typ, args, block, ..
	} = elem
	{
		let new_arg = Variable {
			name: "__ret__",
			typ: Type::ptr(typ.clone()),
		};
		*typ = Type::Void;
		args.push(new_arg);

		replace_returns(block);
	}
}

fn replace_returns(elements: &mut Vec<LanguageElement>) {
	for idx in 0..elements.len() {
		let element = &mut elements[idx];
		match element {
			LanguageElement::IfStatement {
				then, else_then, ..
			} => {
				replace_returns(then);
				if let Some(else_then) = else_then {
					replace_returns(else_then);
				}
			}
			LanguageElement::For { body, .. } | LanguageElement::While { body, .. } => {
				replace_returns(body)
			}
			LanguageElement::Return(statement @ Some(_)) => {
				elements[idx] = LanguageElement::PointerAssignment {
					ptr: StatementElement::Var(Cow::Borrowed("__ret__")),
					value: statement.take().expect("It's already matches as a Some(_)"),
				};
				elements.insert(idx + 1, LanguageElement::Return(None));
			}
			_ => {}
		}
	}
}

impl fmt::Display for StructlessLanguage<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let static_const_volatile = |s, c, v, f: &mut fmt::Formatter<'_>| {
			if s {
				write!(f, "static ")?;
			}
			if c {
				write!(f, "const ")?;
			}
			if v {
				write!(f, "volatile ")?;
			}
			Ok(())
		};
		match self {
			StructlessLanguage::VariableDeclaration {
				typ,
				name,
				is_static,
				is_const,
				is_volatile,
			} => {
				static_const_volatile(*is_static, *is_const, *is_volatile, f)?;
				write!(f, "{} {};", typ, name)
			}
			StructlessLanguage::VariableAssignment { name, value } => {
				write!(f, "{} = {};", name, value)
			}
			StructlessLanguage::VariableDeclarationAssignment {
				typ,
				name,
				value,
				is_static,
				is_const,
				is_volatile,
			} => {
				static_const_volatile(*is_static, *is_const, *is_volatile, f)?;
				write!(f, "{} {} = {}", typ, name, value)
			}
			StructlessLanguage::PointerAssignment { ptr, value } => {
				write!(f, "*{} = {};", ptr, value)
			}
			StructlessLanguage::FunctionDeclaration {
				typ,
				name,
				args,
				block,
			} => {
				write!(f, "{} {}(", typ, name)?;
				helper::write_token_slice(args, f, ", ")?;
				writeln!(f, "){{")?;
				helper::write_token_slice(block, f, "\n")?;
				write!(f, "\n}}")
			}
			StructlessLanguage::IfStatement {
				condition,
				then,
				else_then,
			} => {
				write!(f, "if ({}) {{\n{}\n}}", condition, then)?;
				if let Some(else_then) = else_then {
					write!(f, " else {{\n{}\n}}", else_then)?;
				}
				Ok(())
			}
			StructlessLanguage::Loop { condition, body } => {
				write!(f, "while ({}) {{\n{}\n}}", condition, body)
			}
			StructlessLanguage::Return(statement) => {
				write!(f, "return")?;
				if let Some(statement) = statement {
					write!(f, " {}", statement)?;
				}
				write!(f, ";")
			}
			StructlessLanguage::Statement(s) => write!(f, "{};", s),
			StructlessLanguage::VariableLabelTag {
				name,
				is_static,
				is_const,
				is_volatile,
			} => {
				write!(f, "VARIBABLE_TAG_FOR_STRUCT_NAMES (")?;
				static_const_volatile(*is_static, *is_const, *is_volatile, f)?;
				write!(f, "{})", name)
			}
			StructlessLanguage::Block { block, scope_name } => {
				write!(f, "'{}: {{", scope_name)?;
				helper::write_token_slice(block, f, "")
			}
		}
	}
}

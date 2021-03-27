use crate::*;
use std::{borrow::Cow, collections::HashMap};

///Internal representation of the program.
/// Can represent any language pattern considered valid.
/// (language patterns are complete lines. Right hand side
/// statements are `StatementElement`s)
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum LanguageElement<'a> {
	VariableDeclaration {
		typ: Type<'a>,
		name: Cow<'a, str>,
		is_static: bool,
	},
	VariableAssignment {
		name: Cow<'a, str>,
		value: StatementElement<'a>,
	},
	StructAssignment {
		name: Cow<'a, str>,
		value: Vec<StatementElement<'a>>,
	},
	VariableDeclarationAssignment {
		typ: Type<'a>,
		name: Cow<'a, str>,
		value: StatementElement<'a>,
		is_static: bool,
	},
	StructDeclarationAssignment {
		typ: Type<'a>,
		name: Cow<'a, str>,
		value: Vec<StatementElement<'a>>,
		is_static: bool,
	},
	PointerAssignment {
		ptr: StatementElement<'a>,
		value: StatementElement<'a>,
	},
	StructFieldPointerAssignment {
		name: Cow<'a, str>,
		field: Cow<'a, str>,
		value: StatementElement<'a>,
	},
	FunctionDeclaration {
		typ: Type<'a>,
		name: Cow<'a, str>,
		args: Vec<Variable<'a>>,
		block: Block<'a>,
	},
	StructDefinition {
		name: Cow<'a, str>,
		members: Vec<Variable<'a>>,
	},
	IfStatement {
		condition: StatementElement<'a>,
		then: Block<'a>,
		else_then: Option<Block<'a>>,
	},
	For {
		init: Block<'a>,
		condition: StatementElement<'a>,
		post: Block<'a>,
		body: Block<'a>,
	},
	While {
		condition: StatementElement<'a>,
		body: Block<'a>,
	},
	Return(Option<StatementElement<'a>>),
	Statement(StatementElement<'a>),
}

impl<'a> LanguageElement<'a> {
	pub(crate) fn make_static(mut self) -> Result<Self, ParseError> {
		match &mut self {
			LanguageElement::VariableDeclaration {
				typ: _,
				name: _,
				is_static,
			} => {
				*is_static = true;
			}
			LanguageElement::VariableDeclarationAssignment {
				typ: _,
				name: _,
				value: _,
				is_static,
			} => {
				*is_static = true;
			}
			LanguageElement::StructDeclarationAssignment {
				typ: _,
				name: _,
				value: _,
				is_static,
			} => {
				*is_static = true;
			}
			_ => {
				return Err(ParseError(
					line!(),
					"Internal error: cannot make element static",
				))
			}
		}
		Ok(self)
	}
}

///Internal representation of the program.
/// Can represent any language pattern considered valid.
/// (language patterns are complete lines. Right hand side
/// statements are `StatementElement`s)
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum LanguageElementStructless<'a> {
	VariableDeclaration {
		typ: NativeType,
		name: Cow<'a, str>,
		is_static: bool,
	},
	VariableAssignment {
		name: Cow<'a, str>,
		value: StatementElement<'a>,
	},
	VariableDeclarationAssignment {
		typ: NativeType,
		name: Cow<'a, str>,
		value: StatementElement<'a>,
		is_static: bool,
	},
	PointerAssignment {
		ptr: StatementElement<'a>,
		value: StatementElement<'a>,
	},
	FunctionDeclaration {
		typ: NativeType,
		name: Cow<'a, str>,
		args: Vec<Variable<'a>>,
		block: BlockStructless<'a>,
	},
	IfStatement {
		condition: StatementElement<'a>,
		then: BlockStructless<'a>,
		else_then: Option<BlockStructless<'a>>,
	},
	For {
		init: BlockStructless<'a>,
		condition: StatementElement<'a>,
		post: BlockStructless<'a>,
		body: BlockStructless<'a>,
	},
	While {
		condition: StatementElement<'a>,
		body: BlockStructless<'a>,
	},
	Return(Option<StatementElement<'a>>),
	Statement(StatementElement<'a>),
}

impl<'a> LanguageElementStructless<'a> {
	//refactor into doing it one by one to an option and then map over/collect?
	// but not until StatementElements have been unstructified
	///Destructures structs into normal language elements
	pub(crate) fn from_language_elements(
		elements: Vec<LanguageElement<'a>>,
	) -> Result<Vec<LanguageElementStructless<'a>>, ParseError> {
		let mut struct_types = HashMap::new();
		let mut structs_and_struct_pointers = HashMap::new();
		LanguageElementStructless::from_language_elements_internal(
			elements,
			&mut struct_types,
			&mut structs_and_struct_pointers,
		)
	}

	fn from_language_elements_internal(
		elements: Vec<LanguageElement<'a>>,
		struct_types: &mut HashMap<Cow<'a, str>, Vec<Variable<'a>>>,
		structs_and_struct_pointers: &mut HashMap<Cow<'a, str>, &'a str>,
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
						structs_and_struct_pointers.insert(name.clone(), n);
					}
					new_elements.push(LanguageElementStructless::VariableDeclaration {
						typ: typ.into(),
						name,
						is_static,
					})
				}
				LanguageElement::VariableAssignment { name, value } => {
					new_elements.push(LanguageElementStructless::VariableAssignment { name, value })
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
							value: val,
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
						value,
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
						dbg!(struct_types);
						return Err(ParseError(line!(), "Undefined struct type"));
					};
					for (val, field) in value.into_iter().zip(fields.iter()) {
						new_elements.push(
							LanguageElementStructless::VariableDeclarationAssignment {
								name: Cow::Owned(name.to_string() + "::" + field.name),
								value: val,
								typ: field.typ.clone(),
								is_static,
							},
						);
					}
					structs.insert(name, Cow::Borrowed(struct_type));
				}
				LanguageElement::PointerAssignment { ptr, value } => {
					new_elements.push(LanguageElementStructless::PointerAssignment { ptr, value })
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
						StatementElement::Var(name)
					} else {
						StatementElement::Add {
							lhs: Box::new(StatementElement::Num(idx as isize)),
							rhs: Box::new(StatementElement::Var(name)),
						}
					};
					new_elements.push(LanguageElementStructless::PointerAssignment {
						ptr: new_ptr,
						value,
					});
				}
				LanguageElement::FunctionDeclaration {
					typ,
					name,
					args,
					block,
				} => new_elements.push(LanguageElementStructless::FunctionDeclaration {
					typ: typ.into(),
					name,
					args,
					block: LanguageElementStructless::from_language_elements_internal(
						block,
						struct_types,
						structs_and_struct_pointers,
					)?,
				}),
				LanguageElement::IfStatement {
					condition,
					then,
					else_then,
				} => new_elements.push(LanguageElementStructless::IfStatement {
					condition,
					then: LanguageElementStructless::from_language_elements_internal(
						then,
						struct_types,
						structs_and_struct_pointers,
					)?,
					//no, clippy, this can't be replaced with a Option::map
					else_then: if let Some(else_then) = else_then {
						Some(LanguageElementStructless::from_language_elements_internal(
							else_then,
							struct_types,
							structs_and_struct_pointers,
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
					)?,
					condition,
					post: LanguageElementStructless::from_language_elements_internal(
						post,
						struct_types,
						structs_and_struct_pointers,
					)?,
					body: LanguageElementStructless::from_language_elements_internal(
						body,
						struct_types,
						structs_and_struct_pointers,
					)?,
				}),
				LanguageElement::While { condition, body } => {
					new_elements.push(LanguageElementStructless::While {
						condition,
						body: LanguageElementStructless::from_language_elements_internal(
							body,
							struct_types,
							structs_and_struct_pointers,
						)?,
					})
				}
				LanguageElement::Return(ret) => {
					new_elements.push(LanguageElementStructless::Return(ret))
				}
				LanguageElement::Statement(stat) => {
					new_elements.push(LanguageElementStructless::Statement(stat))
				}
			}
		}
		Ok(new_elements)
	}
}

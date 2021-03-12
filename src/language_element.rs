use crate::*;
use std::borrow::Cow;

///Internal representation of the program.
/// Can represent any language pattern considered valid.
/// (language patterns are complete lines. Right hand side
/// statements are `StatementElement`s)
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum LanguageElement<'a> {
	VariableDeclaration {
		typ: Type,
		name: Cow<'a, str>,
		is_static: bool,
	},
	StructDeclaration {
		typ: Cow<'a, str>,
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
		typ: Type,
		name: Cow<'a, str>,
		value: StatementElement<'a>,
		is_static: bool,
	},
	StructDeclarationAssignment {
		typ: Cow<'a, str>,
		name: Cow<'a, str>,
		value: Vec<StatementElement<'a>>,
		is_static: bool,
	},
	PointerAssignment {
		ptr: StatementElement<'a>,
		value: StatementElement<'a>,
	},
	FunctionDeclaration {
		typ: Type,
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

///Internal representation of the program.
/// Can represent any language pattern considered valid.
/// (language patterns are complete lines. Right hand side
/// statements are `StatementElement`s)
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum LanguageElementStructless<'a> {
	VariableDeclaration {
		typ: Type,
		name: Cow<'a, str>,
		is_static: bool,
	},
	VariableAssignment {
		name: Cow<'a, str>,
		value: StatementElement<'a>,
	},
	VariableDeclarationAssignment {
		typ: Type,
		name: Cow<'a, str>,
		value: StatementElement<'a>,
		is_static: bool,
	},
	PointerAssignment {
		ptr: StatementElement<'a>,
		value: StatementElement<'a>,
	},
	FunctionDeclaration {
		typ: Type,
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
		let mut struct_types = Vec::new();
		let mut new_elements = Vec::new();
		for element in elements {
			match element {
				LanguageElement::StructDefinition { name, members } => {
					struct_types.push(types::Struct { name, members })
				}

				LanguageElement::VariableDeclaration {
					typ,
					name,
					is_static,
				} => new_elements.push(LanguageElementStructless::VariableDeclaration {
					typ,
					name,
					is_static,
				}),
				LanguageElement::StructDeclaration {
					typ,
					name,
					is_static,
				} => panic!("Struct"),
				LanguageElement::VariableAssignment { name, value } => {
					new_elements.push(LanguageElementStructless::VariableAssignment { name, value })
				}
				LanguageElement::StructAssignment { name, value } => panic!("struct"),
				LanguageElement::VariableDeclarationAssignment {
					typ,
					name,
					value,
					is_static,
				} => new_elements.push(LanguageElementStructless::VariableDeclarationAssignment {
					typ,
					name,
					value,
					is_static,
				}),
				LanguageElement::StructDeclarationAssignment {
					typ,
					name,
					value,
					is_static,
				} => panic!("struct"),
				LanguageElement::PointerAssignment { ptr, value } => {
					new_elements.push(LanguageElementStructless::PointerAssignment { ptr, value })
				}
				LanguageElement::FunctionDeclaration {
					typ,
					name,
					args,
					block,
				} => new_elements.push(LanguageElementStructless::FunctionDeclaration {
					typ,
					name,
					args,
					block: LanguageElementStructless::from_language_elements(block)?,
				}),
				LanguageElement::IfStatement {
					condition,
					then,
					else_then,
				} => new_elements.push(LanguageElementStructless::IfStatement {
					condition,
					then: LanguageElementStructless::from_language_elements(then)?,
					//no, clippy, this can't be replaced with a Option::map
					else_then: if let Some(else_then) = else_then {
						Some(LanguageElementStructless::from_language_elements(
							else_then,
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
					init: LanguageElementStructless::from_language_elements(init)?,
					condition,
					post: LanguageElementStructless::from_language_elements(post)?,
					body: LanguageElementStructless::from_language_elements(body)?,
				}),
				LanguageElement::While { condition, body } => {
					new_elements.push(LanguageElementStructless::While {
						condition,
						body: LanguageElementStructless::from_language_elements(body)?,
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

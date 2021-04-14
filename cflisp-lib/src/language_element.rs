use crate::*;
use std::borrow::Cow;

///Internal representation of the program.
/// Can represent any language pattern considered valid.
/// (language patterns are complete lines. Right hand side
/// statements are `StatementElement`s)
#[derive(Debug, Clone, PartialEq)]
pub enum LanguageElement<'a> {
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
			LanguageElement::VariableDeclaration { is_static, .. } => {
				*is_static = true;
			}
			LanguageElement::VariableDeclarationAssignment { is_static, .. } => {
				*is_static = true;
			}
			LanguageElement::StructDeclarationAssignment { is_static, .. } => {
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

use crate::*;
use statement_element::StatementElement;
use types::{Block, Type};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum LanguageElement<'a> {
	VariableDeclaration {
		typ: Type,
		name: &'a str,
	},
	VariableAssignment {
		name: &'a str,
		value: StatementElement<'a>,
	},
	VariableDecarationAssignment {
		typ: Type,
		name: &'a str,
		value: StatementElement<'a>,
	},
	PointerAssignment {
		ptr: StatementElement<'a>,
		value: StatementElement<'a>,
	},
	FunctionDeclaration {
		typ: Type,
		name: &'a str,
		args: Vec<Variable<'a>>,
		block: Block<'a>,
	},
	IfStatement {
		condition: StatementElement<'a>,
		then: Block<'a>,
		else_then: Option<Block<'a>>,
	},
	///`init` must be a `VariableDeclarationAssignment`,
	///
	/// `after` must only contain `VariableAssignment`
	For {
		init: Box<LanguageElement<'a>>,
		condition: StatementElement<'a>,
		after: Block<'a>,
		body: Block<'a>,
	},
	While {
		condition: StatementElement<'a>,
		body: Block<'a>,
	},
}

use crate::*;

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
	VariableDeclarationAssignment {
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
	Return(Option<StatementElement<'a>>),
	Statement(StatementElement<'a>),
}

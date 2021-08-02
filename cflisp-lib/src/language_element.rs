use std::{borrow::Cow, fmt};

use crate::*;

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
		is_const: bool,
		is_volatile: bool,
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
		is_const: bool,
		is_volatile: bool,
	},
	StructDeclarationAssignment {
		typ: Type<'a>,
		name: Cow<'a, str>,
		value: Vec<StatementElement<'a>>,
		is_static: bool,
		is_const: bool,
		is_volatile: bool,
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
		members: Vec<NativeVariable<'a>>,
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
	//If we are renaming, it's guaranteed that the new name is an Owned variant.
	// We therefore don't need to care about keeping the &'a str reference alive
	// and have to allocate new Strings everywhere
	pub(crate) fn rename(&mut self, from: &str, to: &str) {
		match self {
			LanguageElement::VariableDeclaration { name, .. } => {
				if name == from {
					*name = Cow::Owned(to.to_string());
				}
			}
			LanguageElement::VariableAssignment {
				name,
				value: statement,
				..
			}
			| LanguageElement::VariableDeclarationAssignment {
				name,
				value: statement,
				..
			}
			| LanguageElement::StructFieldPointerAssignment {
				name,
				value: statement,
				..
			} => {
				if name == from {
					*name = Cow::Owned(to.to_string());
				}
				statement.rename(from, to);
			}
			LanguageElement::FunctionDeclaration {
				name,
				block: statements,
				..
			} => {
				if name == from {
					*name = Cow::Owned(to.to_string());
				}
				rename_many(statements, from, to);
			}
			LanguageElement::StructAssignment {
				name,
				value: statements,
				..
			}
			| LanguageElement::StructDeclarationAssignment {
				name,
				value: statements,
				..
			} => {
				if name == from {
					*name = Cow::Owned(to.to_string());
				}
				statements
					.iter_mut()
					.for_each(|statement| statement.rename(from, to));
			}
			LanguageElement::PointerAssignment { ptr, value } => {
				ptr.rename(from, to);
				value.rename(from, to);
			}
			LanguageElement::StructDefinition { name, .. } => {
				if name == from {
					*name = Cow::Owned(to.to_string());
				}
			}
			LanguageElement::IfStatement {
				condition,
				then,
				else_then,
			} => {
				condition.rename(from, to);
				rename_many(then, from, to);
				if let Some(else_then) = else_then {
					rename_many(else_then, from, to);
				}
			}
			LanguageElement::For {
				init,
				condition,
				post,
				body,
			} => {
				condition.rename(from, to);
				rename_many(init, from, to);
				rename_many(post, from, to);
				rename_many(body, from, to);
			}
			LanguageElement::While { condition, body } => {
				condition.rename(from, to);
				rename_many(body, from, to);
			}
			LanguageElement::Return(Some(statement)) | LanguageElement::Statement(statement) => {
				statement.rename(from, to);
			}
			LanguageElement::Return(None) => {}
		}
	}
}

impl fmt::Display for LanguageElement<'_> {
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
			LanguageElement::VariableDeclaration {
				typ,
				name,
				is_static,
				is_const,
				is_volatile,
			} => {
				static_const_volatile(*is_static, *is_const, *is_volatile, f)?;
				write!(f, "{} {};", typ, name)
			}
			LanguageElement::VariableAssignment { name, value } => {
				write!(f, "{} = {};", name, value)
			}
			LanguageElement::StructAssignment { name, value } => {
				write!(f, "{} = {{", name)?;
				for field in value.iter() {
					write!(f, "{}, ", field)?;
				}
				write!(f, "}};")
			}
			LanguageElement::VariableDeclarationAssignment {
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
			LanguageElement::StructDeclarationAssignment {
				typ,
				name,
				value,
				is_static,
				is_const,
				is_volatile,
			} => {
				static_const_volatile(*is_static, *is_const, *is_volatile, f)?;
				write!(f, "{} {} = {{", typ, name)?;
				for field in value.iter() {
					write!(f, "{}, ", field)?;
				}
				write!(f, "}};")
			}
			LanguageElement::PointerAssignment { ptr, value } => write!(f, "*{} = {};", ptr, value),
			LanguageElement::StructFieldPointerAssignment { name, field, value } => {
				write!(f, "{}->{} = {};", name, field, value)
			}
			LanguageElement::FunctionDeclaration {
				typ,
				name,
				args,
				block,
			} => {
				write!(f, "{} {}(", typ, name)?;
				for Variable { name, typ } in args.iter() {
					write!(f, "{} {},", typ, name)?;
				}
				writeln!(f, ") {{")?;
				for line in block.iter() {
					writeln!(f, "{}", line)?;
				}
				write!(f, "}}")
			}
			LanguageElement::StructDefinition { name, members } => {
				writeln!(f, "struct {} {{", name)?;
				for NativeVariable { typ, name } in members.iter() {
					writeln!(f, "{} {};", typ, name)?;
				}
				write!(f, "}};")
			}
			LanguageElement::IfStatement {
				condition,
				then,
				else_then,
			} => {
				writeln!(f, "if ({}) {{", condition)?;
				for line in then.iter() {
					writeln!(f, "{}", line)?;
				}
				write!(f, "}}")?;
				if let Some(else_then) = else_then {
					writeln!(f, " else {{")?;
					for line in else_then.iter() {
						writeln!(f, "{}", line)?;
					}
					write!(f, "}}")
				} else {
					Ok(())
				}
			}
			LanguageElement::For {
				init,
				condition,
				post,
				body,
			} => {
				//todo: investigate if for loops really can contain multiple init and post statements
				writeln!(f, "for ({} {}; {}) {{", init[0], condition, post[0])?;
				for line in body.iter() {
					writeln!(f, "{}", line)?;
				}
				write!(f, "}}")
			}
			LanguageElement::While { condition, body } => {
				write!(f, "while ({}) {{", condition)?;
				for line in body {
					writeln!(f, "{}", line)?;
				}
				write!(f, "}}")
			}
			LanguageElement::Return(val) => {
				write!(f, "return")?;
				if let Some(val) = val {
					write!(f, " {}", val)?;
				}
				write!(f, ";")
			}
			LanguageElement::Statement(statement) => write!(f, "{};", statement),
		}
	}
}

fn rename_many(elements: &mut [LanguageElement], from: &str, to: &str) {
	for element in elements {
		element.rename(from, to);
	}
}

pub(crate) fn move_declarations_first(block: &mut Block) {
	let give_value = |element: &LanguageElement| -> usize {
		match element {
			LanguageElement::StructDefinition { .. } => 0,
			LanguageElement::StructDeclarationAssignment {
				is_static: true, ..
			} => 1,
			LanguageElement::VariableDeclaration {
				is_static: true, ..
			} => 1,

			LanguageElement::VariableDeclarationAssignment {
				is_static: true, ..
			} => 1,

			LanguageElement::StructDeclarationAssignment {
				is_static: false, ..
			} => 1,

			LanguageElement::VariableDeclaration {
				is_static: false, ..
			} => 2,

			LanguageElement::VariableDeclarationAssignment {
				is_static: false, ..
			} => 2,

			LanguageElement::FunctionDeclaration { .. } => 3,
			_ => 4,
		}
	};
	block.sort_by_key(give_value);

	for element in block.iter_mut() {
		match element {
			LanguageElement::FunctionDeclaration { block, .. }
			| LanguageElement::While { body: block, .. } => {
				move_declarations_first(block);
			}
			LanguageElement::IfStatement {
				then, else_then, ..
			} => {
				move_declarations_first(then);
				if let Some(else_then) = else_then {
					move_declarations_first(else_then);
				}
			}
			LanguageElement::For {
				init, post, body, ..
			} => {
				move_declarations_first(init);
				move_declarations_first(post);
				move_declarations_first(body);
			}
			_ => {}
		}
	}
}

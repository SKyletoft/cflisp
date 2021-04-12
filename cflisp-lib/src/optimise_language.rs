use crate::*;
use std::borrow::Cow;

pub fn all_optimisations(elements: &mut Vec<LanguageElementStructless>) -> Result<(), ParseError> {
	statement_optimisation(elements)?;
	dead_code_elimination(elements);
	Ok(())
}

fn statement_optimisation(elements: &mut Vec<LanguageElementStructless>) -> Result<(), ParseError> {
	for element in elements {
		match element {
			LanguageElementStructless::VariableDeclaration { .. } => {}
			LanguageElementStructless::VariableAssignment { name: _, value }
			| LanguageElementStructless::VariableDeclarationAssignment {
				typ: _,
				name: _,
				value,
				is_static: _,
			} => {
				optimise_statement::all_optimisations(value)?;
			}
			LanguageElementStructless::PointerAssignment { ptr, value } => {
				optimise_statement::all_optimisations(ptr)?;
				optimise_statement::all_optimisations(value)?;
			}
			LanguageElementStructless::FunctionDeclaration {
				typ: _,
				name: _,
				args: _,
				block,
			} => {
				all_optimisations(block)?;
			}
			LanguageElementStructless::IfStatement {
				condition,
				then,
				else_then,
			} => {
				optimise_statement::all_optimisations(condition)?;
				all_optimisations(then)?;
				if let Some(else_then) = else_then {
					all_optimisations(else_then)?;
				}
			}
			LanguageElementStructless::For {
				init,
				condition,
				post,
				body,
			} => {
				optimise_statement::all_optimisations(condition)?;
				all_optimisations(init)?;
				all_optimisations(post)?;
				all_optimisations(body)?;
			}
			LanguageElementStructless::While { condition, body } => {
				optimise_statement::all_optimisations(condition)?;
				all_optimisations(body)?;
			}
			LanguageElementStructless::Return(_)
			| LanguageElementStructless::Statement(_)
			| LanguageElementStructless::StructDeclaration { .. } => {}
			LanguageElementStructless::Block {
				block,
				scope_name: _,
			} => {
				all_optimisations(block)?;
			}
		}
	}
	Ok(())
}

pub(crate) fn dead_code_elimination(elements: &mut Vec<LanguageElementStructless>) {
	let mut idx = 0;
	while idx < elements.len() {
		match &elements[idx] {
			LanguageElementStructless::IfStatement {
				condition,
				then,
				else_then,
			} => match condition {
				StatementElementStructless::Bool(true) => {
					elements[idx] = LanguageElementStructless::Block {
						block: then.clone(),
						scope_name: Cow::Owned(String::from("if_then")),
					};
				}
				StatementElementStructless::Bool(false) => {
					if let Some(else_then) = else_then {
						elements[idx] = LanguageElementStructless::Block {
							block: else_then.clone(),
							scope_name: Cow::Owned(String::from("if_else")),
						};
					} else {
						elements.remove(idx);
						continue;
					}
				}
				_ => {}
			},
			LanguageElementStructless::For {
				init,
				condition,
				post: _,
				body: _,
			} => {
				if condition == &StatementElementStructless::Bool(false) {
					elements[idx] = LanguageElementStructless::Block {
						block: init.clone(),
						scope_name: Cow::Owned(String::from("for_init")),
					};
					continue;
				}
			}
			LanguageElementStructless::While { condition, body: _ } => {
				if condition == &StatementElementStructless::Bool(false) {
					elements.remove(idx);
					continue;
				}
			}
			LanguageElementStructless::Return(_) => {
				while elements.len() != idx + 1 {
					elements.pop();
				}
			}
			LanguageElementStructless::Statement(_) => {
				elements.remove(idx);
				continue;
			}

			_ => {}
		}
		idx += 1;
	}
}

use crate::*;

pub fn all_optimisations(elements: &mut Vec<LanguageElementStructless>) -> Result<(), ParseError> {
	statement_optimisation(elements)?;
	dead_code_elimination(elements);
	Ok(())
}

fn single_statement_optimisation(
	element: &mut LanguageElementStructless,
) -> Result<(), ParseError> {
	if let LanguageElementStructless::Block {
		block,
		scope_name: _,
	} = element
	{
		all_optimisations(block)?;
	}
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
				single_statement_optimisation(then)?;
				if let Some(else_then) = else_then {
					single_statement_optimisation(else_then)?;
				}
			}
			LanguageElementStructless::Loop { condition, body } => {
				optimise_statement::all_optimisations(condition)?;
				single_statement_optimisation(body)?;
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
				StatementElementStructless::Bool(true) => elements[idx] = *then.clone(),
				StatementElementStructless::Bool(false) => {
					if let Some(else_then) = else_then {
						elements[idx] = *else_then.clone();
					} else {
						elements.remove(idx);
						continue;
					}
				}
				_ => {}
			},
			LanguageElementStructless::Loop { condition, body: _ } => {
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

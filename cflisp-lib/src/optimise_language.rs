use crate::*;

pub fn all_optimisations(elements: &mut Vec<LanguageElementStructless>) -> Result<(), ParseError> {
	statement_optimisation(elements)?;
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
		}
	}
	Ok(())
}

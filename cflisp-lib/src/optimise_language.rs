use crate::*;
use std::{collections::HashSet, slice};

pub fn all_optimisations(elements: &mut Vec<LanguageElementStructless>) -> Result<(), ParseError> {
	statement_optimisation(elements)?;
	dead_code_elimination(elements);
	remove_unused_variables(elements);
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

pub(crate) fn remove_unused_variables(elements: &mut Vec<LanguageElementStructless>) {
	let mut used_variables = HashSet::new();
	find_variables_le(&mut used_variables, elements);
	elements.retain(|elem| match elem {
		LanguageElementStructless::VariableDeclarationAssignment {
			typ: _,
			name,
			value: _,
			is_static: _,
		}
		| LanguageElementStructless::VariableDeclaration {
			typ: _,
			name,
			is_static: _,
		} => {
			let name: &str = name;
			used_variables.contains(name)
		}
		_ => true,
	});
}

fn find_variables_le<'a>(
	vars: &mut HashSet<String>,
	elements: &'a [LanguageElementStructless<'a>],
) {
	for element in elements {
		match element {
			LanguageElementStructless::VariableAssignment { name: _, value }
			| LanguageElementStructless::VariableDeclarationAssignment {
				typ: _,
				name: _,
				value,
				is_static: _,
			} => {
				find_variables_se(vars, value);
			}
			LanguageElementStructless::PointerAssignment { ptr, value } => {
				find_variables_se(vars, ptr);
				find_variables_se(vars, value);
			}
			LanguageElementStructless::IfStatement {
				condition,
				then,
				else_then: Some(else_then),
			} => {
				find_variables_se(vars, condition);
				find_variables_le(vars, slice::from_ref(then));
				find_variables_le(vars, slice::from_ref(else_then));
			}
			LanguageElementStructless::IfStatement {
				condition,
				then: body,
				else_then: _,
			}
			| LanguageElementStructless::Loop { condition, body } => {
				find_variables_se(vars, condition);
				find_variables_le(vars, slice::from_ref(body));
			}
			LanguageElementStructless::Return(Some(statement))
			| LanguageElementStructless::Statement(statement) => {
				find_variables_se(vars, statement);
			}
			LanguageElementStructless::Block {
				block,
				scope_name: _,
			} => {
				find_variables_le(vars, block);
			}

			_ => {}
		}
	}
}

fn find_variables_se<'a>(vars: &mut HashSet<String>, element: &'a StatementElementStructless<'a>) {
	match element {
		StatementElementStructless::Add { lhs, rhs }
		| StatementElementStructless::Sub { lhs, rhs }
		| StatementElementStructless::Mul { lhs, rhs }
		| StatementElementStructless::Div { lhs, rhs }
		| StatementElementStructless::Mod { lhs, rhs }
		| StatementElementStructless::LShift { lhs, rhs }
		| StatementElementStructless::RShift { lhs, rhs }
		| StatementElementStructless::And { lhs, rhs }
		| StatementElementStructless::Or { lhs, rhs }
		| StatementElementStructless::Xor { lhs, rhs }
		| StatementElementStructless::GreaterThan { lhs, rhs }
		| StatementElementStructless::LessThan { lhs, rhs }
		| StatementElementStructless::GreaterThanEqual { lhs, rhs }
		| StatementElementStructless::LessThanEqual { lhs, rhs }
		| StatementElementStructless::Cmp { lhs, rhs }
		| StatementElementStructless::NotCmp { lhs, rhs } => {
			find_variables_se(vars, lhs);
			find_variables_se(vars, rhs);
		}
		StatementElementStructless::Deref(lhs) | StatementElementStructless::Not { lhs } => {
			find_variables_se(vars, lhs);
		}
		StatementElementStructless::FunctionCall { .. }
		| StatementElementStructless::Num(_)
		| StatementElementStructless::Char(_)
		| StatementElementStructless::Bool(_) => {}
		StatementElementStructless::Array(statements) => {
			for statement in statements {
				find_variables_se(vars, statement)
			}
		}
		StatementElementStructless::AdrOf(n) | StatementElementStructless::Var(n) => {
			vars.insert(n.to_string());
		}
	}
}

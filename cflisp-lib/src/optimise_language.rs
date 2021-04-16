use crate::*;
use std::{
	collections::{HashMap, HashSet},
	slice,
};

pub fn all_optimisations(elements: &mut Vec<LanguageElementStructless>) -> Result<(), ParseError> {
	const_prop(elements);
	statement_optimisation(elements)?;
	dead_code_elimination(elements);
	remove_unused_variables(elements);
	Ok(())
}

fn single_statement_optimisation(
	element: &mut LanguageElementStructless,
) -> Result<(), ParseError> {
	if let LanguageElementStructless::Block { block, .. } = element {
		all_optimisations(block)?;
	}
	Ok(())
}

fn statement_optimisation(elements: &mut Vec<LanguageElementStructless>) -> Result<(), ParseError> {
	for element in elements {
		match element {
			LanguageElementStructless::VariableDeclaration { .. } => {}
			LanguageElementStructless::Return(Some(value))
			| LanguageElementStructless::Statement(value)
			| LanguageElementStructless::VariableAssignment { value, .. }
			| LanguageElementStructless::VariableDeclarationAssignment { value, .. } => {
				optimise_statement::all_optimisations(value)?;
			}
			LanguageElementStructless::PointerAssignment { ptr, value } => {
				optimise_statement::all_optimisations(ptr)?;
				optimise_statement::all_optimisations(value)?;
			}
			LanguageElementStructless::FunctionDeclaration { block, .. } => {
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
			LanguageElementStructless::Return(None)
			| LanguageElementStructless::StructDeclaration { .. } => {}
			LanguageElementStructless::Block { block, .. } => {
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
		LanguageElementStructless::VariableAssignment { name, .. }
		| LanguageElementStructless::VariableDeclarationAssignment { name, .. }
		| LanguageElementStructless::VariableDeclaration { name, .. } => {
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
			LanguageElementStructless::VariableDeclaration {
				name,
				is_volatile: true,
				..
			}
			| LanguageElementStructless::VariableDeclarationAssignment {
				name,
				is_volatile: true,
				..
			} => {
				vars.insert(name.to_string());
			}
			LanguageElementStructless::VariableAssignment { value, .. }
			| LanguageElementStructless::VariableDeclarationAssignment { value, .. } => {
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
				..
			}
			| LanguageElementStructless::Loop { condition, body } => {
				find_variables_se(vars, condition);
				find_variables_le(vars, slice::from_ref(body));
			}
			LanguageElementStructless::Return(Some(statement))
			| LanguageElementStructless::Statement(statement) => {
				find_variables_se(vars, statement);
			}
			LanguageElementStructless::Block { block, .. } => {
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
		StatementElementStructless::Deref(lhs) | StatementElementStructless::Not(lhs) => {
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

pub(crate) fn const_prop(elements: &mut [LanguageElementStructless]) {
	let mut constants = HashMap::new();
	const_prop_inner(elements, &mut constants);
}

fn const_prop_inner<'a>(
	elements: &mut [LanguageElementStructless<'a>],
	constants: &mut HashMap<String, StatementElementStructless<'a>>,
) {
	for elem in elements.iter_mut() {
		match elem {
			LanguageElementStructless::VariableDeclarationAssignment {
				name,
				value,
				is_const: true,
				is_volatile: false,
				..
			} => {
				optimise_statement::const_prop(value, constants);
				constants.insert(name.to_string(), value.clone());
			}
			LanguageElementStructless::IfStatement {
				condition,
				then,
				else_then,
			} => {
				optimise_statement::const_prop(condition, constants);
				let mut inner_scope = constants.clone();
				const_prop_inner(slice::from_mut(then), &mut inner_scope);
				if let Some(else_then) = else_then {
					inner_scope = constants.clone();
					const_prop_inner(slice::from_mut(else_then), &mut inner_scope);
				}
			}
			LanguageElementStructless::Return(Some(statement))
			| LanguageElementStructless::Statement(statement) => {
				optimise_statement::const_prop(statement, constants);
			}
			LanguageElementStructless::Loop { condition, body } => {
				optimise_statement::const_prop(condition, constants);
				let mut inner_scope = constants.clone();
				const_prop_inner(slice::from_mut(body), &mut inner_scope);
			}
			LanguageElementStructless::Block { block, .. } => {
				let mut inner_scope = constants.clone();
				const_prop_inner(block, &mut inner_scope);
			}
			_ => {}
		}
	}
}

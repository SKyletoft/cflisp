use std::{
	collections::{HashMap, HashSet},
	slice,
};

use crate::*;

pub fn all_optimisations(elements: &mut Vec<StructlessLanguage>) -> Result<()> {
	const_prop(elements);
	statement_optimisation(elements)?;
	dead_code_elimination(elements);
	remove_unused_variables(elements);
	Ok(())
}

fn single_statement_optimisation(element: &mut StructlessLanguage) -> Result<()> {
	if let StructlessLanguage::Block { block, .. } = element {
		all_optimisations(block)?;
	}
	Ok(())
}

fn statement_optimisation(elements: &mut Vec<StructlessLanguage>) -> Result<()> {
	for element in elements {
		match element {
			StructlessLanguage::VariableDeclaration { .. } => {}
			StructlessLanguage::Return(Some(value))
			| StructlessLanguage::Statement(value)
			| StructlessLanguage::VariableAssignment { value, .. }
			| StructlessLanguage::VariableDeclarationAssignment { value, .. } => {
				optimise_statement::all_optimisations(value)?;
			}
			StructlessLanguage::PointerAssignment { ptr, value } => {
				optimise_statement::all_optimisations(ptr)?;
				optimise_statement::all_optimisations(value)?;
			}
			StructlessLanguage::FunctionDeclaration { block, .. } => {
				all_optimisations(block)?;
			}
			StructlessLanguage::IfStatement {
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
			StructlessLanguage::Loop { condition, body } => {
				optimise_statement::all_optimisations(condition)?;
				single_statement_optimisation(body)?;
			}
			StructlessLanguage::Return(None) | StructlessLanguage::VariableLabelTag { .. } => {}
			StructlessLanguage::Block { block, .. } => {
				all_optimisations(block)?;
			}
		}
	}
	Ok(())
}

pub(crate) fn dead_code_elimination(elements: &mut Vec<StructlessLanguage>) {
	let mut idx = 0;
	while idx < elements.len() {
		match &elements[idx] {
			StructlessLanguage::IfStatement {
				condition,
				then,
				else_then,
			} => match condition {
				StructlessStatement::Bool(true) => elements[idx] = *then.clone(),
				StructlessStatement::Bool(false) => {
					if let Some(else_then) = else_then {
						elements[idx] = *else_then.clone();
					} else {
						elements.remove(idx);
						continue;
					}
				}
				_ => {}
			},
			StructlessLanguage::Loop { condition, body: _ } => {
				if condition == &StructlessStatement::Bool(false) {
					elements.remove(idx);
					continue;
				}
			}
			StructlessLanguage::Return(_) => {
				while elements.len() != idx + 1 {
					elements.pop();
				}
			}
			StructlessLanguage::Statement(_) => {
				elements.remove(idx);
				continue;
			}

			_ => {}
		}
		idx += 1;
	}
}

pub(crate) fn remove_unused_variables(elements: &mut Vec<StructlessLanguage>) {
	let mut used_variables = HashSet::new();
	find_variables_le(&mut used_variables, elements);
	elements.retain(|elem| match elem {
		StructlessLanguage::VariableAssignment { name, .. }
		| StructlessLanguage::VariableDeclarationAssignment {
			name,
			is_static: false,
			..
		}
		| StructlessLanguage::VariableDeclaration {
			name,
			is_static: false,
			..
		} => {
			let name: &str = name;
			used_variables.contains(name)
		}
		_ => true,
	});
}

fn find_variables_le<'a>(vars: &mut HashSet<String>, elements: &'a [StructlessLanguage<'a>]) {
	for element in elements {
		match element {
			StructlessLanguage::VariableDeclaration {
				name,
				is_volatile: true,
				..
			}
			| StructlessLanguage::VariableDeclarationAssignment {
				name,
				is_volatile: true,
				..
			} => {
				vars.insert(name.to_string());
			}

			StructlessLanguage::VariableAssignment { value, .. }
			| StructlessLanguage::VariableDeclarationAssignment { value, .. } => {
				find_variables_se(vars, value);
			}

			StructlessLanguage::PointerAssignment { ptr, value } => {
				find_variables_se(vars, ptr);
				find_variables_se(vars, value);
			}

			StructlessLanguage::IfStatement {
				condition,
				then,
				else_then: Some(else_then),
			} => {
				find_variables_se(vars, condition);
				find_variables_le(vars, slice::from_ref(then));
				find_variables_le(vars, slice::from_ref(else_then));
			}

			StructlessLanguage::IfStatement {
				condition,
				then: body,
				..
			}
			| StructlessLanguage::Loop { condition, body } => {
				find_variables_se(vars, condition);
				find_variables_le(vars, slice::from_ref(body));
			}
			StructlessLanguage::Return(Some(statement))
			| StructlessLanguage::Statement(statement) => {
				find_variables_se(vars, statement);
			}

			StructlessLanguage::FunctionDeclaration { block, .. }
			| StructlessLanguage::Block { block, .. } => {
				find_variables_le(vars, block);
			}

			_ => {}
		}
	}
}

fn find_variables_se<'a>(vars: &mut HashSet<String>, element: &'a StructlessStatement<'a>) {
	match element {
		StructlessStatement::BinOp { lhs, rhs, .. } => {
			find_variables_se(vars, lhs);
			find_variables_se(vars, rhs);
		}
		StructlessStatement::Deref(lhs) | StructlessStatement::Not(lhs) => {
			find_variables_se(vars, lhs);
		}
		StructlessStatement::FunctionCall { .. }
		| StructlessStatement::Num(_)
		| StructlessStatement::Char(_)
		| StructlessStatement::Bool(_) => {}
		StructlessStatement::Array(statements) => {
			for statement in statements {
				find_variables_se(vars, statement)
			}
		}
		StructlessStatement::AdrOf(n) | StructlessStatement::Var(n) => {
			vars.insert(n.to_string());
		}
	}
}

pub(crate) fn const_prop(elements: &mut [StructlessLanguage]) {
	let mut constants = HashMap::new();
	const_prop_inner(elements, &mut constants);
}

fn const_prop_inner<'a>(
	elements: &mut [StructlessLanguage<'a>],
	constants: &mut HashMap<String, StructlessStatement<'a>>,
) {
	for elem in elements.iter_mut() {
		match elem {
			StructlessLanguage::VariableDeclarationAssignment {
				name,
				value,
				is_const: true,
				is_volatile: false,
				..
			} => {
				optimise_statement::const_prop(value, constants);
				constants.insert(name.to_string(), value.clone());
			}
			StructlessLanguage::IfStatement {
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
			StructlessLanguage::Return(Some(statement))
			| StructlessLanguage::Statement(statement)
			| StructlessLanguage::VariableAssignment {
				value: statement, ..
			} => {
				optimise_statement::const_prop(statement, constants);
			}
			StructlessLanguage::Loop { condition, body } => {
				optimise_statement::const_prop(condition, constants);
				let mut inner_scope = constants.clone();
				const_prop_inner(slice::from_mut(body), &mut inner_scope);
			}
			StructlessLanguage::Block { block, .. } => {
				let mut inner_scope = constants.clone();
				const_prop_inner(block, &mut inner_scope);
			}
			StructlessLanguage::PointerAssignment { ptr, value } => {
				optimise_statement::const_prop(ptr, constants);
				optimise_statement::const_prop(value, constants);
			}
			_ => {}
		}
	}
}

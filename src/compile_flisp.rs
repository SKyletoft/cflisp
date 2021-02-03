use std::collections::{BTreeSet, HashMap};

use crate::*;
use flisp_instructions::{Addressing, CommentedInstruction, Instruction};
use statement_element::StatementElement;
use types::{Function, Type, Variable};

pub(crate) fn compile(program: &[LanguageElement]) -> Result<String, CompileError> {
	Err(CompileError(line!(), "Nothing yet"))
}

pub(crate) fn compile_element<'a>(
	element: &'a LanguageElement,
	variables: &mut HashMap<&'a str, (Type, usize)>,
	global_variables: &mut HashMap<&'a str, (Type, usize)>,
	functions: &mut HashMap<&'a str, Type>,
	scope_name: &str,
	mut stack_size: usize,
) -> Result<Vec<CommentedInstruction<'a>>, CompileError> {
	let res = match element {
		LanguageElement::VariableDeclaration { typ, name } => {
			if variables.contains_key(name) {
				return Err(CompileError(line!(), "Name already exists in scope!"));
			}
			variables.insert(*name, (typ.clone(), stack_size));
			stack_size += 1;
			vec![(Instruction::AddToStack, *name)]
		}
		LanguageElement::VariableAssignment { name, value } => {
			todo!()//let statement = compile_statement(value)?;
		}
		LanguageElement::VariableDecarationAssignment { typ, name, value } => todo!(),
		LanguageElement::PointerAssignment { ptr, value } => todo!(),
		LanguageElement::FunctionDeclaration {
			typ,
			name,
			args,
			block,
		} => todo!(),
		LanguageElement::IfStatement {
			condition,
			then,
			else_then,
		} => todo!(),
		LanguageElement::For {
			init,
			condition,
			after,
			body,
		} => todo!(),
		LanguageElement::While { condition, body } => todo!(),
	};
	todo!()
}

pub(crate) fn compile_statement<'a>(
	statements: &[StatementElement],
	variables: &mut HashMap<&'a str, (Type, usize)>,
	global_variables: &HashMap<&'a str, (Type, usize)>,
	mut stack: usize,
) -> Result<Vec<Instruction>, CompileError> {
	let max_depth = statements
		.iter()
		.map(StatementElement::depth)
		.max()
		.unwrap_or(0);
	let mut block = vec![Instruction::AddToStack; max_depth];
	let mut clear_extra = vec![Instruction::RemoveFromStack; max_depth];
	for instructions in statements
		.iter()
		.map(|statement| compile_statement_inner(statement, variables, global_variables, stack))
	{
		block.append(&mut instructions?);
	}
	block.append(&mut clear_extra);
	Ok(block)
}

fn compile_statement_inner<'a>(
	statement: &StatementElement,
	variables: &mut HashMap<&'a str, (Type, usize)>,
	global_variables: &HashMap<&'a str, (Type, usize)>,
	mut stack: usize,
) -> Result<Vec<Instruction>, CompileError> {
	if statement.size() > 20 {
		return Err(CompileError(line!(), "Statement is too complex"));
	}
	let instructions = match statement {
		StatementElement::Add { lhs, rhs }
		| StatementElement::Sub { lhs, rhs }
		| StatementElement::Mul { lhs, rhs }
		| StatementElement::Div { lhs, rhs }
		| StatementElement::Mod { lhs, rhs }
		| StatementElement::LShift { lhs, rhs }
		| StatementElement::RShift { lhs, rhs }
		| StatementElement::And { lhs, rhs }
		| StatementElement::Or { lhs, rhs }
		| StatementElement::Xor { lhs, rhs }
		| StatementElement::GT { lhs, rhs }
		| StatementElement::LT { lhs, rhs }
		| StatementElement::Cmp { lhs, rhs } => {
			let left_depth = lhs.depth();
			let right_depth = rhs.depth();
			let (left, right) = if left_depth >= right_depth {
				(lhs.as_ref(), rhs.as_ref())
			} else {
				(rhs.as_ref(), lhs.as_ref())
			};
			let mut instructions = compile_statement_inner(left, variables, global_variables, stack)?;
			if right.depth() > 1 {
				instructions.push(Instruction::STA(Addressing::SP(stack)));
				stack += 1;
			} else {
				let right_instructions = compile_statement_inner(right, variables, global_variables, stack)?;
				let addressing = match right_instructions.as_slice() {
					[instruction] => instruction.address().ok_or(CompileError(
						line!(),
						"Internal: Invalid right hand instruction?",
					)),
					_ => return Err(CompileError(line!(), "Internal: Depth caluclation failed?")),
				}?;
				let merge = statement.as_a_instruction(addressing)?;
				instructions.push(merge);
			}
			instructions
		}
		StatementElement::FunctionCall {
			name: _,
			parametres: _,
		} => {
			return Err(CompileError(
				line!(),
				"Internal: Function call in statement. Should've been moved out?",
			));
		}
		StatementElement::Var(_) => unimplemented!(),
		StatementElement::VarLabel(_) => unimplemented!(),
		StatementElement::Num(_) => unimplemented!(),
		StatementElement::Char(_) => unimplemented!(),
		StatementElement::Bool(_) => unimplemented!(),
		StatementElement::Array(_) => unimplemented!(),
		StatementElement::Deref(_) => unimplemented!(),
		StatementElement::AdrOf(_) => unimplemented!(),
		StatementElement::Not { lhs: _ } => unimplemented!(),
		StatementElement::AdrOfLabel(_) => unimplemented!(),
	};
	Ok(instructions)
}

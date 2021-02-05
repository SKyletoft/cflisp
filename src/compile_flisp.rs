use std::collections::HashMap;

use crate::*;
use env::var;
use flisp_instructions::{Addressing, CommentedInstruction, Instruction};
use statement_element::StatementElement;
use types::{Type, Variable};

const ABOVE_STACK_OFFSET: isize = -1;

pub(crate) fn compile(program: &[LanguageElement], flags: &Flags) -> Result<String, CompileError> {
	let instructions = compile_elements(
		program,
		&mut HashMap::new(),
		&mut HashMap::new(),
		&mut HashMap::new(),
		"global",
		&mut 0,
	)?;
	if instructions
		.iter()
		.map(|(instruction, _)| instruction.size())
		.sum::<usize>()
		> 255
	{
		return Err(CompileError(line!(), "Program is too large for digiflisp!"));
	}
	let mut output = String::new();
	for (i, c) in instructions.iter().skip(1) {
		match (flags.hex, flags.comments) {
			(true, true) => match (i, c) {
				(inst, Some(comm)) => output.push_str(&format!("{:X}\t ; {}", inst, comm)),
				(inst, None) => output.push_str(&format!("{:X}", inst)),
			},
			(false, true) => match (i, c) {
				(inst, Some(comm)) => output.push_str(&format!("{}\t ; {}", inst, comm)),
				(inst, None) => output.push_str(&format!("{}", inst)),
			},
			(true, false) => output.push_str(&format!("{:X}", i)),
			(false, false) => output.push_str(&format!("{}", i)),
		}

		if !matches!(i, Instruction::Label(n) if n.len() < 8) {
			output.push('\n');
		} else {
			output.push(' ');
		}
	}
	Ok(output)
}

fn compile_element<'a>(
	element: &'a LanguageElement,
	variables: &mut HashMap<&'a str, (Type, isize)>,
	global_variables: &mut HashMap<&'a str, (Type, isize)>,
	functions: &mut HashMap<&'a str, &'a [Variable<'a>]>,
	scope_name: &str,
	stack_size: &mut isize,
	line_id: usize,
) -> Result<Vec<CommentedInstruction<'a>>, CompileError> {
	let res = match element {
		LanguageElement::VariableDeclaration { typ, name } => {
			if variables.contains_key(name) {
				dbg!(element);
				return Err(CompileError(line!(), "Name already exists in scope!"));
			}
			variables.insert(*name, (typ.clone(), *stack_size));
			*stack_size += 1;
			vec![(Instruction::AddToStack, Some(*name))]
		}
		LanguageElement::VariableAssignment { name, value } => {
			let adr = if let Some(&(_, stack_address)) = variables.get(name) {
				Addressing::SP(stack_address)
			} else if let Some(&(_, stack_address)) = global_variables.get(name) {
				Addressing::Adr(stack_address)
			} else {
				dbg!(element);
				return Err(CompileError(
					line!(),
					"Name resolution failed? Shouldn't be checked by now?",
				));
			};
			let mut statement = compile_statement(value, variables, global_variables, stack_size)?;
			statement.push((Instruction::STA(adr), None));
			statement
		}
		LanguageElement::VariableDeclarationAssignment { typ, name, value } => {
			if variables.contains_key(name) {
				dbg!(element);
				return Err(CompileError(line!(), "Name already exists in scope!"));
			}
			variables.insert(*name, (typ.clone(), *stack_size));
			*stack_size += 1;
			let mut statement = compile_statement(value, variables, global_variables, stack_size)?;
			statement.push((Instruction::PSHA, Some(*name)));
			statement
		}
		LanguageElement::PointerAssignment { ptr, value } => {
			let get_adr = compile_statement(ptr, variables, global_variables, stack_size)?;
			let mut value = compile_statement(value, variables, global_variables, stack_size)?;
			let mut statement = get_adr;
			statement.push((Instruction::STA(Addressing::SP(ABOVE_STACK_OFFSET)), None));
			statement.push((Instruction::LDX(Addressing::SP(ABOVE_STACK_OFFSET)), None));
			statement.append(&mut value);
			statement.push((Instruction::STA(Addressing::Xn(0)), None));
			statement
		}
		LanguageElement::FunctionDeclaration {
			typ: _,
			name,
			args,
			block,
		} => {
			if functions.contains_key(name) {
				return Err(CompileError(
					line!(),
					"Function name already exists in scope!",
				));
			}
			functions.insert(*name, args.as_slice());
			let mut args_count = args.len() as isize;
			let mut local_variables = HashMap::new();
			let mut fun = compile_elements(
				block,
				&mut local_variables,
				global_variables,
				functions,
				name,
				&mut args_count,
			)?;
			if args_count != 0 {
				fun.push((Instruction::LEASP(Addressing::SP(-args_count)), None));
			}
			fun
		}
		LanguageElement::IfStatement {
			condition,
			then,
			else_then,
		} => {
			let line_id_str = line_id.to_string();
			let then_str = "if_then_".to_string() + scope_name + "_" + &line_id_str;
			let else_str = "if_else_".to_string() + scope_name + "_" + &line_id_str;
			let end_str = "if_end_".to_string() + scope_name + "_" + &line_id_str;
			let mut cond = compile_statement(condition, variables, global_variables, stack_size)?;
			cond.push((Instruction::TSTA, None));
			let mut then_stack = *stack_size;
			let mut then_block = compile_elements(
				then.as_slice(),
				&mut variables.clone(),
				global_variables,
				functions,
				&then_str,
				&mut then_stack,
			)?;
			if then_stack != *stack_size {
				cond.push((
					Instruction::LEASP(Addressing::SP(then_stack - *stack_size)),
					None,
				));
			}
			if let Some(v) = else_then {
				let mut else_stack = *stack_size;
				let mut else_block = compile_elements(
					v.as_slice(),
					&mut variables.clone(),
					global_variables,
					functions,
					&else_str,
					&mut else_stack,
				)?;
				cond.push((Instruction::BEQ(Addressing::Label(else_str)), None));
				cond.append(&mut then_block);
				cond.push((Instruction::JMP(Addressing::Label(end_str.clone())), None));
				cond.append(&mut else_block);
				if else_stack != *stack_size {
					cond.push((
						Instruction::LEASP(Addressing::SP(else_stack - *stack_size)),
						None,
					));
				}
			} else {
				cond.push((Instruction::BEQ(Addressing::Label(end_str.clone())), None));
				cond.append(&mut then_block);
			}
			cond.push((Instruction::Label(end_str), None));
			cond
		}
		LanguageElement::For {
			init,
			condition,
			after,
			body,
		} => todo!(),
		LanguageElement::While { condition, body } => todo!(),
	};
	Ok(res)
}

fn compile_elements<'a>(
	block: &'a [LanguageElement],
	variables: &mut HashMap<&'a str, (Type, isize)>,
	global_variables: &mut HashMap<&'a str, (Type, isize)>,
	functions: &mut HashMap<&'a str, &'a [Variable<'a>]>,
	scope_name: &str,
	stack_size: &mut isize,
) -> Result<Vec<CommentedInstruction<'a>>, CompileError> {
	let mut instructions = vec![(Instruction::Label(scope_name.to_string()), None)];
	for line in block.iter().enumerate().map(|(i, e)| {
		compile_element(
			e,
			variables,
			global_variables,
			functions,
			scope_name,
			stack_size,
			i,
		)
	}) {
		instructions.append(&mut line?);
	}
	Ok(instructions)
}

fn compile_statement<'a>(
	statement: &'a StatementElement,
	variables: &mut HashMap<&'a str, (Type, isize)>,
	global_variables: &HashMap<&'a str, (Type, isize)>,
	stack_size: &mut isize,
) -> Result<Vec<CommentedInstruction<'a>>, CompileError> {
	let depth = statement.depth();
	let mut statement_instructions =
		compile_statement_inner(statement, variables, global_variables, stack_size)?;
	let block = if depth > 1 {
		let mut block = vec![(
			Instruction::LEASP(Addressing::SP(depth as isize - 1)),
			Some("Reserving memory for statement"),
		)];
		block.append(&mut statement_instructions);
		block.append(&mut vec![(
			Instruction::LEASP(Addressing::SP(-(depth as isize - 1))),
			Some("Clearing memory for statement"),
		)]);
		block
	} else {
		statement_instructions
	};

	Ok(block)
}

fn compile_statement_inner<'a>(
	statement: &'a StatementElement,
	variables: &mut HashMap<&'a str, (Type, isize)>,
	global_variables: &HashMap<&'a str, (Type, isize)>,
	stack_size: &mut isize,
) -> Result<Vec<CommentedInstruction<'a>>, CompileError> {
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
			//DOESN'T WORK AT ALL
			let left_depth = lhs.depth();
			let right_depth = rhs.depth();
			let (left, right, depth) = if left_depth >= right_depth {
				(lhs.as_ref(), rhs.as_ref(), right_depth)
			} else {
				(rhs.as_ref(), lhs.as_ref(), left_depth)
			};
			let mut instructions =
				compile_statement_inner(left, variables, global_variables, stack_size)?;
			if depth > 1 {
				instructions.push((Instruction::STA(Addressing::SP(*stack_size)), None));
				*stack_size += 1;
			} else {
				let right_instructions =
					compile_statement_inner(right, variables, global_variables, stack_size)?;
				let addressing = match right_instructions.as_slice() {
					[(instruction, _)] => instruction.address().ok_or(CompileError(
						line!(),
						"Internal: Invalid right hand instruction?",
					)),
					_ => {
						dbg!(statement);
						return Err(CompileError(line!(), "Internal: Depth caluclation failed?"));
					}
				}?;
				let merge = (statement.as_flisp_instruction(addressing), None);
				instructions.push(merge);
			}
			instructions
		}
		StatementElement::FunctionCall {
			name: _,
			parametres: _,
		} => {
			dbg!(statement);
			return Err(CompileError(
				line!(),
				"Internal: Function call in statement. Should've been moved out?",
			));
		}
		StatementElement::Var(name) => {
			let adr = if let Some((_, adr)) = variables.get(name) {
				Addressing::SP(*stack_size - *adr)
			} else if let Some((_, adr)) = global_variables.get(name) {
				Addressing::Adr(*adr)
			} else {
				eprintln!("Error: {}", name);
				return Err(CompileError(
					line!(),
					"Name resolution failed? Shouldn't be checked by now?",
				));
			};
			vec![(Instruction::LDA(adr), Some(*name))]
		}
		StatementElement::Num(n) => {
			vec![(Instruction::LDA(Addressing::Data(*n)), None)]
		}
		StatementElement::Char(c) => {
			vec![(Instruction::LDA(Addressing::Data(*c as isize)), None)]
		}
		StatementElement::Bool(b) => {
			vec![(Instruction::LDA(Addressing::Data(*b as isize)), None)]
		}
		StatementElement::Array(arr) => {
			let mut vec = Vec::new();
			for element in arr.iter() {
				let mut instructions =
					compile_statement_inner(element, variables, global_variables, stack_size)?;
				*stack_size += 1;
				vec.append(&mut instructions);
				vec.push((Instruction::PSHA, None));
			}
			vec
		}
		StatementElement::Deref(adr) => {
			//Is this sound if it occurs on the left hand side?
			let mut instructions =
				compile_statement_inner(adr.as_ref(), variables, global_variables, stack_size)?;
			instructions.push((Instruction::STA(Addressing::SP(ABOVE_STACK_OFFSET)), None));
			instructions.push((Instruction::LDX(Addressing::SP(ABOVE_STACK_OFFSET)), None));
			instructions.push((Instruction::LDA(Addressing::Xn(0)), None));
			instructions
		}
		StatementElement::AdrOf(name) => {
			if let Some((_, adr)) = variables.get(name) {
				vec![(
					Instruction::LDA(Addressing::Data(*stack_size - adr)),
					Some(*name),
				)]
			} else {
				dbg!(name);
				return Err(CompileError(
					line!(),
					"Name resolution failed? Shouldn't it've been checked by now?",
				));
			}
		}
		StatementElement::Not { lhs: _ } => unimplemented!(),
	};
	Ok(instructions)
}

use std::collections::HashMap;

use crate::*;
use flisp_instructions::{Addressing, CommentedInstruction, Instruction};
use statement_element::StatementElement;
use types::{Type, Variable};

const ABOVE_STACK_OFFSET: isize = -1;

pub(crate) fn compile<'a>(
	program: &'a [LanguageElement],
	flags: &Flags,
) -> Result<Vec<CommentedInstruction<'a>>, CompileError> {
	compile_elements(
		program,
		&mut HashMap::new(),
		&mut HashMap::new(),
		&mut HashMap::new(),
		"global",
		&mut 0,
		flags.optimise,
	)
}

fn compile_elements<'a>(
	block: &'a [LanguageElement],
	variables: &mut HashMap<&'a str, (Type, isize)>,
	global_variables: &mut HashMap<&'a str, (Type, isize)>,
	functions: &mut HashMap<&'a str, &'a [Variable<'a>]>,
	scope_name: &str,
	stack_size: &mut isize,
	optimise: bool,
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
		let line = &mut line?;
		if optimise {
			optimise_flisp::all_optimisations(line);
		}
		instructions.append(line);
	}
	Ok(instructions)
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
			if scope_name == "global" {
				if global_variables.contains_key(name) {
					dbg!(element);
					return Err(CompileError(line!(), "Name already exists in scope!"));
				}
				global_variables.insert(*name, (typ.clone(), *stack_size));
				vec![]
			} else {
				if variables.contains_key(name) {
					dbg!(element);
					return Err(CompileError(line!(), "Name already exists in scope!"));
				}
				variables.insert(*name, (typ.clone(), *stack_size));
				*stack_size += 1;
				vec![(
					Instruction::LEASP(Addressing::SP(ABOVE_STACK_OFFSET)),
					Some(*name),
				)]
			}
		}

		LanguageElement::VariableAssignment { name, value } => {
			let adr = if let Some(&(_, stack_address)) = variables.get(name) {
				Addressing::SP(*stack_size - stack_address)
			} else if let Some(&(_, stack_address)) = global_variables.get(name) {
				Addressing::Adr(stack_address)
			} else {
				dbg!(element);
				return Err(CompileError(
					line!(),
					"Name resolution failed? Shouldn't be checked by now?",
				));
			};
			let mut statement =
				compile_statement(value, variables, global_variables, functions, stack_size)?;
			statement.push((Instruction::STA(adr), Some(name)));
			statement
		}

		LanguageElement::VariableDeclarationAssignment { typ, name, value } => {
			if scope_name == "global" {
				todo!();
			} else {
				if variables.contains_key(name) {
					dbg!(element);
					return Err(CompileError(line!(), "Name already exists in scope!"));
				}
				*stack_size += 1;
				variables.insert(*name, (typ.clone(), *stack_size));
				let mut statement =
					compile_statement(value, variables, global_variables, functions, stack_size)?;
				statement.push((Instruction::PSHA, Some(*name)));
				statement
			}
		}

		LanguageElement::PointerAssignment { ptr, value } => {
			let get_adr =
				compile_statement(ptr, variables, global_variables, functions, stack_size)?;
			let mut value =
				compile_statement(value, variables, global_variables, functions, stack_size)?;
			let mut statement = get_adr;
			statement.push((
				Instruction::STA(Addressing::SP(ABOVE_STACK_OFFSET)),
				Some("A to X part 1"),
			));
			statement.push((
				Instruction::LDX(Addressing::SP(ABOVE_STACK_OFFSET)),
				Some("A to X part 2"),
			));
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
			for (idx, Variable { name, typ }) in args.iter().enumerate() {
				local_variables.insert(*name, (typ.clone(), idx as isize));
			}
			let mut fun = compile_elements(
				block,
				&mut local_variables,
				global_variables,
				functions,
				name,
				&mut args_count,
				false,
			)?;
			args_count -= args.len() as isize;
			if args_count != 0 {
				fun.push((Instruction::LEASP(Addressing::SP(args_count)), None));
			}
			fun.push((Instruction::RTS, None));
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
			let mut cond = compile_statement(
				condition,
				variables,
				global_variables,
				functions,
				stack_size,
			)?;
			cond.push((Instruction::TSTA, None));
			let mut then_stack = *stack_size;
			let mut then_block = compile_elements(
				then.as_slice(),
				&mut variables.clone(),
				global_variables,
				functions,
				&then_str,
				&mut then_stack,
				false,
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
					false,
				)?;
				cond.push((Instruction::BNE(Addressing::Label(else_str)), None));
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
				cond.push((Instruction::BNE(Addressing::Label(end_str.clone())), None));
				cond.append(&mut then_block);
			}
			cond.push((Instruction::Label(end_str), None));
			cond
		}

		LanguageElement::For {
			init: _,
			condition: _,
			after: _,
			body: _,
		} => todo!(),

		LanguageElement::While {
			condition: _,
			body: _,
		} => todo!(),

		LanguageElement::Return(ret) => {
			if let Some(statement) = ret {
				compile_statement(
					statement,
					variables,
					global_variables,
					functions,
					stack_size,
				)?
			} else {
				vec![]
			}
		}

		LanguageElement::Statement(statement) => compile_statement(
			statement,
			variables,
			global_variables,
			functions,
			stack_size,
		)?,
	};
	Ok(res)
}

fn compile_statement<'a>(
	statement: &'a StatementElement,
	variables: &mut HashMap<&'a str, (Type, isize)>,
	global_variables: &HashMap<&'a str, (Type, isize)>,
	functions: &mut HashMap<&'a str, &'a [Variable<'a>]>,
	stack_size: &mut isize,
) -> Result<Vec<CommentedInstruction<'a>>, CompileError> {
	let depth = statement.depth() as isize - 1;
	let mut statement_instructions = compile_statement_inner(
		statement,
		variables,
		global_variables,
		functions,
		stack_size,
		&mut 0,
		if depth >= 2 { depth } else { 0 },
	)?;
	if depth > 1 {
		let mut block = vec![(
			Instruction::LEASP(Addressing::SP(-depth)),
			Some("Reserving memory for statement"),
		)];
		block.append(&mut statement_instructions);
		block.append(&mut vec![(
			Instruction::LEASP(Addressing::SP(depth)), //why not -1?
			Some("Clearing memory for statement"),
		)]);
		statement_instructions = block;
	};

	Ok(statement_instructions)
}

fn compile_statement_inner<'a>(
	statement: &'a StatementElement,
	variables: &mut HashMap<&'a str, (Type, isize)>,
	global_variables: &HashMap<&'a str, (Type, isize)>,
	functions: &mut HashMap<&'a str, &'a [Variable<'a>]>,
	stack_size: &mut isize,
	tmps_used: &mut isize,
	tmps: isize,
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
		| StatementElement::LT { lhs: rhs, rhs: lhs }
		| StatementElement::GTE { lhs: rhs, rhs: lhs }
		| StatementElement::LTE { lhs, rhs }
		| StatementElement::NotCmp { lhs, rhs }
		| StatementElement::Cmp { lhs, rhs } => {
			let left_depth = lhs.depth();
			let right_depth = rhs.depth();
			let (left, right) = if left_depth >= right_depth {
				(lhs.as_ref(), rhs.as_ref())
			} else {
				(rhs.as_ref(), lhs.as_ref())
			};
			let mut instructions = compile_statement_inner(
				left,
				variables,
				global_variables,
				functions,
				stack_size,
				tmps_used,
				tmps,
			)?;
			*tmps_used += 1;
			let mut right_instructions = compile_statement_inner(
				right,
				variables,
				global_variables,
				functions,
				stack_size,
				tmps_used,
				tmps,
			)?;
			match statement {
				StatementElement::Mul { rhs: _, lhs: _ } => {
					instructions.push((Instruction::PSHA, Some("mul rhs")));
					instructions.append(&mut right_instructions);
					instructions.push((Instruction::PSHA, Some("mul lhs")));
					instructions.push((
						Instruction::JSR(Addressing::Label("__mul__".to_string())),
						None,
					));
					instructions.push((Instruction::LEASP(Addressing::SP(2)), None));
				}
				StatementElement::Div { rhs: _, lhs: _ } => {
					instructions.push((Instruction::PSHA, Some("div rhs")));
					instructions.append(&mut right_instructions);
					instructions.push((Instruction::PSHA, Some("div lhs")));
					instructions.push((
						Instruction::JSR(Addressing::Label("__div__".to_string())),
						None,
					));
					instructions.push((Instruction::LEASP(Addressing::SP(2)), None));
				}
				StatementElement::Mod { rhs: _, lhs: _ } => {
					instructions.push((Instruction::PSHA, Some("mod rhs")));
					instructions.append(&mut right_instructions);
					instructions.push((Instruction::PSHA, Some("mod lhs")));
					instructions.push((
						Instruction::JSR(Addressing::Label("__mod__".to_string())),
						None,
					));
					instructions.push((Instruction::LEASP(Addressing::SP(2)), None));
				}
				StatementElement::GT { rhs: _, lhs: _ }
				| StatementElement::LT { rhs: _, lhs: _ } => {
					instructions.push((Instruction::PSHA, Some("gt rhs")));
					instructions.append(&mut right_instructions);
					instructions.push((
						Instruction::JSR(Addressing::Label("__gt__".to_string())),
						None,
					));
					instructions.push((Instruction::LEASP(Addressing::SP(1)), None));
				}
				StatementElement::LTE { rhs: _, lhs: _ }
				| StatementElement::GTE { rhs: _, lhs: _ } => {
					instructions.push((Instruction::PSHA, Some("lte rhs")));
					instructions.append(&mut right_instructions);
					instructions.push((
						Instruction::JSR(Addressing::Label("__gt__".to_string())),
						None,
					));
					instructions.push((Instruction::LEASP(Addressing::SP(1)), None));
					instructions.push((Instruction::COMA, None));
				}
				StatementElement::Cmp { rhs: _, lhs: _ } => {
					if let [(Instruction::LDA(adr), comment)] = &right_instructions.as_slice() {
						instructions.push((statement.as_flisp_instruction(adr.clone()), *comment));
					} else {
						instructions.push((Instruction::STA(Addressing::SP(-*tmps_used)), None));
						instructions.append(&mut right_instructions);
						instructions.push((
							statement.as_flisp_instruction(Addressing::SP(-*tmps_used)),
							None,
						));
					}
					instructions.push((Instruction::COMA, None));
				}
				_ => {
					if let [(Instruction::LDA(adr), comment)] = &right_instructions.as_slice() {
						instructions.push((statement.as_flisp_instruction(adr.clone()), *comment));
					} else {
						instructions.push((Instruction::STA(Addressing::SP(-*tmps_used)), None));
						instructions.append(&mut right_instructions);
						instructions.push((
							statement.as_flisp_instruction(Addressing::SP(-*tmps_used)),
							None,
						));
					}
				}
			}

			*tmps_used -= 1;
			instructions
		}

		StatementElement::FunctionCall { name, parametres } => {
			let mut instructions = Vec::new();
			let arg_names = functions.get(name).ok_or(CompileError(
				line!(),
				"Name resolution failed? Shouldn't it've been checked by now?",
			))?;
			for (
				statement,
				Variable {
					name: v_name,
					typ: _,
				},
			) in parametres.iter().zip(arg_names.iter())
			{
				instructions.append(&mut compile_statement(
					statement,
					variables,
					global_variables,
					functions,
					stack_size,
				)?);
				instructions.push((Instruction::PSHA, Some(v_name)));
			}
			instructions.push((Instruction::JSR(Addressing::Label(name.to_string())), None));
			instructions.push((
				Instruction::LEASP(Addressing::SP(parametres.len() as isize)),
				None,
			));
			instructions
		}

		StatementElement::Var(name) => {
			let adr = if let Some((_, adr)) = variables.get(name) {
				Addressing::SP(*stack_size + tmps - *adr)
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
				let mut instructions = compile_statement_inner(
					element,
					variables,
					global_variables,
					functions,
					stack_size,
					tmps_used,
					tmps,
				)?;
				*stack_size += 1;
				vec.append(&mut instructions);
				vec.push((Instruction::PSHA, None));
			}
			vec
		}

		StatementElement::Deref(adr) => {
			match (adr.as_ref(), adr.internal_ref()) {
				//Array opt
				(
					&StatementElement::Add { lhs: _, rhs: _ },
					Some((&StatementElement::Var(name), &StatementElement::Num(offset))),
				) => {
					let adr = if let Some((_, adr)) = variables.get(name) {
						Addressing::SP(*stack_size + tmps - *adr)
					} else if let Some((_, adr)) = global_variables.get(name) {
						Addressing::Adr(*adr)
					} else {
						eprintln!("Error: {}", name);
						return Err(CompileError(
							line!(),
							"Name resolution failed? Shouldn't be checked by now?",
						));
					};
					vec![
						(Instruction::LDY(adr), Some(name)),
						(Instruction::LDA(Addressing::Yn(offset)), None),
					]
				}
				//General opt
				(
					&StatementElement::Add { lhs: _, rhs: _ },
					Some((&StatementElement::Var(name), rhs)),
				) => {
					let adr = if let Some((_, adr)) = variables.get(name) {
						Addressing::SP(*stack_size + tmps - *adr)
					} else if let Some((_, adr)) = global_variables.get(name) {
						Addressing::Adr(*adr)
					} else {
						eprintln!("Error: {}", name);
						return Err(CompileError(
							line!(),
							"Name resolution failed? Shouldn't be checked by now?",
						));
					};
					let mut statement =
						compile_statement(rhs, variables, global_variables, functions, stack_size)?;
					statement.append(&mut vec![
						(Instruction::LDY(adr), Some(name)),
						(Instruction::LDA(Addressing::AY), None),
					]);
					statement
				}
				//General case
				_ => {
					let mut instructions = compile_statement_inner(
						adr.as_ref(),
						variables,
						global_variables,
						functions,
						stack_size,
						tmps_used,
						tmps,
					)?;
					instructions.push((
						Instruction::STA(Addressing::SP(ABOVE_STACK_OFFSET)),
						Some("A to X transfer"),
					));
					instructions.push((
						Instruction::LDX(Addressing::SP(ABOVE_STACK_OFFSET)),
						Some("A to X  continued"),
					));
					instructions.push((Instruction::LDA(Addressing::Xn(0)), None));
					instructions
				}
			}
		}

		StatementElement::AdrOf(name) => {
			if let Some((_, adr)) = variables.get(name) {
				vec![(
					Instruction::LDA(Addressing::Data(*stack_size + tmps - adr)),
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

pub(crate) fn instructions_to_text(
	instructions: &[CommentedInstruction],
	flags: &Flags,
) -> Result<String, CompileError> {
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
		if matches!(i, Instruction::RTS) {
			output.push('\n');
		}
	}
	Ok(output)
}

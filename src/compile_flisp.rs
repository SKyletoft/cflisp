use std::collections::HashMap;

use crate::*;

///Technically illegal address for use in register -> register transfers
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
		0,
		flags.optimise,
	)
}

fn compile_elements<'a>(
	block: &'a [LanguageElement],
	variables: &mut HashMap<&'a str, (Type, isize)>,
	global_variables: &mut HashMap<&'a str, Type>,
	functions: &mut HashMap<&'a str, &'a [Variable<'a>]>,
	scope_name: &str,
	stack_size: &mut isize,
	stack_base: isize,
	optimise: bool,
) -> Result<Vec<CommentedInstruction<'a>>, CompileError> {
	let mut instructions = vec![(Instruction::Label(scope_name.to_string()), None)];
	for (i, e) in block.iter().enumerate() {
		let line = &mut compile_element(
			e,
			variables,
			global_variables,
			functions,
			scope_name,
			stack_size,
			stack_base,
			i,
			optimise,
		)?;
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
	global_variables: &mut HashMap<&'a str, Type>,
	functions: &mut HashMap<&'a str, &'a [Variable<'a>]>,
	scope_name: &str,
	stack_size: &mut isize,
	stack_base: isize,
	line_id: usize,
	optimise: bool,
) -> Result<Vec<CommentedInstruction<'a>>, CompileError> {
	let res = match element {
		LanguageElement::VariableDeclaration { typ, name } => {
			if scope_name == "global" {
				if global_variables.contains_key(name) {
					dbg!(element);
					return Err(CompileError(line!(), "Name already exists in scope!"));
				}
				global_variables.insert(*name, typ.clone());
				vec![]
			} else {
				if variables.contains_key(name) {
					dbg!(element);
					return Err(CompileError(
						line!(),
						"Name already exists in scope! (No shadowing)",
					));
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
			let adr = adr_for_name(name, variables, global_variables, *stack_size)?;
			let mut statement =
				compile_statement(value, variables, global_variables, functions, *stack_size)?;
			statement.push((Instruction::STA(adr), Some(name)));
			statement
		}

		LanguageElement::VariableDeclarationAssignment { typ, name, value } => {
			let global_def = |val: &StatementElement| match val {
				StatementElement::Num(n) => Ok(*n),
				StatementElement::Char(c) => Ok(*c as isize),
				StatementElement::Bool(b) => Ok(*b as isize),
				_ => Err(CompileError(
					line!(),
					"Non constant in array initialisation",
				)),
			};
			if scope_name == "global" {
				if global_variables.contains_key(name) {
					dbg!(element);
					return Err(CompileError(line!(), "Name already exists in scope!"));
				}
				if let StatementElement::Array(elements) = value {
					global_variables.insert(*name, typ.clone());
					let values = elements
						.iter()
						.map(global_def)
						.collect::<Result<Vec<isize>, CompileError>>()?;
					vec![
						(Instruction::Label(name.to_string()), None),
						(Instruction::FCB(values), None),
					]
				} else {
					global_variables.insert(*name, typ.clone());
					vec![
						(Instruction::Label(name.to_string()), None),
						(Instruction::FCB(vec![global_def(value)?]), None),
					]
				}
			} else {
				if variables.contains_key(name) || global_variables.contains_key(name) {
					dbg!(element);
					return Err(CompileError(
						line!(),
						"Name already exists in scope! (No shadowing)",
					));
				}
				if let StatementElement::Array(elements) = value {
					let mut statement = Vec::new();
					for element in elements {
						let stack_copy = *stack_size;
						statement.append(&mut compile_statement(
							element,
							variables,
							global_variables,
							functions,
							*stack_size,
						)?);
						assert_eq!(*stack_size, stack_copy);
						*stack_size += 1;
						statement.push((Instruction::PSHA, Some(*name)));
					}
					variables.insert(*name, (typ.clone(), *stack_size));
					statement
				} else {
					let stack_copy = *stack_size;
					let mut statement = compile_statement(
						value,
						variables,
						global_variables,
						functions,
						*stack_size,
					)?;
					assert_eq!(*stack_size, stack_copy);
					variables.insert(*name, (typ.clone(), *stack_size));
					*stack_size += 1;
					statement.push((Instruction::PSHA, Some(*name)));
					statement
				}
			}
		}

		LanguageElement::PointerAssignment { ptr, value } => {
			let get_adr =
				compile_statement(ptr, variables, global_variables, functions, *stack_size)?;
			let mut value =
				compile_statement(value, variables, global_variables, functions, *stack_size)?;
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
			//Stack: Start -> Arguments -> Return adr -> Variables -> Whatever the stack grows to
			// If this +1 is removed we can no longer have arguments
			// This might waste a byte if there are no arguments?
			let mut args_count = args.len() as isize + 1;
			let args_base = args_count;
			let mut local_variables = HashMap::new();
			for (idx, Variable { name, typ }) in args.iter().enumerate() {
				local_variables.insert(*name, (typ.clone(), idx as isize));
			}
			let mut function_body = compile_elements(
				block,
				&mut local_variables,
				global_variables,
				functions,
				name,
				&mut args_count,
				args_base,
				false,
			)?;
			args_count -= args.len() as isize + 1;
			if args_count != 0 {
				function_body.push((
					Instruction::LEASP(Addressing::SP(args_count)),
					Some("Clearing variables"),
				));
			}
			function_body.push((Instruction::RTS, None));
			function_body
		}

		LanguageElement::IfStatement {
			condition,
			then,
			else_then,
		} => {
			let then_str = format!("if_then_{}_{}", scope_name, line_id);
			let else_str = format!("if_else_{}_{}", scope_name, line_id);
			let end_str = format!("if_end_{}_{}", scope_name, line_id);
			let mut cond = compile_statement(
				condition,
				variables,
				global_variables,
				functions,
				*stack_size,
			)?;
			cond.push((Instruction::TSTA, None));
			let mut then_stack = *stack_size;
			let mut then_block = compile_elements(
				then,
				&mut variables.clone(),
				global_variables,
				functions,
				&then_str,
				&mut then_stack,
				*stack_size,
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
					v,
					&mut variables.clone(),
					global_variables,
					functions,
					&else_str,
					&mut else_stack,
					*stack_size,
					false,
				)?;
				cond.push((Instruction::BEQ(Addressing::Label(else_str)), None));
				cond.append(&mut then_block);
				cond.push((Instruction::JMP(Addressing::Label(end_str.clone())), None));
				cond.append(&mut else_block);
				if else_stack != *stack_size {
					cond.push((
						Instruction::LEASP(Addressing::SP(else_stack - *stack_size)),
						Some("This happened"),
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
			post,
			body,
		} => {
			let init_str = format!("for_init_{}_{}", scope_name, line_id);
			let cond_str = format!("for_cond_{}_{}", scope_name, line_id);
			let post_str = format!("for_post_{}_{}", scope_name, line_id);
			let body_str = format!("for_body_{}_{}", scope_name, line_id);
			let end_str = format!("for_end_{}_{}", scope_name, line_id);
			let mut inner_variables = variables.clone();
			let mut inner_stack = *stack_size;
			let mut instructions = compile_elements(
				init,
				&mut inner_variables,
				global_variables,
				functions,
				&init_str,
				&mut inner_stack,
				stack_base,
				optimise,
			)?;
			instructions.push((Instruction::Label(cond_str.clone()), None));
			instructions.append(&mut compile_statement(
				condition,
				&inner_variables,
				global_variables,
				functions,
				inner_stack,
			)?);
			instructions.push((Instruction::TSTA, None));
			instructions.push((Instruction::BEQ(Addressing::Label(end_str.clone())), None));
			let mut inner_inner_stack = inner_stack;
			instructions.append(&mut compile_elements(
				body,
				&mut inner_variables.clone(),
				global_variables,
				functions,
				&body_str,
				&mut inner_inner_stack,
				stack_base,
				optimise,
			)?);
			instructions.push((
				Instruction::LEASP(Addressing::SP(inner_inner_stack - inner_stack)),
				None,
			));
			instructions.append(&mut compile_elements(
				post,
				&mut inner_variables,
				global_variables,
				functions,
				&post_str,
				&mut inner_stack,
				stack_base,
				optimise,
			)?);
			instructions.push((Instruction::JMP(Addressing::Label(cond_str)), None));
			instructions.push((Instruction::Label(end_str), None));
			instructions.push((
				Instruction::LEASP(Addressing::SP(inner_stack - *stack_size)),
				None,
			));
			instructions
		}

		LanguageElement::While { condition, body } => {
			let cond_str = format!("while_cond_{}_{}", scope_name, line_id);
			let body_str = format!("while_body_{}_{}", scope_name, line_id);
			let end_str = format!("while_end_{}_{}", scope_name, line_id);
			let mut instructions = vec![(Instruction::Label(cond_str.clone()), None)];
			instructions.append(&mut compile_statement(
				condition,
				variables,
				global_variables,
				functions,
				*stack_size,
			)?);
			instructions.push((Instruction::TSTA, None));
			instructions.push((Instruction::BEQ(Addressing::Label(end_str.clone())), None));
			let mut inner_stack = *stack_size;
			instructions.append(&mut compile_elements(
				body,
				&mut variables.clone(),
				global_variables,
				functions,
				&body_str,
				&mut inner_stack,
				stack_base,
				optimise,
			)?);
			instructions.push((
				Instruction::LEASP(Addressing::SP(inner_stack - *stack_size)),
				None,
			));
			instructions.push((Instruction::JMP(Addressing::Label(cond_str)), None));
			instructions.push((Instruction::Label(end_str), None));
			instructions
		}

		LanguageElement::Return(ret) => {
			if let Some(statement) = ret {
				let mut statement = compile_statement(
					statement,
					variables,
					global_variables,
					functions,
					*stack_size,
				)?;
				statement.push((
					Instruction::LEASP(Addressing::SP(*stack_size - stack_base)),
					None,
				));
				statement.push((Instruction::RTS, None));
				statement
			} else {
				vec![
					(
						Instruction::LEASP(Addressing::SP(*stack_size - stack_base)),
						None,
					),
					(Instruction::RTS, None),
				]
			}
		}

		LanguageElement::Statement(statement) => compile_statement(
			statement,
			variables,
			global_variables,
			functions,
			*stack_size,
		)?,
	};
	Ok(res)
}

fn compile_statement<'a>(
	statement: &'a StatementElement,
	variables: &HashMap<&'a str, (Type, isize)>,
	global_variables: &HashMap<&'a str, Type>,
	functions: &HashMap<&'a str, &'a [Variable<'a>]>,
	stack_size: isize,
) -> Result<Vec<CommentedInstruction<'a>>, CompileError> {
	let tmps = (statement.depth() as isize - 2).max(0); //Minus 2 because bottom is value and ops keep one value in memory
	let mut statement_instructions = compile_statement_inner(
		statement,
		variables,
		global_variables,
		functions,
		stack_size + tmps,
		&mut tmps.clone(), //Will start by -1.
		                   // Counts down so that positions closest to the stack are used first which helps optimisation to save memory
	)?;
	if tmps > 0 {
		let mut block = vec![(
			Instruction::LEASP(Addressing::SP(-tmps)),
			Some("Reserving memory for statement"),
		)];
		block.append(&mut statement_instructions);
		block.append(&mut vec![(
			Instruction::LEASP(Addressing::SP(tmps)), //why not -1?
			Some("Clearing memory for statement"),
		)]);
		statement_instructions = block;
	};

	Ok(statement_instructions)
}

fn compile_statement_inner<'a>(
	statement: &'a StatementElement,
	variables: &HashMap<&'a str, (Type, isize)>,
	global_variables: &HashMap<&'a str, Type>,
	functions: &HashMap<&'a str, &'a [Variable<'a>]>,
	stack_size: isize,
	tmps_used: &mut isize,
) -> Result<Vec<CommentedInstruction<'a>>, CompileError> {
	let instructions = match statement {
		//Commutative operations
		StatementElement::Add { lhs, rhs }
		| StatementElement::Mul { lhs, rhs }
		| StatementElement::And { lhs, rhs }
		| StatementElement::Or { lhs, rhs }
		| StatementElement::Xor { lhs, rhs }
		| StatementElement::NotCmp { lhs, rhs }
		| StatementElement::Cmp { lhs, rhs } => {
			let left_depth = lhs.depth();
			let right_depth = rhs.depth();
			let (left, right) = if left_depth > right_depth {
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
			)?;
			let mut right_instructions_plus_one = || {
				*tmps_used -= 1;
				let res = compile_statement_inner(
					right,
					variables,
					global_variables,
					functions,
					stack_size + 1, //Plus one to take the PSHAs into account
					tmps_used,
				);
				*tmps_used += 1;
				res
			};
			match statement {
				StatementElement::Mul { rhs: _, lhs: _ } => {
					instructions.push((Instruction::PSHA, Some("mul rhs")));
					instructions.append(&mut right_instructions_plus_one()?);
					instructions.push((
						Instruction::JSR(Addressing::Label("__mul__".to_string())),
						None,
					));
					instructions.push((Instruction::LEASP(Addressing::SP(1)), None));
				}
				StatementElement::Cmp { rhs: _, lhs: _ } => {
					instructions.push((Instruction::PSHA, Some("cmp rhs")));
					instructions.append(&mut right_instructions_plus_one()?);
					instructions.push((
						Instruction::JSR(Addressing::Label("__eq__".to_string())),
						None,
					));
					instructions.push((Instruction::LEASP(Addressing::SP(1)), None));
				}
				StatementElement::NotCmp { rhs: _, lhs: _ } => {
					instructions.push((Instruction::PSHA, Some("cmp rhs")));
					instructions.append(&mut right_instructions_plus_one()?);
					instructions.push((
						Instruction::JSR(Addressing::Label("__eq__".to_string())),
						None,
					));
					instructions.push((Instruction::LEASP(Addressing::SP(1)), None));
					instructions.push((Instruction::COMA, None));
				}
				//default:
				_ => {
					*tmps_used -= 1;
					let mut right_instructions = compile_statement_inner(
						right,
						variables,
						global_variables,
						functions,
						stack_size,
						tmps_used,
					)?;
					if let [(Instruction::LDA(adr), comment)] = &right_instructions.as_slice() {
						instructions.push((statement.as_flisp_instruction(adr.clone()), *comment));
					} else {
						assert!(right_instructions.len() >= 2);
						instructions.push((Instruction::STA(Addressing::SP(*tmps_used)), None));
						instructions.append(&mut right_instructions);
						instructions.push((
							statement.as_flisp_instruction(Addressing::SP(*tmps_used)),
							None,
						));
					}
					*tmps_used += 1;
				}
			}

			instructions
		}
		//Non-commutative operations
		StatementElement::Sub { lhs: rhs, rhs: lhs }
		| StatementElement::Div { lhs, rhs }
		| StatementElement::Mod { lhs, rhs }
		| StatementElement::LShift { lhs, rhs }
		| StatementElement::RShift { lhs, rhs }
		| StatementElement::GreaterThan { lhs, rhs }
		| StatementElement::LessThanEqual { lhs, rhs }
		| StatementElement::LessThan { lhs: rhs, rhs: lhs }
		| StatementElement::GreaterThanEqual { lhs: rhs, rhs: lhs } => {
			let (left, right) = (lhs.as_ref(), rhs.as_ref());
			let mut instructions = compile_statement_inner(
				left,
				variables,
				global_variables,
				functions,
				stack_size,
				tmps_used,
			)?;
			let mut right_instructions_plus_one = || {
				*tmps_used -= 1;
				let res = compile_statement_inner(
					right,
					variables,
					global_variables,
					functions,
					stack_size + 1, //Plus one to take the PSHAs into account
					tmps_used,
				);
				*tmps_used += 1;
				res
			};
			match statement {
				StatementElement::Div { rhs: _, lhs: _ } => todo!(),
				StatementElement::Mod { rhs: _, lhs: _ } => todo!(),
				StatementElement::GreaterThan { rhs: _, lhs: _ }
				| StatementElement::LessThan { rhs: _, lhs: _ } => {
					instructions.push((Instruction::PSHA, Some("gt rhs")));
					instructions.append(&mut right_instructions_plus_one()?);
					instructions.push((
						Instruction::JSR(Addressing::Label("__gt__".to_string())),
						None,
					));
					instructions.push((Instruction::LEASP(Addressing::SP(1)), None));
				}
				StatementElement::LessThanEqual { rhs: _, lhs: _ }
				| StatementElement::GreaterThanEqual { rhs: _, lhs: _ } => {
					instructions.push((Instruction::PSHA, Some("lte rhs")));
					instructions.append(&mut right_instructions_plus_one()?);
					instructions.push((
						Instruction::JSR(Addressing::Label("__gt__".to_string())),
						None,
					));
					instructions.push((Instruction::LEASP(Addressing::SP(1)), None));
					instructions.push((Instruction::COMA, None));
				}
				_ => {
					*tmps_used -= 1;
					let mut right_instructions = compile_statement_inner(
						right,
						variables,
						global_variables,
						functions,
						stack_size,
						tmps_used,
					)?;
					if let [(Instruction::LDA(adr), comment)] = &right_instructions.as_slice() {
						instructions.push((statement.as_flisp_instruction(adr.clone()), *comment));
					} else {
						assert!(instructions.len() >= 2);
						instructions.push((Instruction::STA(Addressing::SP(*tmps_used)), None));
						instructions.append(&mut right_instructions);
						instructions.push((
							statement.as_flisp_instruction(Addressing::SP(*tmps_used)),
							None,
						));
					}
					*tmps_used += 1;
				}
			}
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
			let adr = adr_for_name(name, variables, global_variables, stack_size)?;
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

		StatementElement::Array(_) => return Err(CompileError(line!(), "Illegal array")),

		StatementElement::Deref(adr) => {
			match (adr.as_ref(), adr.internal_ref()) {
				//Array opt
				(
					&StatementElement::Add { lhs: _, rhs: _ },
					Some((&StatementElement::Var(name), &StatementElement::Num(offset))),
				) => {
					let adr = adr_for_name(name, variables, global_variables, stack_size)?;
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
					let adr = adr_for_name(name, variables, global_variables, stack_size)?;
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
			//use adr_for_name and calculate the address -> A
			let adr = adr_for_name(name, variables, global_variables, stack_size)?;
			match adr {
				Addressing::SP(n) => {
					vec![
						(
							Instruction::STSP(Addressing::SP(ABOVE_STACK_OFFSET)),
							Some("SP -> Stack"),
						),
						(Instruction::ADDA(Addressing::SP(n)), Some(*name)),
					]
				}
				Addressing::Adr(n) => vec![(Instruction::LDA(Addressing::Data(n)), Some(*name))],
				Addressing::Label(lbl) => {
					vec![(Instruction::LDA(Addressing::DataLabel(lbl)), None)]
				}
				_ => return Err(CompileError(line!(), "Illegal access type")),
			}
		}

		StatementElement::Not { lhs: _ } => unimplemented!(),
	};
	Ok(instructions)
}

fn adr_for_name<'a>(
	name: &'a str,
	variables: &HashMap<&'a str, (Type, isize)>,
	global_variables: &HashMap<&'a str, Type>,
	stack_size: isize,
) -> Result<Addressing, CompileError> {
	if let Some((_, adr)) = variables.get(name) {
		Ok(Addressing::SP(stack_size - *adr - 1))
	} else if global_variables.contains_key(name) {
		Ok(Addressing::Label(name.to_string()))
	} else {
		eprintln!("Error: {}", name);
		Err(CompileError(
			line!(),
			"Name resolution failed? Shouldn't be checked by now?",
		))
	}
}

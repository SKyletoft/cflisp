use std::{borrow::Cow, collections::HashMap};

use crate::*;

///Technically illegal address for use in register -> register transfers
pub(crate) const ABOVE_STACK_OFFSET: isize = -1;

///Name (lifetime from source code), (Type, Stack position (from the bottom))
type Variables<'a, 'b> = &'b mut HashMap<Cow<'a, str>, (NativeType, isize)>;

///Name (lifetime from source code), Type
type GlobalVariables<'a, 'b> = &'b mut HashMap<Cow<'a, str>, NativeType>;

///Name, Argument list (not named)
type Functions<'a, 'b> = &'b mut HashMap<Cow<'a, str>, &'a [NativeVariable<'a>]>;

#[derive(Debug, PartialEq)]
struct State<'a, 'b, 'c, 'd, 'e, 'f> {
	variables: Variables<'a, 'b>,
	global_variables: GlobalVariables<'a, 'c>,
	functions: Functions<'a, 'd>,
	stack_size: &'f mut isize,
	scope_name: &'e str,
	line_id: usize,
}

pub fn compile<'a>(
	program: &'a [LanguageElementStructless],
	flags: &Flags,
) -> Result<Vec<CommentedInstruction<'a>>, CompileError> {
	compile_elements(
		program,
		&mut State {
			variables: &mut HashMap::new(),
			global_variables: &mut HashMap::new(),
			functions: &mut HashMap::new(),
			scope_name: "global",
			stack_size: &mut 0,
			line_id: 0,
		},
		0,
		flags.optimise >= 1,
	)
}

fn compile_elements<'a>(
	block: &'a [LanguageElementStructless],
	state: &mut State<'a, '_, '_, '_, '_, '_>,
	stack_base: isize,
	optimise: bool,
) -> Result<Vec<CommentedInstruction<'a>>, CompileError> {
	let mut instructions = vec![(Instruction::Label(state.scope_name.to_string()), None)];
	for (i, e) in block.iter().enumerate() {
		let mut new_state = State {
			variables: state.variables,
			global_variables: state.global_variables,
			functions: state.functions,
			stack_size: state.stack_size,
			scope_name: state.scope_name,
			line_id: i,
		};
		let line = &mut compile_element(e, &mut new_state, stack_base, optimise)?;
		if optimise {
			optimise_flisp::all_optimisations(line)?;
		}
		instructions.append(line);
	}
	Ok(instructions)
}

//Any error returned from here is an internal error or a naming issue that should've already been caught
// by the (potentially disabled) type checker
fn compile_element<'a>(
	element: &'a LanguageElementStructless,
	state: &mut State<'a, '_, '_, '_, '_, '_>,
	stack_base: isize,
	optimise: bool,
) -> Result<Vec<CommentedInstruction<'a>>, CompileError> {
	let res = match element {
		LanguageElementStructless::StructDeclaration { name, is_static } => {
			if state.scope_name == "global" || *is_static {
				if state.scope_name == "global" && !is_static {
					return Err(CompileError(line!(), "Nonstatic global variable!"));
				}
				if state.global_variables.contains_key(name) {
					dbg!(element);
					return Err(CompileError(line!(), "Name already exists in scope!"));
				}
				state
					.global_variables
					.insert(name.clone(), NativeType::Void);
			} else {
				if state.variables.contains_key(name) {
					dbg!(element);
					return Err(CompileError(
						line!(),
						"Name already exists in scope! (No shadowing)",
					));
				}
				state
					.variables
					.insert(name.clone(), (NativeType::Void, *state.stack_size));
			}
			vec![(Instruction::Label(name.to_string()), None)]
		}
		LanguageElementStructless::VariableDeclaration {
			typ,
			name,
			is_static,
		} => {
			if state.scope_name == "global" || *is_static {
				if state.scope_name == "global" && !is_static {
					return Err(CompileError(line!(), "Nonstatic global variable!"));
				}
				if state.global_variables.contains_key(name) {
					dbg!(element);
					return Err(CompileError(line!(), "Name already exists in scope!"));
				}
				state.global_variables.insert(name.clone(), typ.clone());
				vec![]
			} else {
				if state.variables.contains_key(name) {
					dbg!(element);
					return Err(CompileError(
						line!(),
						"Name already exists in scope! (No shadowing)",
					));
				}
				state
					.variables
					.insert(name.clone(), (typ.clone(), *state.stack_size));
				*state.stack_size += 1;
				vec![(
					Instruction::LEASP(Addressing::SP(ABOVE_STACK_OFFSET)),
					Some(Cow::Borrowed(name.as_ref())),
				)]
			}
		}

		LanguageElementStructless::VariableAssignment { name, value } => {
			if state.scope_name == "global" {
				return Err(CompileError(line!(), "Lone statement in global scope"));
			}
			let adr = adr_for_name(
				name,
				state.variables,
				state.global_variables,
				*state.stack_size,
			)?;
			let mut statement = compile_statement(value, state)?;
			statement.push((Instruction::STA(adr), Some(Cow::Borrowed(name.as_ref()))));
			statement
		}

		LanguageElementStructless::VariableDeclarationAssignment {
			typ,
			name,
			value,
			is_static,
		} => {
			let global_def = |val: &StatementElementStructless| match val {
				StatementElementStructless::Num(n) => Ok(*n),
				StatementElementStructless::Char(c) => Ok(*c as isize),
				StatementElementStructless::Bool(b) => Ok(*b as isize),
				_ => Err(CompileError(
					line!(),
					"Non constant in array initialisation",
				)),
			};
			if state.scope_name == "global" || *is_static {
				if state.scope_name == "global" && !is_static {
					return Err(CompileError(line!(), "Nonstatic global variable!"));
				}
				if state.global_variables.contains_key(name) {
					dbg!(element);
					return Err(CompileError(line!(), "Name already exists in scope!"));
				}
				if let StatementElementStructless::Array(elements) = value {
					state
						.global_variables
						.insert(Cow::Borrowed(name.as_ref()), typ.clone());
					let values = elements
						.iter()
						.map(global_def)
						.collect::<Result<Vec<isize>, CompileError>>()?;
					vec![
						(Instruction::Label(name.to_string()), None),
						(Instruction::FCB(values), None),
					]
				} else {
					state
						.global_variables
						.insert(Cow::Borrowed(name.as_ref()), typ.clone());
					vec![
						(Instruction::Label(name.to_string()), None),
						(Instruction::FCB(vec![global_def(value)?]), None),
					]
				}
			} else {
				if state.variables.contains_key(name) || state.global_variables.contains_key(name) {
					dbg!(element);
					return Err(CompileError(
						line!(),
						"Name already exists in scope! (No shadowing)",
					));
				}
				if let StatementElementStructless::Array(elements) = value {
					let mut statement = Vec::new();
					for element in elements {
						let stack_copy = *state.stack_size;
						statement.append(&mut compile_statement(element, state)?);
						assert_eq!(*state.stack_size, stack_copy);
						*state.stack_size += 1;
						statement.push((Instruction::PSHA, Some(Cow::Borrowed(name.as_ref()))));
					}
					state.variables.insert(
						Cow::Borrowed(name.as_ref()),
						(typ.clone(), *state.stack_size),
					);
					statement
				} else {
					let stack_copy = *state.stack_size;
					let mut statement = compile_statement(value, state)?;
					assert_eq!(*state.stack_size, stack_copy);
					state.variables.insert(
						Cow::Borrowed(name.as_ref()),
						(typ.clone(), *state.stack_size),
					);
					*state.stack_size += 1;
					statement.push((Instruction::PSHA, Some(Cow::Borrowed(name.as_ref()))));
					statement
				}
			}
		}

		LanguageElementStructless::PointerAssignment { ptr, value } => {
			if state.scope_name == "global" {
				return Err(CompileError(line!(), "Lone statement in global scope"));
			}
			let get_adr = compile_statement(ptr, state)?;
			let mut value = compile_statement(value, state)?;
			let mut statement = get_adr;
			statement.push((
				Instruction::STA(Addressing::SP(ABOVE_STACK_OFFSET)),
				Some("A to X part 1".into()),
			));
			statement.push((
				Instruction::LDX(Addressing::SP(ABOVE_STACK_OFFSET)),
				Some("A to X part 2".into()),
			));
			statement.append(&mut value);
			statement.push((Instruction::STA(Addressing::Xn(0)), None));
			statement
		}

		LanguageElementStructless::FunctionDeclaration {
			typ: _,
			name,
			args,
			block,
		} => {
			if state.functions.contains_key(name) {
				return Err(CompileError(
					line!(),
					"Function name already exists in scope!",
				));
			}
			state
				.functions
				.insert(Cow::Borrowed(name.as_ref()), args.as_slice());
			//Stack: Start -> Arguments -> Return adr -> Variables -> Whatever the stack grows to
			// If this +1 is removed we can no longer have arguments
			// This might waste a byte if there are no arguments?
			let mut args_count = args.len() as isize + 1;
			let args_base = args_count;
			let mut local_variables = HashMap::new();
			for (idx, NativeVariable { name, typ }) in args.iter().enumerate() {
				local_variables.insert(name.clone(), (typ.clone(), idx as isize));
			}
			let mut function_body = compile_elements(
				block,
				&mut State {
					variables: &mut local_variables,
					global_variables: state.global_variables,
					functions: state.functions,
					scope_name: name,
					stack_size: &mut args_count,
					line_id: state.line_id,
				},
				args_base,
				false,
			)?;
			if args_count != args_base {
				function_body.push((
					Instruction::LEASP(Addressing::SP(args_count - args_base)),
					Some("Clearing variables".into()),
				));
			}
			function_body.push((Instruction::RTS, None));
			function_body
		}

		LanguageElementStructless::IfStatement {
			condition,
			then,
			else_then,
		} => {
			let else_str = format!("if_else_{}_{}", state.scope_name, state.line_id);
			let end_str = format!("if_end_{}_{}", state.scope_name, state.line_id);
			let mut cond = compile_statement(condition, state)?;
			cond.push((Instruction::TSTA, None));
			let mut then_block = compile_element(then, state, stack_base, optimise)?;
			if let Some(v) = else_then {
				let mut else_block = compile_element(v, state, stack_base, optimise)?;
				cond.push((Instruction::BEQ(Addressing::Label(else_str)), None));
				cond.append(&mut then_block);
				cond.push((Instruction::JMP(Addressing::Label(end_str.clone())), None));
				cond.append(&mut else_block);
			} else {
				cond.push((Instruction::BEQ(Addressing::Label(end_str.clone())), None));
				cond.append(&mut then_block);
			}
			cond.push((Instruction::Label(end_str), None));
			cond
		}

		LanguageElementStructless::Loop { condition, body } => {
			let cond_str = format!("cond_{}", state.scope_name);
			let end_str = format!("end_{}", state.scope_name);
			let mut instructions = vec![(Instruction::Label(cond_str.clone()), None)];
			instructions.append(&mut compile_statement(condition, state)?);
			instructions.push((Instruction::TSTA, None));
			instructions.push((Instruction::BEQ(Addressing::Label(end_str.clone())), None));
			instructions.append(&mut compile_element(body, state, stack_base, optimise)?);
			instructions.push((Instruction::JMP(Addressing::Label(cond_str)), None));
			instructions.push((Instruction::Label(end_str), None));
			instructions
		}

		LanguageElementStructless::Return(ret) => {
			let stack_clear = (
				Instruction::LEASP(Addressing::SP(*state.stack_size - stack_base)),
				None,
			);
			if let Some(statement) = ret {
				let mut statement = compile_statement(statement, state)?;
				statement.push(stack_clear);
				statement.push((Instruction::RTS, None));
				statement
			} else {
				vec![stack_clear, (Instruction::RTS, None)]
			}
		}

		LanguageElementStructless::Statement(statement) => {
			if state.scope_name == "global" {
				return Err(CompileError(line!(), "Lone statement in global scope"));
			}
			compile_statement(statement, state)?
		}

		LanguageElementStructless::Block { block, scope_name } => {
			let mut inner_variables = state.variables.clone();
			let mut inner_stack = *state.stack_size;
			let scope_name = format!("{}_{}_{}", scope_name, state.scope_name, state.line_id);
			let mut inner_state = State {
				variables: &mut inner_variables,
				global_variables: state.global_variables,
				functions: state.functions,
				scope_name: &scope_name,
				stack_size: &mut inner_stack,
				line_id: state.line_id,
			};
			let mut instructions = compile_elements(block, &mut inner_state, stack_base, optimise)?;
			let stack_diff = inner_stack - *state.stack_size;
			if stack_diff != 0 {
				instructions.push((Instruction::LEASP(Addressing::SP(stack_diff)), None));
			}
			instructions
		}
	};
	Ok(res)
}

fn compile_statement<'a>(
	statement: &'a StatementElementStructless,
	state: &mut State<'a, '_, '_, '_, '_, '_>,
) -> Result<Vec<CommentedInstruction<'a>>, CompileError> {
	let tmps = (statement.depth() as isize - 2).max(0); //Minus 2 because bottom is value and ops keep one value in memory
	let mut statement_instructions = compile_statement_inner(
		statement,
		&mut State {
			variables: state.variables,
			global_variables: state.global_variables,
			functions: state.functions,
			stack_size: &mut (tmps + *state.stack_size),
			scope_name: state.scope_name,
			line_id: state.line_id,
		},
		&mut tmps.clone(), //Will start by -1.
		                   // Counts down so that positions closest to the stack are used first which helps optimisation to save memory
	)?;
	if tmps > 0 {
		let mut block = vec![(
			Instruction::LEASP(Addressing::SP(-tmps)),
			Some(Cow::Borrowed("Reserving memory for statement")),
		)];
		block.append(&mut statement_instructions);
		block.append(&mut vec![(
			Instruction::LEASP(Addressing::SP(tmps)), //why not -1?
			Some(Cow::Borrowed("Clearing memory for statement")),
		)]);
		statement_instructions = block;
	};

	Ok(statement_instructions)
}

fn compile_statement_inner<'a>(
	statement: &'a StatementElementStructless,
	state: &mut State<'a, '_, '_, '_, '_, '_>,
	tmps_used: &mut isize,
) -> Result<Vec<CommentedInstruction<'a>>, CompileError> {
	let instructions = match statement {
		//Commutative operations
		StatementElementStructless::Add { lhs, rhs }
		| StatementElementStructless::Mul { lhs, rhs }
		| StatementElementStructless::And { lhs, rhs }
		| StatementElementStructless::Or { lhs, rhs }
		| StatementElementStructless::Xor { lhs, rhs }
		| StatementElementStructless::NotCmp { lhs, rhs }
		| StatementElementStructless::Cmp { lhs, rhs } => {
			let left_depth = lhs.depth();
			let right_depth = rhs.depth();
			let (left, right) = if left_depth > right_depth {
				(lhs.as_ref(), rhs.as_ref())
			} else {
				(rhs.as_ref(), lhs.as_ref())
			};
			let mut instructions = compile_statement_inner(left, state, tmps_used)?;
			let mut right_instructions_plus_one = || {
				*tmps_used -= 1;
				let res = compile_statement_inner(
					right,
					&mut State {
						variables: state.variables,
						global_variables: state.global_variables,
						functions: state.functions,
						stack_size: &mut (1 + *state.stack_size),
						scope_name: state.scope_name,
						line_id: state.line_id,
					},
					tmps_used,
				);
				*tmps_used += 1;
				res
			};
			match statement {
				StatementElementStructless::Mul { rhs: _, lhs: _ } => {
					instructions.push((Instruction::PSHA, Some(Cow::Borrowed("mul rhs"))));
					instructions.append(&mut right_instructions_plus_one()?);
					instructions.push((
						Instruction::JSR(Addressing::Label("__mul__".to_string())),
						None,
					));
					instructions.push((Instruction::LEASP(Addressing::SP(1)), None));
				}
				StatementElementStructless::Cmp { rhs: _, lhs: _ } => {
					instructions.push((Instruction::PSHA, Some("cmp rhs".into())));
					instructions.append(&mut right_instructions_plus_one()?);
					instructions.push((
						Instruction::JSR(Addressing::Label("__eq__".to_string())),
						None,
					));
					instructions.push((Instruction::LEASP(Addressing::SP(1)), None));
				}
				StatementElementStructless::NotCmp { rhs: _, lhs: _ } => {
					instructions.push((Instruction::PSHA, Some("cmp rhs".into())));
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
					let mut right_instructions = compile_statement_inner(right, state, tmps_used)?;
					if let [(Instruction::LDA(adr), comment)] = &right_instructions.as_slice() {
						instructions.push((
							Instruction::from_statement_element_structless(statement, adr.clone())?,
							comment.clone(),
						));
					} else {
						assert!(right_instructions.len() >= 2);
						instructions.push((Instruction::STA(Addressing::SP(*tmps_used)), None));
						instructions.append(&mut right_instructions);
						instructions.push((
							Instruction::from_statement_element_structless(
								statement,
								Addressing::SP(*tmps_used),
							)?,
							None,
						));
					}
					*tmps_used += 1;
				}
			}
			instructions
		}

		//Non-commutative operations
		StatementElementStructless::Sub { lhs, rhs }
		| StatementElementStructless::Div { lhs: rhs, rhs: lhs }
		| StatementElementStructless::Mod { lhs: rhs, rhs: lhs }
		| StatementElementStructless::LShift { lhs, rhs }
		| StatementElementStructless::RShift { lhs, rhs }
		| StatementElementStructless::GreaterThan { lhs: rhs, rhs: lhs }
		| StatementElementStructless::LessThanEqual { lhs: rhs, rhs: lhs }
		| StatementElementStructless::LessThan { lhs, rhs }
		| StatementElementStructless::GreaterThanEqual { lhs, rhs } => {
			let (left, right) = (lhs.as_ref(), rhs.as_ref());
			let mut instructions = compile_statement_inner(left, state, tmps_used)?;
			let mut right_instructions_plus_one = || {
				*tmps_used -= 1;
				let res = compile_statement_inner(
					right,
					&mut State {
						variables: state.variables,
						global_variables: state.global_variables,
						functions: state.functions,
						stack_size: &mut (1 + *state.stack_size),
						scope_name: state.scope_name,
						line_id: state.line_id,
					},
					tmps_used,
				);
				*tmps_used += 1;
				res
			};
			match statement {
				StatementElementStructless::Div { rhs: _, lhs: _ } => {
					instructions.push((Instruction::PSHA, Some("div rhs".into())));
					instructions.append(&mut right_instructions_plus_one()?);
					instructions.push((
						Instruction::JSR(Addressing::Label("__div__".to_string())),
						None,
					));
					instructions.push((Instruction::LEASP(Addressing::SP(1)), None));
				}
				StatementElementStructless::Mod { rhs: _, lhs: _ } => {
					instructions.push((Instruction::PSHA, Some("mod rhs".into())));
					instructions.append(&mut right_instructions_plus_one()?);
					instructions.push((
						Instruction::JSR(Addressing::Label("__mod__".to_string())),
						None,
					));
					instructions.push((Instruction::LEASP(Addressing::SP(1)), None));
				}
				StatementElementStructless::GreaterThan { rhs: _, lhs: _ }
				| StatementElementStructless::LessThan { rhs: _, lhs: _ } => {
					instructions.push((Instruction::PSHA, Some("gt rhs".into())));
					instructions.append(&mut right_instructions_plus_one()?);
					instructions.push((
						Instruction::JSR(Addressing::Label("__gt__".to_string())),
						None,
					));
					instructions.push((Instruction::LEASP(Addressing::SP(1)), None));
				}
				StatementElementStructless::LessThanEqual { rhs: _, lhs: _ }
				| StatementElementStructless::GreaterThanEqual { rhs: _, lhs: _ } => {
					instructions.push((Instruction::PSHA, Some("lte rhs".into())));
					instructions.append(&mut right_instructions_plus_one()?);
					instructions.push((
						Instruction::JSR(Addressing::Label("__gt__".to_string())),
						None,
					));
					instructions.push((Instruction::LEASP(Addressing::SP(1)), None));
					instructions.push((Instruction::COMA, None));
				}
				StatementElementStructless::RShift { lhs: _, rhs: _ }
				| StatementElementStructless::LShift { lhs: _, rhs: _ } => {
					let mut right_instructions = compile_statement_inner(right, state, tmps_used)?;
					if let StatementElementStructless::Num(n) = rhs.as_ref() {
						if *n < 0 {
							return Err(CompileError(line!(), "Cannot shift by negative amount"));
						}
						let inst = if matches!(
							statement,
							StatementElementStructless::RShift { lhs: _, rhs: _ }
						) {
							Instruction::LSRA
						} else {
							Instruction::LSLA
						};
						instructions.append(&mut vec![(inst, None); *n as usize]);
					} else {
						//The byte to be shifted is in SP(tmps_used) as it's the left hand side
						// and the amount of shifts are in the A register.
						let start_str =
							format!("bit_shift_start_{}_{}", state.scope_name, state.line_id);
						let end_str =
							format!("bit_shift_end_{}_{}", state.scope_name, state.line_id);
						let inst = if matches!(
							statement,
							StatementElementStructless::RShift { lhs: _, rhs: _ }
						) {
							Instruction::LSR(Addressing::SP(*tmps_used))
						} else {
							Instruction::LSL(Addressing::SP(*tmps_used))
						};
						let mut shift_loop = vec![
							(Instruction::Label(start_str.clone()), None),
							(Instruction::TSTA, None),
							(Instruction::BEQ(Addressing::Label(end_str.clone())), None),
							(inst, None),
							(Instruction::DECA, None),
							(Instruction::JMP(Addressing::Label(start_str)), None),
							(Instruction::Label(end_str), None),
						];
						instructions.push((Instruction::STA(Addressing::SP(*tmps_used)), None));
						instructions.append(&mut right_instructions);
						instructions.append(&mut shift_loop);
					}
				}
				_ => {
					*tmps_used -= 1;
					let mut right_instructions = compile_statement_inner(right, state, tmps_used)?;
					if let [(Instruction::LDA(adr), comment)] = &right_instructions.as_slice() {
						instructions.push((
							Instruction::from_statement_element_structless(statement, adr.clone())?,
							comment.clone(),
						));
					} else {
						assert!(instructions.len() >= 2);
						instructions.push((Instruction::STA(Addressing::SP(*tmps_used)), None));
						instructions.append(&mut right_instructions);
						instructions.push((
							Instruction::from_statement_element_structless(
								statement,
								Addressing::SP(*tmps_used),
							)?,
							None,
						));
					}
					*tmps_used += 1;
				}
			}
			instructions
		}

		StatementElementStructless::FunctionCall { name, parametres } => {
			let mut instructions = Vec::new();
			let arg_names = state.functions.get(name).ok_or_else(|| {
				dbg!(name);
				CompileError(
					line!(),
					"Name resolution failed? Shouldn't it've been checked by now?",
				)
			})?;
			for (
				statement,
				NativeVariable {
					name: v_name,
					typ: _,
				},
			) in parametres.iter().zip(
				arg_names
					.iter()
					.filter(|NativeVariable { typ, name: _ }| !matches!(typ, NativeType::Void)),
			) {
				instructions.append(&mut compile_statement(statement, state)?);
				instructions.push((
					Instruction::PSHA,
					Some(helper::merge_name_and_field(name, v_name)),
				));
			}
			instructions.push((Instruction::JSR(Addressing::Label(name.to_string())), None));
			instructions.push((
				Instruction::LEASP(Addressing::SP(parametres.len() as isize)),
				None,
			));
			instructions
		}

		StatementElementStructless::Var(name) => {
			let adr = adr_for_name(
				name,
				state.variables,
				state.global_variables,
				*state.stack_size,
			)?;
			vec![(Instruction::LDA(adr), Some(Cow::Borrowed(name.as_ref())))]
		}

		StatementElementStructless::Num(n) => {
			vec![(Instruction::LDA(Addressing::Data(*n)), None)]
		}

		StatementElementStructless::Char(c) => {
			vec![(Instruction::LDA(Addressing::Data(*c as isize)), None)]
		}

		StatementElementStructless::Bool(b) => {
			vec![(Instruction::LDA(Addressing::Data(*b as isize)), None)]
		}

		StatementElementStructless::Array(_) => return Err(CompileError(line!(), "Illegal array")),

		StatementElementStructless::Deref(adr) => {
			match (adr.as_ref(), adr.internal_ref()) {
				//Array opt
				(
					&StatementElementStructless::Add { lhs: _, rhs: _ },
					Some((
						StatementElementStructless::Var(name),
						&StatementElementStructless::Num(offset),
					)),
				) => {
					let adr = adr_for_name(
						name.as_ref(),
						state.variables,
						state.global_variables,
						*state.stack_size,
					)?;
					vec![
						(Instruction::LDY(adr), Some(Cow::Borrowed(name.as_ref()))),
						(Instruction::LDA(Addressing::Yn(offset)), None),
					]
				}
				//General opt
				(
					&StatementElementStructless::Add { lhs: _, rhs: _ },
					Some((StatementElementStructless::Var(name), rhs)),
				) => {
					let adr = adr_for_name(
						name.as_ref(),
						state.variables,
						state.global_variables,
						*state.stack_size,
					)?;
					let mut statement = compile_statement(rhs, state)?;
					statement.append(&mut vec![
						(Instruction::LDY(adr), Some(Cow::Borrowed(name.as_ref()))),
						(Instruction::LDA(Addressing::AY), None),
					]);
					statement
				}
				//General case
				_ => {
					let mut instructions = compile_statement_inner(adr.as_ref(), state, tmps_used)?;
					instructions.push((
						Instruction::STA(Addressing::SP(ABOVE_STACK_OFFSET)),
						Some("A to X transfer".into()),
					));
					instructions.push((
						Instruction::LDX(Addressing::SP(ABOVE_STACK_OFFSET)),
						Some("A to X  continued".into()),
					));
					instructions.push((Instruction::LDA(Addressing::Xn(0)), None));
					instructions
				}
			}
		}

		StatementElementStructless::AdrOf(name) => {
			//use adr_for_name and calculate the address -> A
			let adr = adr_for_name(
				name,
				state.variables,
				state.global_variables,
				*state.stack_size,
			)?;
			match adr {
				Addressing::SP(n) => {
					vec![
						(
							Instruction::STSP(Addressing::SP(ABOVE_STACK_OFFSET)),
							Some(Cow::Borrowed("SP -> Stack")),
						),
						(
							Instruction::ADDA(Addressing::SP(n)),
							Some(Cow::Borrowed(name.as_ref())),
						),
					]
				}
				Addressing::Adr(n) => {
					vec![(
						Instruction::LDA(Addressing::Data(n)),
						Some(Cow::Borrowed(name.as_ref())),
					)]
				}
				Addressing::Label(lbl) => {
					vec![(Instruction::LDA(Addressing::DataLabel(lbl)), None)]
				}
				_ => return Err(CompileError(line!(), "Illegal access type")),
			}
		}

		StatementElementStructless::Not { lhs } => {
			let mut instructions = compile_statement_inner(lhs, state, tmps_used)?;
			instructions.push((Instruction::COMA, None));
			instructions
		}
	};
	Ok(instructions)
}

///Returns addressing for a name. Stack offset for locals and a Label for globals (and not an absolute address)
fn adr_for_name<'a>(
	name: &'a str,
	variables: &HashMap<Cow<'a, str>, (NativeType, isize)>,
	global_variables: &HashMap<Cow<'a, str>, NativeType>,
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
			"Name resolution failed? Shouldn't that have be checked by now?",
		))
	}
}

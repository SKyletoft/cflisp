use std::{borrow::Cow, collections::HashMap};

use super::*;
use crate::*;

///Technically illegal address for use in register -> register transfers
pub(crate) const ABOVE_STACK_OFFSET: isize = -1;

///Name (lifetime from source code), (Type, Stack position (from the bottom))
type Variables<'a, 'b> = &'b mut HashMap<Cow<'a, str>, (NativeType, isize)>;

///Name (lifetime from source code), Type
type GlobalVariables<'a, 'b> = &'b mut HashMap<Cow<'a, str>, NativeType>;

///Name, Argument list (not named)
type Functions<'a, 'b> = &'b mut HashMap<Cow<'a, str>, &'a [NativeVariable<'a>]>;

#[allow(clippy::type_complexity)]
const STANDARD_FUNCTIONS: [(
	&str,
	&dyn for<'a> Fn(
		&'a [StructlessStatement<'a>],
		&mut State<'a, '_, '_, '_, '_>,
		&mut isize,
		bool,
	) -> Result<Vec<CommentedInstruction<'a>>>,
); 4] = [
	("swap", &swap),
	("mirror_in_place", &mirror),
	("read_str", &read_string),
	("print", &print),
];

#[derive(Debug, PartialEq)]
pub(crate) struct State<'a, 'b, 'c, 'd, 'e> {
	variables: Variables<'a, 'b>,
	global_variables: GlobalVariables<'a, 'c>,
	functions: Functions<'a, 'd>,
	stack_size: &'e mut isize,
	scope_name: Cow<'a, str>,
	line_id: usize,
}

pub fn compile<'a>(
	program: &'a [StructlessLanguage<'a>],
	flags: &Flags,
) -> Result<Vec<CommentedInstruction<'a>>> {
	let mut functions: HashMap<Cow<'a, str>, &[NativeVariable]> = HashMap::new();
	functions.insert(
		Cow::Borrowed("__tb__"),
		&[NativeVariable {
			name: Cow::Borrowed("val"),
			typ: NativeType::Int,
		}],
	);
	compile_elements(
		program,
		&mut State {
			variables: &mut HashMap::new(),
			global_variables: &mut HashMap::new(),
			functions: &mut functions,
			scope_name: Cow::Borrowed("global"),
			stack_size: &mut 0,
			line_id: 0,
		},
		0,
		flags.optimise >= 1,
		true,
	)
}

fn compile_elements<'a>(
	block: &'a [StructlessLanguage<'a>],
	state: &mut State<'a, '_, '_, '_, '_>,
	stack_base: isize,
	optimise: bool,
	std_functions: bool,
) -> Result<Vec<CommentedInstruction<'a>>> {
	let mut instructions = vec![(Instruction::Label(state.scope_name.clone()), None)];
	for (i, e) in block.iter().enumerate() {
		let mut new_state = State {
			variables: state.variables,
			global_variables: state.global_variables,
			functions: state.functions,
			stack_size: state.stack_size,
			scope_name: state.scope_name.clone(),
			line_id: i,
		};
		let line = &mut compile_element(e, &mut new_state, stack_base, optimise, std_functions)?;
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
	element: &'a StructlessLanguage<'a>,
	state: &mut State<'a, '_, '_, '_, '_>,
	stack_base: isize,
	optimise: bool,
	std_functions: bool,
) -> Result<Vec<CommentedInstruction<'a>>> {
	let res = match element {
		StructlessLanguage::VariableLabelTag {
			name, is_static, ..
		} => {
			if state.scope_name == "global" || *is_static {
				if state.global_variables.contains_key(name) {
					dbg!(element);
					return Err(error!(DuplicateName, element));
				}
				state
					.global_variables
					.insert(name.clone(), NativeType::Void);
				vec![(Instruction::Label(name.clone()), None)]
			} else {
				if state.variables.contains_key(name) {
					dbg!(element);
					return Err(error!(DuplicateName, element));
				}
				state
					.variables
					.insert(name.clone(), (NativeType::Void, *state.stack_size));
				vec![]
			}
		}
		StructlessLanguage::VariableDeclaration {
			typ,
			name,
			is_static,
			..
		} => {
			if state.scope_name == "global" || *is_static {
				if state.global_variables.contains_key(name) {
					dbg!(element);
					return Err(error!(DuplicateName, element));
				}
				state
					.global_variables
					.insert(Cow::Borrowed(name.as_ref()), typ.clone());
				vec![
					(Instruction::Label(name.clone()), None),
					(Instruction::FCB(vec![0]), None),
				]
			} else {
				if state.variables.contains_key(name) {
					dbg!(element);
					return Err(error!(DuplicateName, element));
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

		StructlessLanguage::VariableAssignment { name, value } => {
			if state.scope_name == "global" {
				return Err(error!(LoneGlobalStatement, element));
			}
			let adr = adr_for_name(
				name,
				state.variables,
				state.global_variables,
				*state.stack_size,
			)?;
			let mut statement = compile_statement(value, state, std_functions)?;
			statement.push((Instruction::STA(adr), Some(Cow::Borrowed(name.as_ref()))));
			statement
		}

		StructlessLanguage::VariableDeclarationAssignment {
			typ,
			name,
			value,
			is_static,
			..
		} => {
			let global_def = |val: &StructlessStatement| match val {
				StructlessStatement::Num(n) => Ok(n.val),
				StructlessStatement::Char(c) => Ok(*c as isize),
				StructlessStatement::Bool(b) => Ok(*b as isize),
				_ => {
					return Err(error!(NonConstInConstInit, element));
				}
			};
			if state.scope_name == "global" || *is_static {
				if state.global_variables.contains_key(name) {
					return Err(error!(DuplicateName, element));
				}
				state
					.global_variables
					.insert(Cow::Borrowed(name.as_ref()), typ.clone());
				if let StructlessStatement::Array(elements) = value {
					let values = elements
						.iter()
						.map(global_def)
						.collect::<Result<Vec<isize>>>()?;
					vec![
						(Instruction::Label(name.clone()), None),
						(Instruction::FCB(values), None),
					]
				} else {
					vec![
						(Instruction::Label(name.clone()), None),
						(Instruction::FCB(vec![global_def(value)?]), None),
					]
				}
			} else {
				if state.variables.contains_key(name) || state.global_variables.contains_key(name) {
					return Err(error!(DuplicateName, element));
				}
				if let StructlessStatement::Array(elements) = value {
					let mut statement = Vec::new();
					//Rev because the stack grows down and we don't want to invert indexing
					for element in elements.iter().rev() {
						let stack_copy = *state.stack_size;
						statement.append(&mut compile_statement(element, state, std_functions)?);
						assert_eq!(*state.stack_size, stack_copy);
						*state.stack_size += 1;
						statement.push((Instruction::PSHA, Some(Cow::Borrowed(name.as_ref()))));
					}
					state.variables.insert(
						Cow::Borrowed(name.as_ref()),
						// -1 because it should go before the incrementation of stack_size, but
						// we can't do that here due to it being in the loop.
						(typ.clone(), *state.stack_size - 1),
					);
					statement
				} else {
					let stack_copy = *state.stack_size;
					let mut statement = compile_statement(value, state, std_functions)?;
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

		StructlessLanguage::PointerAssignment { ptr, value } => {
			if state.scope_name == "global" {
				return Err(error!(LoneGlobalStatement, element));
			}

			match (ptr, ptr.internal_ref()) {
				(
					StructlessStatement::BinOp { op: BinOp::Add, .. },
					Some((StructlessStatement::Num(idx), StructlessStatement::Var(name)))
					| Some((StructlessStatement::Var(name), StructlessStatement::Num(idx))),
				) => {
					let mut statement = compile_statement(value, state, std_functions)?;
					statement.append(&mut vec![
						(
							Instruction::LDX(adr_for_name(
								name,
								state.variables,
								state.global_variables,
								*state.stack_size,
							)?),
							None,
						),
						(
							Instruction::STA(Addressing::Xn(idx.val)),
							Some(name.clone()),
						),
					]);
					statement
				}

				_ => {
					let mut value = compile_statement(value, state, std_functions)?;
					let mut load_ptr = compile_statement(ptr, state, std_functions)?;
					load_ptr.push((Instruction::PSHA, Some(Cow::Borrowed("A to X part 1"))));
					load_ptr.push((Instruction::PULX, Some(Cow::Borrowed("A to X part 2"))));
					load_ptr.append(&mut value);
					load_ptr.push((Instruction::STA(Addressing::Xn(0)), None));
					load_ptr
				}
			}
		}

		StructlessLanguage::FunctionDeclaration {
			name, args, block, ..
		} => {
			if state.functions.contains_key(name) {
				return Err(error!(DuplicateName, element));
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
					scope_name: name.clone(),
					stack_size: &mut args_count,
					line_id: state.line_id,
				},
				args_base,
				false,
				std_functions,
			)?;
			function_body.insert(0, (Instruction::Label(Cow::Borrowed("\n")), None));
			if args_count != args_base {
				function_body.push((
					Instruction::LEASP(Addressing::SP(args_count - args_base)),
					Some(Cow::Borrowed("Clearing variables")),
				));
			}
			if !matches!(function_body.last(), Some((Instruction::RTS, _))) {
				function_body.push((Instruction::RTS, None));
			}
			if name == "interrupt" {
				optimise_flisp::make_interrupt_return(&mut function_body);
			}
			function_body
		}

		StructlessLanguage::IfStatement {
			condition,
			then,
			else_then,
		} => {
			let else_str = Cow::Owned(format!("if_else_{}_{}", state.scope_name, state.line_id));
			let end_str = format!("if_end_{}_{}", state.scope_name, state.line_id);
			let mut cond = compile_statement(condition, state, std_functions)?;
			cond.push((Instruction::TSTA, None));
			let mut then_block = compile_element(then, state, stack_base, optimise, std_functions)?;
			if let Some(v) = else_then {
				let mut else_block =
					compile_element(v, state, stack_base, optimise, std_functions)?;
				cond.push((Instruction::BEQ(Addressing::Label(else_str)), None));
				cond.append(&mut then_block);
				cond.push((
					Instruction::JMP(Addressing::Label(Cow::Owned(end_str.clone()))),
					None,
				));
				cond.append(&mut else_block);
			} else {
				cond.push((
					Instruction::BEQ(Addressing::Label(Cow::Owned(end_str.clone()))),
					None,
				));
				cond.append(&mut then_block);
			}
			cond.push((Instruction::Label(Cow::Owned(end_str)), None));
			cond
		}

		StructlessLanguage::Loop { condition, body } => {
			let cond_str = format!("cond_{}", state.scope_name);
			let end_str = format!("end_{}", state.scope_name);
			let mut instructions = vec![(Instruction::Label(Cow::Owned(cond_str.clone())), None)];
			instructions.append(&mut compile_statement(condition, state, std_functions)?);
			instructions.push((Instruction::TSTA, None));
			instructions.push((
				Instruction::BEQ(Addressing::Label(Cow::Owned(end_str.clone()))),
				None,
			));
			instructions.append(&mut compile_element(
				body,
				state,
				stack_base,
				optimise,
				std_functions,
			)?);
			instructions.push((
				Instruction::JMP(Addressing::Label(Cow::Owned(cond_str))),
				None,
			));
			instructions.push((Instruction::Label(Cow::Owned(end_str)), None));
			instructions
		}

		StructlessLanguage::Return(ret) => {
			let stack_clear = (
				Instruction::LEASP(Addressing::SP(*state.stack_size - stack_base)),
				Some(Cow::Borrowed("Clearing variables")),
			);
			if let Some(statement) = ret {
				let mut statement = compile_statement(statement, state, std_functions)?;
				statement.push(stack_clear);
				statement.push((Instruction::RTS, None));
				statement
			} else {
				vec![stack_clear, (Instruction::RTS, None)]
			}
		}

		StructlessLanguage::Statement(statement) => {
			if state.scope_name == "global" {
				return Err(error!(LoneGlobalStatement, element));
			}
			compile_statement(statement, state, std_functions)?
		}

		StructlessLanguage::Block { block, scope_name } => {
			let mut inner_variables = state.variables.clone();
			let mut inner_stack = *state.stack_size;
			let scope_name = format!("{}_{}_{}", scope_name, state.scope_name, state.line_id);
			let mut inner_state = State {
				variables: &mut inner_variables,
				global_variables: state.global_variables,
				functions: state.functions,
				scope_name: Cow::Owned(scope_name),
				stack_size: &mut inner_stack,
				line_id: state.line_id,
			};
			let mut instructions =
				compile_elements(block, &mut inner_state, stack_base, optimise, std_functions)?;
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
	statement: &'a StructlessStatement<'a>,
	state: &mut State<'a, '_, '_, '_, '_>,
	std_functions: bool,
) -> Result<Vec<CommentedInstruction<'a>>> {
	let tmps = (statement.depth() as isize - 2).max(0); //Minus 2 because bottom is value and ops keep one value in memory
	let mut statement_instructions = compile_statement_inner(
		statement,
		&mut State {
			variables: state.variables,
			global_variables: state.global_variables,
			functions: state.functions,
			stack_size: &mut (tmps + *state.stack_size),
			scope_name: state.scope_name.clone(),
			line_id: state.line_id,
		},
		&mut tmps.clone(), //Will start by -1.
		// Counts down so that positions closest to the stack are used first which helps optimisation to save memory
		std_functions,
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
	statement: &'a StructlessStatement,
	state: &mut State<'a, '_, '_, '_, '_>,
	tmps_used: &mut isize,
	std_functions: bool,
) -> Result<Vec<CommentedInstruction<'a>>> {
	let instructions = match statement {
		//Commutative operations
		StructlessStatement::BinOp {
			op:
				op
				@
				(BinOp::Add
				| BinOp::Mul
				| BinOp::And
				| BinOp::Or
				| BinOp::Xor
				| BinOp::NotCmp
				| BinOp::Cmp),
			lhs,
			rhs,
			..
		} => {
			let left_depth = lhs.depth();
			let right_depth = rhs.depth();
			let (left, right) = if left_depth > right_depth {
				(lhs.as_ref(), rhs.as_ref())
			} else {
				(rhs.as_ref(), lhs.as_ref())
			};
			let mut instructions = compile_statement_inner(left, state, tmps_used, std_functions)?;
			let mut right_instructions_plus_one = || {
				*tmps_used -= 1;
				let res = compile_statement_inner(
					right,
					&mut State {
						variables: state.variables,
						global_variables: state.global_variables,
						functions: state.functions,
						stack_size: &mut (1 + *state.stack_size),
						scope_name: state.scope_name.clone(),
						line_id: state.line_id,
					},
					tmps_used,
					std_functions,
				);
				*tmps_used += 1;
				res
			};
			let mut op_as_function_call =
				|function: &'a str, comment: &'a str, negate_result: bool| {
					instructions.push((Instruction::PSHA, Some(Cow::Borrowed(comment))));
					instructions.append(&mut right_instructions_plus_one()?);
					instructions.push((
						Instruction::JSR(Addressing::Label(Cow::Borrowed(function))),
						None,
					));
					instructions.push((Instruction::LEASP(Addressing::SP(1)), None));
					if negate_result {
						instructions.push((Instruction::COMA, None));
					}
					Ok(())
				};
			match op {
				BinOp::Mul => op_as_function_call("__mul__", "mul rhs", false)?,
				BinOp::Cmp => op_as_function_call("__eq__", "cmp rhs", false)?,
				BinOp::NotCmp => op_as_function_call("__eq__", "cmp rhs", true)?,

				//default:
				_ => {
					*tmps_used -= 1;
					let mut right_instructions =
						compile_statement_inner(right, state, tmps_used, std_functions)?;
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
		StructlessStatement::BinOp {
			op:
				op
				@
				(BinOp::Sub
				| BinOp::Div
				| BinOp::Mod
				| BinOp::LShift
				| BinOp::RShift
				| BinOp::LessThan
				| BinOp::GreaterThanEqual),
			lhs,
			rhs,
			signedness,
		}
		| StructlessStatement::BinOp {
			op: op @ (BinOp::LessThanEqual | BinOp::GreaterThan),
			lhs: rhs,
			rhs: lhs,
			signedness,
		} => {
			let signed = matches!(*signedness, NumberType::Signed | NumberType::Unknown);
			let (left, right) = (lhs.as_ref(), rhs.as_ref());
			let mut instructions = compile_statement_inner(left, state, tmps_used, std_functions)?;
			let mut right_instructions_plus_one = || {
				*tmps_used -= 1;
				let res = compile_statement_inner(
					right,
					&mut State {
						variables: state.variables,
						global_variables: state.global_variables,
						functions: state.functions,
						stack_size: &mut (1 + *state.stack_size),
						scope_name: state.scope_name.clone(),
						line_id: state.line_id,
					},
					tmps_used,
					std_functions,
				);
				*tmps_used += 1;
				res
			};
			let mut op_as_function_call =
				|function: &'a str, comment: &'a str, negate_result: bool| {
					instructions.push((Instruction::PSHA, Some(Cow::Borrowed(comment))));
					instructions.append(&mut right_instructions_plus_one()?);
					instructions.push((
						Instruction::JSR(Addressing::Label(Cow::Borrowed(function))),
						None,
					));
					instructions.push((Instruction::LEASP(Addressing::SP(1)), None));
					if negate_result {
						instructions.push((Instruction::COMA, None));
					}
					Ok(())
				};
			match op {
				BinOp::Div => op_as_function_call("__div__", "div rhs", false)?,
				BinOp::Mod => op_as_function_call("__mod__", "mod rhs", false)?,
				BinOp::GreaterThan | BinOp::LessThan => {
					let (comment, function_name) = if signed {
						("gts rhs", "__gts_")
					} else {
						("gtu rhs", "__gtu_")
					};
					op_as_function_call(function_name, comment, false)?;
				}
				BinOp::LessThanEqual | BinOp::GreaterThanEqual => {
					let (comment, function_name) = if signed {
						("ltes rhs", "__gts_")
					} else {
						("lteu rhs", "__gtu_")
					};
					op_as_function_call(function_name, comment, true)?;
				}
				BinOp::RShift | BinOp::LShift => {
					let mut right_instructions =
						compile_statement_inner(right, state, tmps_used, std_functions)?;
					if let StructlessStatement::Num(n) = rhs.as_ref() {
						if *n < Number::ZERO {
							return Err(error!(NegativeShift, statement));
						}
						let inst = if matches!(op, BinOp::RShift) {
							Instruction::LSRA
						} else {
							Instruction::LSLA
						};
						instructions.append(&mut vec![(inst, None); n.val as usize]);
					} else {
						//The byte to be shifted is in SP(tmps_used) as it's the left hand side
						// and the amount of shifts are in the A register.
						let start_str =
							format!("bit_shift_start_{}_{}", state.scope_name, state.line_id);
						let end_str =
							format!("bit_shift_end_{}_{}", state.scope_name, state.line_id);
						let inst = if matches!(op, BinOp::RShift) {
							Instruction::LSR(Addressing::SP(*tmps_used))
						} else {
							Instruction::LSL(Addressing::SP(*tmps_used))
						};
						let mut shift_loop = vec![
							(Instruction::Label(Cow::Owned(start_str.clone())), None),
							(Instruction::TSTA, None),
							(
								Instruction::BEQ(Addressing::Label(Cow::Owned(end_str.clone()))),
								None,
							),
							(inst, None),
							(Instruction::DECA, None),
							(
								Instruction::JMP(Addressing::Label(Cow::Owned(start_str))),
								None,
							),
							(Instruction::Label(Cow::Owned(end_str)), None),
						];
						instructions.push((Instruction::STA(Addressing::SP(*tmps_used)), None));
						instructions.append(&mut right_instructions);
						instructions.append(&mut shift_loop);
					}
				}
				_ => {
					*tmps_used -= 1;
					let mut right_instructions =
						compile_statement_inner(right, state, tmps_used, std_functions)?;
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

		StructlessStatement::FunctionCall { name, parametres }
			if STANDARD_FUNCTIONS
				.iter()
				.any(|(fn_name, _)| fn_name == name) =>
		{
			//Yes, we loop twice, but the list is short...
			let (_, f) = STANDARD_FUNCTIONS
				.iter()
				.find(|(fn_name, _)| fn_name == name)
				.expect("Already checked by pattern");
			f(parametres, state, tmps_used, std_functions)?
		}

		StructlessStatement::FunctionCall { name, parametres } => {
			let mut instructions = Vec::new();
			let arg_names = state
				.functions
				.get(name)
				.ok_or_else(|| error!(UndefinedVariable, statement))?;
			for (statement, NativeVariable { name: v_name, .. }) in parametres.iter().zip(
				arg_names
					.iter()
					.filter(|NativeVariable { typ, name: _ }| !matches!(typ, NativeType::Void)),
			) {
				instructions.append(&mut compile_statement(statement, state, std_functions)?);
				instructions.push((
					Instruction::PSHA,
					Some(helper::merge_name_and_field(name, v_name)),
				));
			}
			instructions.push((Instruction::JSR(Addressing::Label(name.clone())), None));
			if !parametres.is_empty() {
				instructions.push((
					Instruction::LEASP(Addressing::SP(parametres.len() as isize)),
					None,
				));
			}
			instructions
		}

		StructlessStatement::Var(name) => {
			let adr = adr_for_name(
				name,
				state.variables,
				state.global_variables,
				*state.stack_size,
			)?;
			vec![(Instruction::LDA(adr), Some(Cow::Borrowed(name.as_ref())))]
		}

		StructlessStatement::Num(n) => vec![(Instruction::LDA(Addressing::Data(n.val)), None)],

		StructlessStatement::Char(c) => {
			vec![(Instruction::LDA(Addressing::Data(*c as isize)), None)]
		}

		StructlessStatement::Bool(b) => {
			let val = if *b { 0xFF } else { 0 };
			vec![(Instruction::LDA(Addressing::Data(val)), None)]
		}

		StructlessStatement::Array(_) => {
			return Err(error!(IllegalArrayLiteral, statement));
		}

		StructlessStatement::Deref(adr) => {
			match (adr.as_ref(), adr.internal_ref()) {
				//Array opt
				(
					StructlessStatement::BinOp { op: BinOp::Add, .. },
					Some((StructlessStatement::Var(name), &StructlessStatement::Num(offset)))
					| Some((&StructlessStatement::Num(offset), StructlessStatement::Var(name))),
				) => {
					let adr = adr_for_name(
						name.as_ref(),
						state.variables,
						state.global_variables,
						*state.stack_size,
					)?;
					vec![
						(Instruction::LDY(adr), Some(Cow::Borrowed(name.as_ref()))),
						(Instruction::LDA(Addressing::Yn(offset.val)), None),
					]
				}
				//General opt
				(
					&StructlessStatement::BinOp { op: BinOp::Add, .. },
					Some((StructlessStatement::Var(name), rhs))
					| Some((rhs, StructlessStatement::Var(name))),
				) => {
					let adr = adr_for_name(
						name.as_ref(),
						state.variables,
						state.global_variables,
						*state.stack_size,
					)?;
					let mut statement = compile_statement(rhs, state, std_functions)?;
					statement.append(&mut vec![
						(Instruction::LDY(adr), Some(Cow::Borrowed(name.as_ref()))),
						(Instruction::LDA(Addressing::AY), None),
					]);
					statement
				}
				//General opt
				(
					&StructlessStatement::BinOp { op: BinOp::Add, .. },
					Some((StructlessStatement::Num(idx), rhs))
					| Some((rhs, StructlessStatement::Num(idx))),
				) => {
					let mut statement = compile_statement(rhs, state, std_functions)?;
					statement.append(&mut vec![
						(Instruction::LDY(Addressing::Data(idx.val)), None),
						(Instruction::LDA(Addressing::AY), None),
					]);
					statement
				}
				//General case
				_ => {
					let mut instructions =
						compile_statement_inner(adr.as_ref(), state, tmps_used, std_functions)?;
					instructions.push((Instruction::PSHA, Some(Cow::Borrowed("A to Y transfer"))));
					instructions
						.push((Instruction::PULY, Some(Cow::Borrowed("A to Y  continued"))));
					instructions.push((Instruction::LDA(Addressing::Yn(0)), None));
					instructions
				}
			}
		}

		StructlessStatement::AdrOf(name) => {
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
						(Instruction::LEASP(Addressing::SP(-1)), None),
						(Instruction::STSP(Addressing::SP(0)), None),
						(Instruction::PULA, None),
						(Instruction::ADDA(Addressing::Data(n)), None),
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
				_ => return Err(error!(InvalidAddressOf, statement)),
			}
		}

		StructlessStatement::Not(lhs) => {
			let mut instructions = compile_statement_inner(lhs, state, tmps_used, std_functions)?;
			instructions.push((Instruction::COMA, None));
			instructions
		}
	};
	Ok(instructions)
}

///Returns addressing for a name. Stack offset for locals and a Label for globals (and not an absolute address)
pub(crate) fn adr_for_name<'a>(
	name: &'a str,
	variables: &HashMap<Cow<'a, str>, (NativeType, isize)>,
	global_variables: &HashMap<Cow<'a, str>, NativeType>,
	stack_size: isize,
) -> Result<Addressing<'a>> {
	if let Some((_, adr)) = variables.get(name) {
		Ok(Addressing::SP(stack_size - *adr - 1))
	} else if let Some(typ) = global_variables.get(name) {
		if matches!(typ, NativeType::Ptr(_)) {
			Ok(Addressing::DataLabel(Cow::Borrowed(name)))
		} else {
			Ok(Addressing::Label(Cow::Borrowed(name)))
		}
	} else {
		return Err(error!(UndefinedVariable, name));
	}
}

/*
Template signature
pub(crate) fn _<'a>(
	parametres: &'a [StructlessStatement],
	state: &mut State<'a, '_, '_, '_, '_>,
	tmps_used: &mut isize,
	std_functions: bool,
) -> Result<Vec<CommentedInstruction<'a>>> {
	todo!()
}
*/

pub(crate) fn swap<'a>(
	parametres: &'a [StructlessStatement],
	state: &mut State<'a, '_, '_, '_, '_>,
	tmps_used: &mut isize,
	std_functions: bool,
) -> Result<Vec<CommentedInstruction<'a>>> {
	if parametres.len() != 2 {
		return Err(error!(MissingArguments));
	}
	let mut instructions =
		compile_statement_inner(&parametres[0], state, tmps_used, std_functions)?;
	instructions.push((Instruction::PSHA, None));
	instructions.push((Instruction::PULX, None));
	instructions.append(&mut compile_statement_inner(
		&parametres[1],
		state,
		tmps_used,
		std_functions,
	)?);
	instructions.push((Instruction::PSHA, None));
	instructions.push((Instruction::PULY, None));
	instructions.push((
		Instruction::JSR(Addressing::Label(Cow::Borrowed("__swap_"))),
		None,
	));
	Ok(instructions)
}

pub(crate) fn mirror<'a>(
	parametres: &'a [StructlessStatement],
	state: &mut State<'a, '_, '_, '_, '_>,
	tmps_used: &mut isize,
	std_functions: bool,
) -> Result<Vec<CommentedInstruction<'a>>> {
	custom_abi_single_instruction_in_x(parametres, state, tmps_used, std_functions, "_mirsmal")
}

pub(crate) fn read_string<'a>(
	parametres: &'a [StructlessStatement],
	state: &mut State<'a, '_, '_, '_, '_>,
	tmps_used: &mut isize,
	std_functions: bool,
) -> Result<Vec<CommentedInstruction<'a>>> {
	custom_abi_single_instruction_in_x(parametres, state, tmps_used, std_functions, "__read_")
}

pub(crate) fn print<'a>(
	parametres: &'a [StructlessStatement],
	state: &mut State<'a, '_, '_, '_, '_>,
	tmps_used: &mut isize,
	std_functions: bool,
) -> Result<Vec<CommentedInstruction<'a>>> {
	custom_abi_single_instruction_in_x(parametres, state, tmps_used, std_functions, "_print_")
}

fn custom_abi_single_instruction_in_x<'a>(
	parametres: &'a [StructlessStatement],
	state: &mut State<'a, '_, '_, '_, '_>,
	tmps_used: &mut isize,
	std_functions: bool,
	function: &'static str,
) -> Result<Vec<CommentedInstruction<'a>>> {
	if parametres.len() != 1 {
		return Err(error!(MissingArguments));
	}
	let mut instructions =
		compile_statement_inner(&parametres[0], state, tmps_used, std_functions)?;
	instructions.push((Instruction::PSHA, None));
	instructions.push((Instruction::PULX, None));
	instructions.push((
		Instruction::JSR(Addressing::Label(Cow::Borrowed(function))),
		None,
	));
	Ok(instructions)
}

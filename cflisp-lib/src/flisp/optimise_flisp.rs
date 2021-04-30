use super::*;
use crate::*;
use compile_flisp::ABOVE_STACK_OFFSET;
use std::{borrow::Cow, cmp::Ordering, collections::HashSet, iter};

///Doesn't actually call all optimisations. It only calls those optimisations that
/// can be called on an independent code block. This excludes `remove_unused_labels`
/// and `repeat_rts`
pub fn all_optimisations(instructions: &mut Vec<CommentedInstruction>) -> Result<(), CompileError> {
	load_array_constant_index(instructions);
	load_xy(instructions);
	repeat_xy(instructions);
	repeat_load(instructions);
	reduce_reserves_redux(instructions)?;
	cmp_eq_jmp(instructions);
	cmp_neq_jmp(instructions);
	cmp_gt_jmp(instructions);
	cmp_gte_jmp(instructions);
	merge_allocs(instructions);
	nop(instructions); //Should go AFTER reduce reserves
	repeat_a(instructions);
	load_a(instructions);
	psha(instructions);
	pula(instructions);
	function_op_load_reduce(instructions);
	inc(instructions);
	inca(instructions);
	dec(instructions);
	deca(instructions);
	clr(instructions);
	clra(instructions);
	remove_post_early_return_code(instructions);

	Ok(())
}

///Replaces a load A and move to X/Y with a direct load to X/Y and
/// address load to X and write to X,0 to a write to address directly.
fn load_xy(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 3 && idx <= instructions.len() - 3 {
		if let (
			(Instruction::LDA(addressing), _),
			(Instruction::STA(Addressing::SP(ABOVE_STACK_OFFSET)), _),
			(Instruction::LDX(Addressing::SP(ABOVE_STACK_OFFSET)), _),
		) = (
			&instructions[idx],
			&instructions[idx + 1],
			&instructions[idx + 2],
		) {
			if !matches!(addressing, Addressing::AX) {
				instructions[idx].0 = Instruction::LDX(addressing.clone());
				instructions.remove(idx + 2); //Order matters!
				instructions.remove(idx + 1);
			}
		}
		if let ((Instruction::LDA(addressing), _), (Instruction::PSHA, _), (Instruction::PULX, _)) = (
			&instructions[idx],
			&instructions[idx + 1],
			&instructions[idx + 2],
		) {
			if !matches!(addressing, Addressing::AX) {
				instructions[idx].0 = Instruction::LDX(addressing.clone());
				instructions.remove(idx + 2); //Order matters!
				instructions.remove(idx + 1);
			}
		}
		if let (
			(Instruction::LDA(addressing), _),
			(Instruction::STA(Addressing::SP(ABOVE_STACK_OFFSET)), _),
			(Instruction::LDY(Addressing::SP(ABOVE_STACK_OFFSET)), _),
		) = (
			&instructions[idx],
			&instructions[idx + 1],
			&instructions[idx + 2],
		) {
			if !matches!(addressing, Addressing::AY) {
				instructions[idx].0 = Instruction::LDY(addressing.clone());
				instructions.remove(idx + 2); //Order matters!
				instructions.remove(idx + 1);
			}
		}
		if let ((Instruction::LDA(addressing), _), (Instruction::PSHA, _), (Instruction::PULY, _)) = (
			&instructions[idx],
			&instructions[idx + 1],
			&instructions[idx + 2],
		) {
			if !matches!(addressing, Addressing::AY) {
				instructions[idx].0 = Instruction::LDY(addressing.clone());
				instructions.remove(idx + 2); //Order matters!
				instructions.remove(idx + 1);
			}
		}
		if let (
			(Instruction::LDX(Addressing::Data(x_adr)), x_comment),
			(Instruction::LDA(a_adr), a_comment),
			(Instruction::STA(Addressing::Xn(0)), _),
		) = (
			&instructions[idx],
			&instructions[idx + 1],
			&instructions[idx + 2],
		) {
			let a_comment = a_comment.clone();
			let a_adr = a_adr.clone();
			let x_comment = x_comment.clone();
			let x_adr = *x_adr;
			instructions[idx] = (Instruction::LDA(a_adr), a_comment);
			instructions[idx + 1] = (Instruction::STA(Addressing::Adr(x_adr)), x_comment);
			instructions.remove(idx + 2);
		}
		if let (
			(Instruction::LDY(Addressing::Data(x_adr)), x_comment),
			(Instruction::LDA(a_adr), a_comment),
			(Instruction::STA(Addressing::Yn(0)), _),
		) = (
			&instructions[idx],
			&instructions[idx + 1],
			&instructions[idx + 2],
		) {
			let a_comment = a_comment.clone();
			let a_adr = a_adr.clone();
			let x_comment = x_comment.clone();
			let x_adr = *x_adr;
			instructions[idx] = (Instruction::LDA(a_adr), a_comment);
			instructions[idx + 1] = (Instruction::STA(Addressing::Adr(x_adr)), x_comment);
			instructions.remove(idx + 2);
		}
		idx += 1;
	}
}

///Removes loads (not to A) that are immediately overwritten.
fn repeat_load(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 2 && idx <= instructions.len() - 2 {
		if matches!(
			(&instructions[idx], &instructions[idx + 1]),
			((Instruction::LDA(_), _), (Instruction::LDA(adr), _))
			if !matches!(adr, Addressing::AX | Addressing::AY)
		) || matches!(
			(&instructions[idx], &instructions[idx + 1]),
			((Instruction::LDX(_), _), (Instruction::LDX(adr), _))
			if !matches!(adr, Addressing::AX | Addressing::Xn(_))
		) || matches!(
			(&instructions[idx], &instructions[idx + 1]),
			((Instruction::LDY(_), _), (Instruction::LDY(adr), _))
			if !matches!(adr, Addressing::AY | Addressing::Yn(_))
		) || matches!(
			(&instructions[idx], &instructions[idx + 1]),
			((Instruction::LDSP(_), _), (Instruction::LDSP(adr), _))
			if !matches!(adr, Addressing::SP(_))
		) {
			instructions.remove(idx);
		}
		idx += 1;
	}
}

///Removes repeat RTS that can occur after unused labels have been eliminated
pub fn repeat_rts(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 2 && idx <= instructions.len() - 2 {
		if matches!(
			(&instructions[idx], &instructions[idx + 1]),
			((Instruction::RTS, _), (Instruction::RTS, _))
		) {
			instructions.remove(idx);
			continue;
		}
		idx += 1;
	}
}

///Removes repeat loads to A if the value in A wasn't used
fn repeat_a(instructions: &mut Vec<CommentedInstruction>) {
	let mut last_load = usize::MAX;
	let mut idx = 0;
	while idx < instructions.len() {
		match &instructions[idx] {
			(Instruction::LDA(_), comment) => {
				if comment.is_some() && Some(comment) == instructions.get(last_load).map(|(_, c)| c)
				{
					instructions.remove(idx);
					idx -= 1;
				} else {
					last_load = idx;
				}
			}
			(Instruction::ADDA(_), _)
			| (Instruction::SUBA(_), _)
			| (Instruction::ANDA(_), _)
			| (Instruction::LSLA, _)
			| (Instruction::LSRA, _)
			| (Instruction::ORA(_), _)
			| (Instruction::EORA(_), _)
			| (Instruction::PULA, _)
			| (Instruction::COMA, None) => {
				last_load = usize::MAX;
			}
			_ => {}
		}
		idx += 1;
	}
}

///Removes repeat loads to X/Y if the value in X/Y wasn't used
fn repeat_xy(instructions: &mut Vec<CommentedInstruction>) {
	let mut last_x = isize::MIN;
	let mut last_y = isize::MIN;
	let mut sp = 0;
	let mut idx = 0;
	while idx < instructions.len() {
		match instructions[idx] {
			(Instruction::LDX(Addressing::Data(v)), _) => {
				if v == last_x {
					instructions.remove(idx);
					idx -= 1;
				} else {
					last_x = v;
				}
			}
			(Instruction::LDX(Addressing::SP(v)), _) => {
				let actual = sp + v;
				if actual == last_x {
					instructions.remove(idx);
					idx -= 1;
				} else {
					last_x = actual;
				}
			}
			(Instruction::LDY(Addressing::Data(v)), _) => {
				if v == last_y {
					instructions.remove(idx);
					idx -= 1;
				} else {
					last_y = v;
				}
			}
			(Instruction::LDY(Addressing::SP(v)), _) => {
				let actual = sp + v;
				if actual == last_y {
					instructions.remove(idx);
					idx -= 1;
				} else {
					last_y = actual;
				}
			}
			(Instruction::PSHA, _) => {
				sp -= 1;
			}
			(Instruction::PULA, _) => {
				sp += 1;
			}
			(Instruction::LEASP(Addressing::SP(n)), _) => {
				sp += n;
			}
			(Instruction::LDSP(Addressing::Data(n)), _) => {
				sp = n;
			}
			(Instruction::LEASP(_), _) | (Instruction::LDSP(_), _) => {
				last_x = isize::MIN;
				last_y = isize::MIN;
				sp = 0;
			}

			_ => {}
		}
		idx += 1;
	}
}

///Removes instructions that are equivalent with a NOP
fn nop(instructions: &mut Vec<CommentedInstruction>) {
	instructions.retain(|(i, _)| {
		!matches!(
			i,
			Instruction::ADDA(Addressing::Data(0))
				| Instruction::EORA(Addressing::Data(0))
				| Instruction::ORA(Addressing::Data(0))
				| Instruction::ANDA(Addressing::Data(isize::MAX))
				| Instruction::LEASP(Addressing::SP(0))
		)
	});
}

///Simpifies some sequences of loading to A
fn load_a(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 3 && idx <= instructions.len() - 3 {
		//Load to A the address of A
		if let (
			(Instruction::STA(Addressing::SP(ABOVE_STACK_OFFSET)), _),
			(Instruction::LDX(Addressing::SP(ABOVE_STACK_OFFSET)), _),
			(Instruction::LDA(Addressing::Xn(0)), _),
		) = (
			&instructions[idx],
			&instructions[idx + 1],
			&instructions[idx + 2],
		) {
			instructions[idx] = (Instruction::LDX(Addressing::Data(0)), None);
			instructions[idx + 2].0 = Instruction::LDA(Addressing::AX); //To keep the existing comment on the third instruction
			instructions.remove(idx + 1);
			continue;
		}
		//Load to A the address of A
		if let (
			(Instruction::PSHA, _),
			(Instruction::PULX, _),
			(Instruction::LDA(Addressing::Xn(0)), _),
		) = (
			&instructions[idx],
			&instructions[idx + 1],
			&instructions[idx + 2],
		) {
			instructions[idx] = (Instruction::LDX(Addressing::Data(0)), None);
			instructions[idx + 2].0 = Instruction::LDA(Addressing::AX); //To keep the existing comment on the third instruction
			instructions.remove(idx + 1);
			continue;
		}
		//Load to A the address of A
		if let (
			(Instruction::STA(Addressing::SP(n)), _),
			(Instruction::LDA(Addressing::SP(m)), _),
		) = (&instructions[idx], &instructions[idx + 1])
		{
			if n == m {
				instructions.remove(idx + 1);
				continue;
			}
		}
		//Load to A from SP (Array initialisation)
		if let (
			(Instruction::STSP(Addressing::SP(ABOVE_STACK_OFFSET)), _),
			(Instruction::LDA(Addressing::SP(ABOVE_STACK_OFFSET)), _),
			(Instruction::PSHA, _),
		) = (
			&instructions[idx],
			&instructions[idx + 1],
			&instructions[idx + 2],
		) {
			instructions[idx + 2].0 = Instruction::LEASP(Addressing::SP(-1)); //To keep the existing comment on the third instruction
			instructions.remove(idx + 1);
			continue;
		}
		//Remove immediate load from pushed value
		if let ((Instruction::PSHA, _), (Instruction::LDA(Addressing::SP(0)), _)) =
			(&instructions[idx], &instructions[idx + 1])
		{
			instructions.remove(idx + 1);
			continue;
		}
		//Remove some unused loads
		if let ((Instruction::LDA(_), _), (Instruction::LDA(adr), _)) =
			(&instructions[idx], &instructions[idx + 1])
		{
			if !matches!(adr, Addressing::AX | Addressing::AY) {
				instructions.remove(idx);
				continue;
			}
		}
		idx += 1;
	}
}

fn psha(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 2 && idx <= instructions.len() - 2 {
		if let (
			(Instruction::LEASP(Addressing::SP(-1)), first_comment),
			(Instruction::STA(Addressing::SP(0)), second_comment),
		) = (&instructions[idx], &instructions[idx + 1])
		{
			let comment = merge_comments!(first_comment, second_comment);
			instructions[idx] = (Instruction::PSHA, comment);
			instructions.remove(idx + 1);
			continue;
		}
		//This really shouldn't happen, but might, so let's just handle it anyway
		if let (
			(Instruction::STA(Addressing::SP(-1)), first_comment),
			(Instruction::LEASP(Addressing::SP(-1)), second_comment),
		) = (&instructions[idx], &instructions[idx + 1])
		{
			let comment = merge_comments!(first_comment, second_comment);
			instructions[idx] = (Instruction::PSHA, comment);
			instructions.remove(idx + 1);
			continue;
		}
		if let (
			(Instruction::LEASP(Addressing::SP(1)), first_comment),
			(Instruction::PSHA, second_comment),
		) = (&instructions[idx], &instructions[idx + 1])
		{
			instructions[idx] = (
				Instruction::STA(Addressing::SP(0)),
				merge_comments!(first_comment, second_comment),
			);
			instructions.remove(idx + 1);
			continue;
		}
		idx += 1;
	}
}

fn pula(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 2 && idx <= instructions.len() - 2 {
		if let (
			(Instruction::LDA(Addressing::SP(0)), first_comment),
			(Instruction::LEASP(Addressing::SP(1)), second_comment),
		) = (&instructions[idx], &instructions[idx + 1])
		{
			let comment = merge_comments!(first_comment, second_comment);
			instructions[idx] = (Instruction::PULA, comment);
			instructions.remove(idx + 1);
			idx -= 1;
			continue;
		}
		if let (
			(Instruction::LEASP(Addressing::SP(1)), first_comment),
			(Instruction::LDA(Addressing::SP(-1)), second_comment),
		) = (&instructions[idx], &instructions[idx + 1])
		{
			let comment = merge_comments!(first_comment, second_comment);
			instructions[idx] = (Instruction::PULA, comment);
			instructions.remove(idx + 1);
			idx -= 1;
			continue;
		}
		if let ((Instruction::PSHA, _), (Instruction::PULA, _)) =
			(&instructions[idx], &instructions[idx + 1])
		{
			instructions.remove(idx);
			instructions.remove(idx);
			idx -= 1;
			continue;
		}
		idx += 1;
	}
}

fn merge_allocs(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 2 && idx <= instructions.len() - 2 {
		if let (
			(Instruction::LEASP(Addressing::SP(a)), first_comment),
			(Instruction::LEASP(Addressing::SP(b)), second_comment),
		) = (&instructions[idx], &instructions[idx + 1])
		{
			let comment = merge_comments!(first_comment, second_comment);
			instructions[idx] = (Instruction::LEASP(Addressing::SP(*a + *b)), comment);
			instructions.remove(idx + 1);
		} else {
			idx += 1;
		}
	}
}

///Removes unreachable instructions that exist after an RTS instruction before the next label
/// (all jumps are to labels, even jumps that are relative according to the instruction set)
fn remove_post_early_return_code(instructions: &mut Vec<CommentedInstruction>) {
	let mut remove = false;
	let mut idx = 0;
	while idx < instructions.len() {
		match instructions[idx].0 {
			Instruction::RTS if !remove => {
				remove = true;
				idx += 1;
				continue;
			}
			Instruction::Label(_) => {
				remove = false;
			}
			_ => {}
		}
		if remove {
			instructions.remove(idx);
		} else {
			idx += 1;
		}
	}
}

///Split instruction list into sections with no branching or jumping so
/// `reduce_reserves_section` doesn't need to take that into account.
fn reduce_reserves_redux(instructions: &mut [CommentedInstruction]) -> Result<(), CompileError> {
	let mut last_instruction_split = 0;
	while let Some((idx, _)) =
		instructions
			.iter()
			.enumerate()
			.skip(1 + last_instruction_split)
			.find(|(_, (inst, _))| {
				matches!(
					inst,
					Instruction::RTS
						| Instruction::Label(_) | Instruction::JMP(_)
						| Instruction::BNE(_) | Instruction::BEQ(_)
						| Instruction::BGE(_) | Instruction::BLT(_)
						| Instruction::LDSP(_)
				)
			}) {
		let slice = &mut instructions[last_instruction_split..idx];
		reduce_reserves_section(slice)?;
		last_instruction_split = idx;
	}
	let slice = &mut instructions[last_instruction_split..];
	reduce_reserves_section(slice)?;
	Ok(())
}

///Reduce memory allocation in a scope so that the smallest memory access should be SP(0)
fn reduce_reserves_section(instructions: &mut [CommentedInstruction]) -> Result<(), CompileError> {
	let mut sp_stack = Vec::new();
	for idx in 0..instructions.len() {
		match instructions[idx].0 {
			Instruction::LEASP(Addressing::SP(reduce_by)) => match reduce_by.cmp(&0) {
				Ordering::Equal => {}
				Ordering::Less => {
					sp_stack.push(idx);
				}
				Ordering::Greater => {
					let start = if let Some(start) = sp_stack.last() {
						*start
					} else {
						continue;
					};
					sp_stack.pop();
					if matches!(instructions[start], (Instruction::PSHA, _)) {
						continue;
					}

					let start_value = instructions[start]
						.0
						.get_adr()
						.map(|inst| {
							if let Addressing::SP(sp) = inst {
								*sp
							} else {
								1
							}
						})
						.unwrap()
						.abs();
					let end_value = reduce_by.abs();

					let minimum_value = instructions[(start + 1)..idx]
						.iter()
						.filter(|(inst, _)| !matches!(inst, Instruction::LEASP(_)))
						.filter_map(|(inst, _)| {
							if let Some(Addressing::SP(n)) = inst.get_adr() {
								Some(*n)
							} else {
								None
							}
						})
						.min()
						.unwrap_or(isize::MAX);
					let minimum_to_use = minimum_value.max(0).min(end_value).min(start_value);

					instructions[(start + 1)..idx]
						.iter_mut()
						.filter(|(inst, _)| !matches!(inst, Instruction::LEASP(_)))
						.filter_map(|(inst, _)| {
							if let Some(Addressing::SP(n)) = inst.get_adr_mut() {
								Some(n)
							} else {
								None
							}
						})
						.for_each(|n| *n -= minimum_to_use);
					if let Some(Addressing::SP(n)) = instructions[start].0.get_adr_mut() {
						*n += minimum_to_use;
					}
					if let Some(Addressing::SP(n)) = instructions[idx].0.get_adr_mut() {
						*n -= minimum_to_use;
					}
				}
			},
			Instruction::PSHA => {
				sp_stack.push(idx);
			}
			Instruction::PULA => {
				sp_stack.pop();
			}
			_ => {}
		}
	}
	Ok(())
}

///Removes extra pushes and stack reductions that can just be a STA(SP(1))
/// that can occur when several operations that are implemented with function
/// calls follow each other
fn function_op_load_reduce(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 4 && idx <= instructions.len() - 4 {
		if let (
			(Instruction::LEASP(Addressing::SP(2)), _),
			(Instruction::PSHA, _),
			(Instruction::LDA(_), _),
			(Instruction::PSHA, _),
		) = (
			&instructions[idx],
			&instructions[idx + 1],
			&instructions[idx + 2],
			&instructions[idx + 3],
		) {
			instructions.remove(idx);
			instructions.remove(idx);
			instructions[idx + 1] = (Instruction::STA(Addressing::SP(1)), None);
		}
		idx += 1;
	}
}

///Replaces load 0 with a CLR instruction
fn clr(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 3 && idx <= instructions.len() - 3 {
		if let ((Instruction::LDA(Addressing::Data(0)), _), (Instruction::STA(to), _)) =
			(&instructions[idx], &instructions[idx + 1])
		{
			instructions[idx].0 = Instruction::CLR(to.clone());
			instructions.remove(idx + 1);
		}
		if let (
			(Instruction::LDA(Addressing::Data(0)), _),
			(Instruction::LDX(_), _),
			(Instruction::STA(adr), _),
		) = (
			&instructions[idx],
			&instructions[idx + 1],
			&instructions[idx + 2],
		) {
			if matches!(adr, Addressing::Xn(_)) {
				instructions[idx + 2].0 = Instruction::CLR(adr.clone());
				instructions.remove(idx);
			}
		}
		idx += 1;
	}
}

///Replaces load 0 with a CLR instruction
fn clra(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while !instructions.is_empty() && idx < instructions.len() {
		if let (Instruction::LDA(Addressing::Data(0)), _) = &instructions[idx] {
			instructions[idx].0 = Instruction::CLRA;
		}
		idx += 1;
	}
}

///Replaces subtraction by one with a DEC instruction
fn dec(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 3 && idx <= instructions.len() - 3 {
		if let (
			(Instruction::LDA(Addressing::Data(1)), _),
			(Instruction::SUBA(from), _),
			(Instruction::STA(to), _),
		) = (
			&instructions[idx],
			&instructions[idx + 1],
			&instructions[idx + 2],
		) {
			if to == from {
				instructions[idx + 2].0 = Instruction::DEC(to.clone());
				instructions.remove(idx + 1);
				instructions.remove(idx);
			}
		}
		idx += 1;
	}
}

///Replaces addition by one with an INC instruction
fn inc(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 3 && idx <= instructions.len() - 3 {
		if let (
			(Instruction::LDA(from), _),
			(Instruction::ADDA(Addressing::Data(1)), _),
			(Instruction::STA(to), _),
		) = (
			&instructions[idx],
			&instructions[idx + 1],
			&instructions[idx + 2],
		) {
			if to == from {
				instructions[idx].0 = Instruction::INC(to.clone());
				instructions.remove(idx + 2);
				instructions.remove(idx + 1);
			}
		}
		if let (
			(Instruction::LDA(Addressing::Data(1)), _),
			(Instruction::ADDA(from), _),
			(Instruction::STA(to), _),
		) = (
			&instructions[idx],
			&instructions[idx + 1],
			&instructions[idx + 2],
		) {
			if to == from {
				instructions[idx].0 = Instruction::INC(to.clone());
				instructions.remove(idx + 2);
				instructions.remove(idx + 1);
			}
		}
		idx += 1;
	}
}

///Replaces addition by one with an INC instruction
fn inca(instructions: &mut Vec<CommentedInstruction>) {
	for (inst, _) in instructions.iter_mut() {
		if matches!(inst, Instruction::ADDA(Addressing::Data(1))) {
			*inst = Instruction::INCA;
		}
	}
}

///Replaces subtraction by one with a DEC instruction
fn deca(instructions: &mut Vec<CommentedInstruction>) {
	for (inst, _) in instructions.iter_mut() {
		if matches!(inst, Instruction::SUBA(Addressing::Data(1))) {
			*inst = Instruction::DECA;
		}
	}
}

///Replaces the generated function call to __eq__ and a jump that is required for
/// chained boolean operators with a BEQ instruction directly
fn cmp_eq_jmp(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 6 && idx <= instructions.len() - 6 {
		if let (
			(Instruction::PSHA, Some(text)),
			(Instruction::LDA(lhs), lhs_comment),
			(Instruction::JSR(Addressing::Label(function_name)), None),
			(Instruction::LEASP(Addressing::SP(1)), None),
			(Instruction::TSTA, None),
			(Instruction::BEQ(jump_adr), None),
		) = (
			&instructions[idx],
			&instructions[idx + 1],
			&instructions[idx + 2],
			&instructions[idx + 3],
			&instructions[idx + 4],
			&instructions[idx + 5],
		) {
			if !(function_name == "__eq__" && text == "cmp rhs") {
				idx += 1;
				continue;
			}
			let lhs = if let Addressing::SP(n) = lhs {
				Addressing::SP(n - 1) //We're removing a PSHA
			} else {
				lhs.clone()
			};
			let lhs_comment = lhs_comment.clone();
			let jump_adr = jump_adr.clone();
			instructions[idx] = (Instruction::CMPA(lhs), lhs_comment);
			instructions[idx + 5] = (Instruction::BNE(jump_adr), None);
			instructions.remove(idx + 4);
			instructions.remove(idx + 3);
			instructions.remove(idx + 2);
			instructions.remove(idx + 1);
		}
		idx += 1;
	}
}

///Replaces the generated function call to __eq__ and a jump that is required for
/// chained boolean operators with a BNE instruction directly
fn cmp_neq_jmp(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 7 && idx <= instructions.len() - 7 {
		if let (
			(Instruction::PSHA, Some(text)),
			(Instruction::LDA(lhs), lhs_comment),
			(Instruction::JSR(Addressing::Label(function_name)), None),
			(Instruction::LEASP(Addressing::SP(1)), None),
			(Instruction::COMA, None),
			(Instruction::TSTA, None),
			(Instruction::BEQ(jump_adr), None),
		) = (
			&instructions[idx],
			&instructions[idx + 1],
			&instructions[idx + 2],
			&instructions[idx + 3],
			&instructions[idx + 4],
			&instructions[idx + 5],
			&instructions[idx + 6],
		) {
			if !(function_name == "__eq__" && text == "cmp rhs") {
				idx += 1;
				continue;
			}
			let lhs = if let Addressing::SP(n) = lhs {
				Addressing::SP(n - 1) //We're removing a PSHA
			} else {
				lhs.clone()
			};
			let lhs_comment = lhs_comment.clone();
			let jump_adr = jump_adr.clone();
			instructions[idx] = (Instruction::CMPA(lhs), lhs_comment);
			instructions[idx + 1] = (Instruction::BEQ(jump_adr), None);
			instructions.remove(idx + 6);
			instructions.remove(idx + 5);
			instructions.remove(idx + 4);
			instructions.remove(idx + 3);
			instructions.remove(idx + 2);
		}
		idx += 1;
	}
}

///Replaces the generated function call to __gt__ and a jump that is required for
/// chained boolean operators with a BGE instruction directly
fn cmp_gt_jmp(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 6 && idx <= instructions.len() - 6 {
		if let (
			(Instruction::PSHA, Some(text)),
			(Instruction::LDA(lhs), lhs_comment),
			(Instruction::JSR(Addressing::Label(function_name)), None),
			(Instruction::LEASP(Addressing::SP(1)), None),
			(Instruction::TSTA, None),
			(Instruction::BEQ(jump_adr), None),
		) = (
			&instructions[idx],
			&instructions[idx + 1],
			&instructions[idx + 2],
			&instructions[idx + 3],
			&instructions[idx + 4],
			&instructions[idx + 5],
		) {
			if !(function_name == "__gt__" && text == "gt rhs") {
				idx += 1;
				continue;
			}
			let lhs = if let Addressing::SP(n) = lhs {
				Addressing::SP(n - 1) //We're removing a PSHA
			} else {
				lhs.clone()
			};
			let lhs_comment = lhs_comment.clone();
			let jump_adr = jump_adr.clone();
			instructions[idx] = (Instruction::CMPA(lhs), lhs_comment);
			instructions[idx + 1] = (Instruction::BGE(jump_adr), None);
			instructions.remove(idx + 5);
			instructions.remove(idx + 4);
			instructions.remove(idx + 3);
			instructions.remove(idx + 2);
		}
		idx += 1;
	}
}

///Replaces the generated function call to __gt__ and a jump that is required for
/// chained boolean operators with a BLT instruction directly
fn cmp_gte_jmp(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 7 && idx <= instructions.len() - 7 {
		if let (
			(Instruction::PSHA, Some(text)),
			(Instruction::LDA(lhs), lhs_comment),
			(Instruction::JSR(Addressing::Label(function_name)), None),
			(Instruction::LEASP(Addressing::SP(1)), None),
			(Instruction::COMA, None),
			(Instruction::TSTA, None),
			(Instruction::BEQ(jump_adr), None),
		) = (
			&instructions[idx],
			&instructions[idx + 1],
			&instructions[idx + 2],
			&instructions[idx + 3],
			&instructions[idx + 4],
			&instructions[idx + 5],
			&instructions[idx + 6],
		) {
			if !(function_name == "__gt__" && text == "lte rhs") {
				idx += 1;
				continue;
			}
			let lhs = if let Addressing::SP(n) = lhs {
				Addressing::SP(n - 1) //We're removing a PSHA
			} else {
				lhs.clone()
			};
			let lhs_comment = lhs_comment.clone();
			let jump_adr = jump_adr.clone();
			instructions[idx] = (Instruction::CMPA(lhs), lhs_comment);
			instructions[idx + 1] = (Instruction::BLT(jump_adr), None);
			instructions.remove(idx + 6);
			instructions.remove(idx + 5);
			instructions.remove(idx + 4);
			instructions.remove(idx + 3);
			instructions.remove(idx + 2);
		}
		idx += 1;
	}
}

fn load_array_constant_index(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 2 && idx <= instructions.len() - 2 {
		if let (
			(Instruction::LDY(Addressing::SP(y)), y_comment),
			(Instruction::LDA(Addressing::Yn(a)), a_comment),
		) = (&instructions[idx], &instructions[idx + 1])
		{
			instructions[idx] = (
				Instruction::LDA(Addressing::SP(*y + *a)),
				merge_comments!(y_comment, a_comment),
			);
			instructions.remove(idx + 1);
		}
		if let (
			(Instruction::LDX(Addressing::SP(x)), x_comment),
			(Instruction::LDA(Addressing::Xn(a)), a_comment),
		) = (&instructions[idx], &instructions[idx + 1])
		{
			instructions[idx] = (
				Instruction::LDA(Addressing::SP(*x + *a)),
				merge_comments!(x_comment, a_comment),
			);
			instructions.remove(idx + 1);
		}
		idx += 1;
	}
}

///Removes labels that are never used. Includes everything, even function names
pub fn remove_unused_labels(instructions: &mut Vec<CommentedInstruction>) {
	let jumps_to = instructions
		.iter()
		.filter_map(|(inst, _)| match inst.get_adr() {
			Some(Addressing::Label(lbl)) | Some(Addressing::DataLabel(lbl)) => {
				Some(lbl.trim().to_string())
			}
			_ => None,
		})
		.chain(iter::once("main".to_string()))
		.chain(iter::once("interrupt".to_string()))
		.collect::<HashSet<String>>();
	instructions.retain(|(inst, _)| {
		if let Instruction::Label(lbl) = inst {
			let lbl: &str = lbl;
			jumps_to.contains(lbl.trim())
		} else {
			true
		}
	});
}

pub(crate) fn make_interrupt_return(instructions: &mut Vec<CommentedInstruction>) {
	let make_rti = |(inst, _): &mut CommentedInstruction| {
		if let Instruction::RTS = inst {
			*inst = Instruction::RTI
		}
	};
	instructions.iter_mut().for_each(make_rti);
}

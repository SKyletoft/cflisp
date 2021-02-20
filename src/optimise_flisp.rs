use crate::*;
use std::{cmp::Ordering, collections::HashSet};

///Doesn't actually call all optimisations. It only calls those optimisations that
/// can be called on an independent code block. This excludes `remove_unused_labels`
/// and `repeat_rts`
pub(crate) fn all_optimisations(instructions: &mut Vec<CommentedInstruction>) {
	remove_post_early_return_code(instructions);
	load_xy(instructions);
	repeat_xy(instructions);
	repeat_load(instructions);
	load_a(instructions);
	function_op_load_reduce(instructions);
	repeat_a(instructions);
	reduce_reserves(instructions);
	nop(instructions); //Should go AFTER reduce reserves
	cmp_eq_jmp(instructions);
	cmp_neq_jmp(instructions);
	cmp_gt_jmp(instructions);
	cmp_gte_jmp(instructions);
	inc(instructions);
	inca(instructions);
	dec(instructions);
	deca(instructions);
}

fn load_xy(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 3 && idx < instructions.len() - 3 {
		if let (
			(Instruction::LDA(addressing), comment),
			(Instruction::STA(Addressing::SP(-1)), _),
			(Instruction::LDX(Addressing::SP(-1)), _),
		) = (
			&instructions[idx],
			&instructions[idx + 1],
			&instructions[idx + 2],
		) {
			instructions[idx] = (Instruction::LDX(addressing.clone()), *comment);
			instructions.remove(idx + 2); //Order matters!
			instructions.remove(idx + 1);
		}
		if let (
			(Instruction::LDA(addressing), comment),
			(Instruction::STA(Addressing::SP(-1)), _),
			(Instruction::LDY(Addressing::SP(-1)), _),
		) = (
			&instructions[idx],
			&instructions[idx + 1],
			&instructions[idx + 2],
		) {
			instructions[idx] = (Instruction::LDY(addressing.clone()), *comment);
			instructions.remove(idx + 2); //Order matters!
			instructions.remove(idx + 1);
		}
		if let (
			&(Instruction::LDX(Addressing::Data(x_adr)), x_comment),
			(Instruction::LDA(a_adr), a_comment),
			(Instruction::STA(Addressing::Xn(0)), _),
		) = (
			&instructions[idx],
			&instructions[idx + 1],
			&instructions[idx + 2],
		) {
			let a_comment = *a_comment;
			let a_adr = a_adr.clone();
			instructions[idx] = (Instruction::LDA(a_adr), a_comment);
			instructions[idx + 1] = (Instruction::STA(Addressing::Adr(x_adr)), x_comment);
			instructions.remove(idx + 2);
		}
		idx += 1;
	}
}

fn repeat_load(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 2 && idx < instructions.len() - 2 {
		if matches!(
			(&instructions[idx], &instructions[idx + 1]),
			((Instruction::LDA(_), _), (Instruction::LDA(_), _))
				| ((Instruction::LDX(_), _), (Instruction::LDX(_), _))
				| ((Instruction::LDY(_), _), (Instruction::LDY(_), _))
				| ((Instruction::LDSP(_), _), (Instruction::LDSP(_), _))
				| ((Instruction::RTS, _), (Instruction::RTS, _))
		) {
			instructions.remove(idx);
		}
		idx += 1;
	}
}

pub(crate) fn repeat_rts(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 2 && idx < instructions.len() - 2 {
		if matches!(
			(&instructions[idx], &instructions[idx + 1]),
			((Instruction::RTS, _), (Instruction::RTS, _))
		) {
			instructions.remove(idx);
		}
		idx += 1;
	}
}

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

fn load_a(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 3 && idx < instructions.len() - 3 {
		if let (
			(Instruction::STA(Addressing::SP(-1)), _),
			(Instruction::LDX(Addressing::SP(-1)), _),
			(Instruction::LDA(Addressing::Xn(0)), comment),
		) = (
			&instructions[idx],
			&instructions[idx + 1],
			&instructions[idx + 2],
		) {
			instructions[idx + 1] = (Instruction::LDA(Addressing::AX), *comment);
			instructions[idx] = (Instruction::LDX(Addressing::Data(0)), None);
			instructions.remove(idx + 2);
		}
		if let ((Instruction::PSHA, _), (Instruction::LDA(Addressing::SP(0)), _)) =
			(&instructions[idx], &instructions[idx + 1])
		{
			instructions.remove(idx + 1);
		}
		if let ((Instruction::LDA(_), _), (Instruction::LDA(adr), _)) =
			(&instructions[idx], &instructions[idx + 1])
		{
			if !matches!(adr, Addressing::AX | Addressing::AY) {
				instructions.remove(idx);
			}
		}
		idx += 1;
	}
}

//For each allocation, find how many unused bytes there are and reduce the allocation by that much.
// Won't remove 0 size allocations, just run nop afterwards.
fn reduce_reserves(instructions: &mut Vec<CommentedInstruction>) {
	let mut sp_stack: Vec<(usize, isize)> = Vec::new();

	for idx in 0..instructions.len() {
		match instructions[idx].0 {
			Instruction::LDSP(Addressing::SP(n)) => {
				sp_stack.push((idx, n));
			}
			Instruction::RTS
			| Instruction::Label(_)
			| Instruction::JMP(_)
			| Instruction::BNE(_)
			| Instruction::BEQ(_)
			| Instruction::BGE(_)
			| Instruction::BLT(_) => {
				//Jump or jump target, new context. Some missed
				// optimisations but massively reduced complexity
				sp_stack.clear();
			}
			Instruction::LEASP(Addressing::SP(n)) => {
				match n.cmp(&&mut 0) {
					Ordering::Equal => {}
					Ordering::Less => {
						sp_stack.push((idx, n));
					}
					Ordering::Greater => {
						if let Some((sp_index, value)) = sp_stack.pop() {
							if matches!(instructions[sp_index], (Instruction::PSHA, _)) {
								//We can't reduce allocation size of a PSHA, only LEASPs
								continue;
							}
							if -value != n {
								sp_stack.clear(); //Should hopefully skip to next reset by not hitting the pop?
							}
							let minimum_access = instructions
								.iter()
								.skip(sp_index + 1)
								.take(idx - sp_index)
								.map(|(inst, _)| {
									if let Some(Addressing::SP(n)) = inst.get_adr() {
										*n
									} else {
										isize::MAX
									}
								})
								.min()
								.unwrap_or(isize::MAX);
							if minimum_access <= 0 {
								continue;
							}
							if let Some(Addressing::SP(n)) = instructions[sp_index].0.get_adr_mut()
							{
								*n += minimum_access
							}
							if let Some(Addressing::SP(n)) = instructions[idx].0.get_adr_mut() {
								*n -= minimum_access
							}
							for (inst, _) in instructions
								.iter_mut()
								.skip(sp_index + 1)
								.take(idx - sp_index - 1)
							{
								if let Some(Addressing::SP(adr)) = inst.get_adr_mut() {
									*adr -= minimum_access + 1;
								}
							}
						}
					}
				}
			}
			Instruction::PSHA => {
				sp_stack.push((idx, -1));
			}
			Instruction::PULA => {
				if matches!(sp_stack.last(), Some((1, _))) {
					sp_stack.pop();
				} else {
					panic!("Internal error: Where did the PULA even come from?");
				}
			}
			_ => {}
		}
	}
}

fn remove_post_early_return_code(instructions: &mut Vec<CommentedInstruction>) {
	let mut remove = false;
	let mut idx = 0;
	while idx < instructions.len() {
		match instructions[idx].0 {
			Instruction::RTS => {
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

fn function_op_load_reduce(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 4 && idx < instructions.len() - 4 {
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

fn dec(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 3 && idx < instructions.len() - 3 {
		if let (
			(Instruction::LDA(Addressing::Data(1)), _),
			(Instruction::SUBA(from), comment),
			(Instruction::STA(to), _),
		) = (
			&instructions[idx],
			&instructions[idx + 1],
			&instructions[idx + 2],
		) {
			if to == from {
				instructions[idx] = (Instruction::DEC(to.clone()), *comment);
				instructions.remove(idx + 2);
				instructions.remove(idx + 1);
			}
		}
		idx += 1;
	}
}

fn inc(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 3 && idx < instructions.len() - 3 {
		if let (
			(Instruction::LDA(from), comment),
			(Instruction::ADDA(Addressing::Data(1)), _),
			(Instruction::STA(to), _),
		) = (
			&instructions[idx],
			&instructions[idx + 1],
			&instructions[idx + 2],
		) {
			if to == from {
				instructions[idx] = (Instruction::INC(to.clone()), *comment);
				instructions.remove(idx + 2);
				instructions.remove(idx + 1);
			}
		}
		idx += 1;
	}
}

fn inca(instructions: &mut Vec<CommentedInstruction>) {
	for (inst, _) in instructions.iter_mut() {
		if matches!(inst, Instruction::ADDA(Addressing::Data(1))) {
			*inst = Instruction::INCA;
		}
	}
}

fn deca(instructions: &mut Vec<CommentedInstruction>) {
	for (inst, _) in instructions.iter_mut() {
		if matches!(inst, Instruction::SUBA(Addressing::Data(1))) {
			*inst = Instruction::DECA;
		}
	}
}

fn cmp_eq_jmp(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 6 && idx < instructions.len() - 6 {
		if let (
			(Instruction::PSHA, Some("cmp rhs")),
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
			if function_name != "__eq__" {
				idx += 1;
				continue;
			}
			let lhs = if let Addressing::SP(n) = lhs {
				Addressing::SP(n - 1) //We're removing a PSHA
			} else {
				lhs.clone()
			};
			let lhs_comment = *lhs_comment;
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

fn cmp_neq_jmp(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 7 && idx < instructions.len() - 7 {
		if let (
			(Instruction::PSHA, Some("cmp rhs")),
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
			if function_name != "__eq__" {
				idx += 1;
				continue;
			}
			let lhs = if let Addressing::SP(n) = lhs {
				Addressing::SP(n - 1) //We're removing a PSHA
			} else {
				lhs.clone()
			};
			let lhs_comment = *lhs_comment;
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

fn cmp_gt_jmp(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 6 && idx < instructions.len() - 6 {
		if let (
			(Instruction::PSHA, Some("gt rhs")),
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
			if function_name != "__gt__" {
				idx += 1;
				continue;
			}
			let lhs = if let Addressing::SP(n) = lhs {
				Addressing::SP(n - 1) //We're removing a PSHA
			} else {
				lhs.clone()
			};
			let lhs_comment = *lhs_comment;
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

fn cmp_gte_jmp(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while instructions.len() >= 7 && idx < instructions.len() - 7 {
		if let (
			(Instruction::PSHA, Some("lte rhs")),
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
			if function_name != "__gt__" {
				idx += 1;
				continue;
			}
			let lhs = if let Addressing::SP(n) = lhs {
				Addressing::SP(n - 1) //We're removing a PSHA
			} else {
				lhs.clone()
			};
			let lhs_comment = *lhs_comment;
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

pub(crate) fn remove_unused_labels(instructions: &mut Vec<CommentedInstruction>) {
	let mut jumps_to = instructions
		.iter()
		.filter_map(|(inst, _)| match inst {
			Instruction::LDA(Addressing::Label(lbl))
			| Instruction::LDX(Addressing::Label(lbl))
			| Instruction::LDY(Addressing::Label(lbl))
			| Instruction::LDSP(Addressing::Label(lbl))
			| Instruction::ADDA(Addressing::Label(lbl))
			| Instruction::SUBA(Addressing::Label(lbl))
			| Instruction::ANDA(Addressing::Label(lbl))
			| Instruction::ORA(Addressing::Label(lbl))
			| Instruction::EORA(Addressing::Label(lbl))
			| Instruction::STA(Addressing::Label(lbl))
			| Instruction::JMP(Addressing::Label(lbl))
			| Instruction::BNE(Addressing::Label(lbl))
			| Instruction::BEQ(Addressing::Label(lbl))
			| Instruction::BGE(Addressing::Label(lbl))
			| Instruction::BLT(Addressing::Label(lbl))
			| Instruction::LEASP(Addressing::Label(lbl))
			| Instruction::CMPA(Addressing::Label(lbl))
			| Instruction::INC(Addressing::Label(lbl))
			| Instruction::JSR(Addressing::Label(lbl)) => Some(lbl.clone()),
			_ => None,
		})
		.collect::<HashSet<String>>();
	jumps_to.insert("main".to_string());
	instructions.retain(|(inst, _)| {
		if let Instruction::Label(lbl) = inst {
			jumps_to.contains(lbl)
		} else {
			true
		}
	});
}

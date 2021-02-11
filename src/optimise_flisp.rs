use crate::*;
use flisp_instructions::{Addressing, CommentedInstruction, Instruction};

pub(crate) fn all_optimisations(instructions: &mut Vec<CommentedInstruction>) {
	load_xy(instructions);
	repeat_xy(instructions);
	nop(instructions);
	repeat_load(instructions);
	load_a(instructions);
	function_op_load_reduce(instructions);
	repeat_a(instructions);
	reduce_reserves(instructions);
	cmp_eq_jmp(instructions);
	inc(instructions);
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
			| (Instruction::ROLA(_), _)
			| (Instruction::RORA(_), _)
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
		idx += 1;
	}
}

fn reduce_reserves(instructions: &mut Vec<CommentedInstruction>) {
	//Loop over instructions and check if there are no write
	// (to memory, not registers) operations between reserve
	// and clear and remove them if there are none
	let mut idx = 0;
	let mut start = usize::MAX;
	let mut memory_touched = false;

	let mut edit_at = 0;
	let mut depth = 0;

	while idx < instructions.len() {
		match instructions[idx] {
			(Instruction::LEASP(_), Some("Reserving memory for statement")) => {
				start = idx;
				memory_touched = false;
				depth += 1;
				edit_at = depth;
			}
			(Instruction::LEASP(_), Some("Clearing memory for statement")) => {
				if !memory_touched && depth == edit_at {
					instructions.remove(idx);
					instructions.remove(start);
					reduce_reserves(instructions);
					return;
				}
				depth -= 1;
			}
			(Instruction::STA(_), _)
			| (Instruction::PSHA, _)
			| (Instruction::PULA, _)
			| (Instruction::JSR(_), _) => {
				memory_touched = false;
			}
			_ => {}
		}
		idx += 1;
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
				instructions.remove(idx + 1);
				instructions.remove(idx + 1);
			}
		}
		idx += 1;
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
			(Instruction::BNE(jump_to), None),
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
			let lhs = lhs.clone();
			let lhs_comment = *lhs_comment;
			let jump_to = jump_to.clone();
			instructions[idx] = (Instruction::CMPA(lhs), lhs_comment);
			instructions[idx + 1] = (Instruction::BEQ(jump_to), None);
			instructions.remove(idx + 2);
			instructions.remove(idx + 2);
			instructions.remove(idx + 2);
			instructions.remove(idx + 2);
		}
		idx += 1;
	}
}

use crate::*;
use flisp_instructions::{Addressing, CommentedInstruction, Instruction};

pub(crate) fn all_optimisations(instructions: &mut Vec<CommentedInstruction>) {
	load_xy(instructions);
	repeat_xy(instructions);
	nop(instructions);
	repeat_load(instructions);
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

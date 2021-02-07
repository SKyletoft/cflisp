use crate::*;
use flisp_instructions::{Addressing, CommentedInstruction, Instruction};

pub(crate) fn all_optimisations(instructions: &mut Vec<CommentedInstruction>) {
	load_x(instructions);
	//op_on_simple(instructions);
}

fn load_x(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while idx < instructions.len() - 3 && instructions.len() >= 3 {
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
		idx += 1;
	}
}

fn op_on_simple(instructions: &mut Vec<CommentedInstruction>) {
	let mut idx = 0;
	while idx < instructions.len() - 3 && instructions.len() >= 3 {
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
			instructions.remove(idx + 1);
			instructions.remove(idx + 2);
		}
		idx += 1;
	}
}

fn repeat_x(instructions: &mut Vec<CommentedInstruction>) {}
fn repeat_y(instructions: &mut Vec<CommentedInstruction>) {}

use crate::*;

const DIGIFLISP_PROGRAM_LIMIT: usize = 172;

pub fn instructions_to_text(
	instructions: &[CommentedInstruction],
	flags: &Flags,
) -> Result<String, CompileError> {
	if instructions
		.iter()
		.map(|(instruction, _)| instruction.size())
		.sum::<usize>()
		> DIGIFLISP_PROGRAM_LIMIT
	{
		return Err(CompileError(line!(), "Program is too large for digiflisp!"));
	}
	let mut output = String::new();
	for ((i, c), (next, _)) in instructions.iter().zip(
		instructions
			.iter()
			.skip(1)
			.chain([(Instruction::RTS, None)].iter()),
	) {
		if matches!(i, Instruction::Label(lbl) if lbl == "global") {
			continue;
		}
		match (flags.hex, flags.comments) {
			(true, true) => match (i, c) {
				(inst, Some(comm)) => output.push_str(&format!("{:X}\t; {}", inst, comm)),
				(inst, None) => output.push_str(&format!("{:X}", inst)),
			},
			(false, true) => match (i, c) {
				(inst, Some(comm)) => output.push_str(&format!("{}\t; {}", inst, comm)),
				(inst, None) => output.push_str(&format!("{}", inst)),
			},
			(true, false) => output.push_str(&format!("{:X}", i)),
			(false, false) => output.push_str(&format!("{}", i)),
		}

		if !matches!(i, Instruction::Label(n) if n.len() < 8)
			|| matches!((i, next), (Instruction::RTS, Instruction::Label(lbl)) if !(lbl.starts_with("if") || lbl.starts_with("for") || lbl.starts_with("while")))
			|| matches!((i, next), (Instruction::FCB(_), Instruction::Label(_)))
			|| matches!((i, next), (Instruction::Label(_), Instruction::Label(lbl)) if !lbl.starts_with('\n'))
		{
			output.push('\n');
		}
	}
	output.push('\n');
	Ok(output)
}

///Imports all functions for non native operations that are used by the program.
/// Also adds init if no init function is written
pub fn automatic_imports(instructions: &mut String, debug: bool) {
	if instructions.contains("__mul__") {
		instructions.push_str(include_str!("asm_deps/mul.sflisp"));
	}
	if instructions.contains("__eq__") {
		instructions.push_str(include_str!("asm_deps/eq.sflisp"));
	}
	if instructions.contains("__gt__") {
		instructions.push_str(include_str!("asm_deps/gt.sflisp"));
	}
	if instructions.contains("__div__") {
		instructions.push_str(include_str!("asm_deps/div.sflisp"));
	}
	if instructions.contains("__mod__") {
		instructions.push_str(include_str!("asm_deps/mod.sflisp"));
	}
	if !instructions.contains("__init_") {
		if debug {
			instructions.push_str(include_str!("asm_deps/debug_init.sflisp"));
		} else {
			instructions.push_str(include_str!("asm_deps/init.sflisp"));
		}
	}
	instructions.push_str("\n\tORG\t$FF\n\tFCB\t__init_\n");
}

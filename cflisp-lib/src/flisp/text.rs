use crate::*;

const DIGIFLISP_PROGRAM_LIMIT: usize = 172;

pub fn instructions_to_text<'a>(
	instructions: &[CommentedInstruction<'a>],
	flags: &Flags,
) -> Result<String> {
	if instructions
		.iter()
		.map(|(instruction, _)| instruction.size())
		.sum::<usize>()
		> DIGIFLISP_PROGRAM_LIMIT
	{
		return Err(error!(ProgramTooLarge));
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

		let newline_exception = !matches!(i, Instruction::Label(l) if l.strip_prefix("global::").unwrap_or(l).len() < 8)
			|| matches!((i, next), (Instruction::RTS, Instruction::Label(lbl)) if !(lbl.starts_with("if") || lbl.starts_with("for") || lbl.starts_with("while")))
			|| matches!((i, next), (Instruction::FCB(_), Instruction::Label(_)))
			|| matches!((i, next), (Instruction::Label(_), Instruction::Label(lbl)) if !lbl.starts_with('\n'));
		if newline_exception {
			output.push('\n');
		}
	}
	output.push('\n');
	Ok(output)
}

///Imports all functions for non native operations that are used by the program.
/// Also adds init if no init function is written
pub fn automatic_imports(instructions: &mut String, debug: bool, kill_interrupts: bool) {
	if instructions.contains("__mul__") {
		instructions.push_str(include_str!("asm_deps/mul.sflisp"));
	}
	if instructions.contains("__eq__") {
		instructions.push_str(include_str!("asm_deps/eq.sflisp"));
	}
	if instructions.contains("__gts_") {
		instructions.push_str(include_str!("asm_deps/gts.sflisp"));
	}
	if instructions.contains("__gtu_") {
		instructions.push_str(include_str!("asm_deps/gtu.sflisp"));
	}
	if instructions.contains("__div__") {
		instructions.push_str(include_str!("asm_deps/div.sflisp"));
	}
	if instructions.contains("__mod__") {
		instructions.push_str(include_str!("asm_deps/mod.sflisp"));
	}
	if instructions.contains("__tb__") {
		instructions.push_str(include_str!("asm_deps/cast_to_bool.sflisp"));
	}
	if instructions.contains("__swap_") {
		instructions.push_str(include_str!("asm_deps/swap_preload.sflisp"));
	}
	if instructions.contains("_mirror") {
		instructions.push_str(include_str!("asm_deps/mirror_small.sflisp"));
	}
	if instructions.contains("_print_") {
		instructions.push_str(include_str!("asm_deps/print.sflisp"));
	}
	if instructions.contains("__read_") {
		instructions.push_str(include_str!("asm_deps/read.sflisp"));
	}
	if !instructions.contains("__init_") {
		let interrupts = instructions.contains("interrupt");
		let init = match (interrupts, debug, kill_interrupts) {
			(false, true, _) => include_str!("asm_deps/debug_init.sflisp"),
			(false, false, _) => include_str!("asm_deps/init.sflisp"),
			(true, true, true) => include_str!("asm_deps/debug_init_kill.sflisp"),
			(true, true, false) => include_str!("asm_deps/debug_init_continue.sflisp"),
			(true, false, true) => include_str!("asm_deps/init_kill.sflisp"),
			(true, false, false) => include_str!("asm_deps/init_continue.sflisp"),
		};
		instructions.push_str(init);
	}
	if instructions.contains("interrupt") {
		instructions.push_str("\n\tORG\t$FD\n\tFCB\tinterrupt\n\tFCB\t__err__\n\tFCB\t__init_\n");
	} else {
		instructions.push_str("\n\tORG\t$FE\n\tFCB\t__err__\n\tFCB\t__init_\n");
	}
}

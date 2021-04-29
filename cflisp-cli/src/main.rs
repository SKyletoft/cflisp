use std::{
	env, fs,
	path::PathBuf,
	{process, process::Command},
};

use cflisp_lib::{
	flags::Flags,
	flisp::{compile_flisp, optimise_flisp, text},
	optimise_language, parser,
	structless_language::StructlessLanguage,
	type_checker,
};

const PATH: &str = "PATH";
#[cfg(unix)]
const QAFLISP: &str = "/qaflisp";
#[cfg(windows)]
const QAFLISP: &str = "\\qaflisp.exe";

//Yes, this is a horrible mess
fn main() {
	let args = env::args().skip(1).collect::<Vec<_>>();

	if args
		.iter()
		.map(String::as_str)
		.any(|s| s == "--help" || s == "-help")
	{
		println!(include_str!("help.txt"));
		process::exit(0);
	}

	let flags = args.iter().collect::<Flags>();
	let files = args
		.iter()
		.filter(|s| !s.starts_with('-') && !s.is_empty())
		.filter(|s| **s != flags.out)
		.map(|s| s.as_str())
		.collect::<Vec<&str>>();
	if files.is_empty() {
		exit_error("Error: No input files");
	}
	let mut source = String::new();
	for file in files {
		source.push_str(
			&fs::read_to_string(file).unwrap_or_else(|e| {
				exit_error(&format!("IO Error: File could not be read ({})", e))
			}),
		);
		source.push('\n');
	}
	source = parser::remove_comments(&source);

	let parsed = parser::parse(&source, !flags.debug)
		.unwrap_or_else(|e| exit_error(&format!("Parse error ({})", e)));
	if flags.tree {
		dbg!(&parsed);
	}
	if flags.type_check {
		type_checker::type_check(&parsed)
			.unwrap_or_else(|e| exit_error(&format!("Type check error ({})", e)));
	}
	let mut struct_filtered = StructlessLanguage::from_language_elements(parsed)
		.unwrap_or_else(|e| exit_error(&format!("Parse error ({})", e)));
	if flags.optimise >= 2 {
		optimise_language::all_optimisations(&mut struct_filtered)
			.unwrap_or_else(|e| exit_error(&format!("Name error ({})", e)));
	}
	if flags.tree_structless {
		if flags.tree {
			eprintln!();
		}
		dbg!(&struct_filtered);
	}
	let mut instr = compile_flisp::compile(&struct_filtered, &flags)
		.unwrap_or_else(|e| exit_error(&format!("Compilation error ({})", e)));
	if !flags.debug {
		optimise_flisp::remove_unused_labels(&mut instr);
		optimise_flisp::repeat_rts(&mut instr);
	}
	let mut compiled = text::instructions_to_text(&instr, &flags)
		.unwrap_or_else(|e| exit_error(&format!("Program too large? ({})", e)));
	text::automatic_imports(&mut compiled, flags.debug, flags.kill_interrupts);
	if flags.debug {
		compiled.insert_str(0, "\tORG\t$20\n");
	}
	if flags.print_result {
		println!("{}", &compiled);
	}
	if !flags.print_result || flags.assemble {
		fs::write(&flags.out, &compiled)
			.unwrap_or_else(|e| exit_error(&format!("IO Error: Could not save file ({})", e)));
	}
	if flags.assemble {
		let (_, path_var) = std::env::vars()
			.find(|(name, _)| name == PATH)
			.unwrap_or_else(|| exit_error("PATH variable doesn't exist"));
		let qaflisp_path = path_var
			.split(':')
			.filter_map(|path_segment| {
				let path: PathBuf = (path_segment.to_string() + QAFLISP).into();
				if path.exists() {
					Some(path)
				} else {
					None
				}
			})
			.next()
			.unwrap_or_else(|| exit_error("Couldn't find qaflisp"));
		let program_call = Command::new(qaflisp_path)
			.arg(flags.out)
			.output()
			.unwrap_or_else(|_| exit_error("Output isn't utf8?"));
		let exit_status = program_call.status.code().unwrap_or_default();
		let stdout = String::from_utf8(program_call.stdout)
			.unwrap_or_else(|_| exit_error("Failed to call qaflisp"));
		let stderr = String::from_utf8(program_call.stderr)
			.unwrap_or_else(|_| exit_error("Failed to call qaflisp"));
		print!("{}\n{}", stdout, stderr);
		process::exit(exit_status);
	}
}

fn exit_error(msg: &str) -> ! {
	eprintln!("{}", msg);
	process::exit(-1);
}

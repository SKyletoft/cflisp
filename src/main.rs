use std::{env, fmt, fs, path::PathBuf, process::exit};

pub mod compile_flisp;
pub mod error;
pub mod flags;
pub mod flisp_instructions;
pub mod helper;
pub mod language_element;
pub mod optimise_flisp;
pub mod parser;
pub mod statement_element;
pub mod statement_token;
pub mod token;
pub mod types;

use error::{CompileError, ParseError};
use flags::Flags;
use language_element::LanguageElement;
use statement_token::StatementToken;
use token::Token;
use types::Variable;

fn main() {
	let args = env::args().skip(1).collect::<Vec<_>>();
	let flags = args.iter().collect::<Flags>();
	let files = args
		.iter()
		.filter(|s| !s.starts_with('-') && !s.is_empty())
		.filter(|s| **s != flags.out)
		.map(|s| s.as_str())
		.collect::<Vec<&str>>();
	if files.is_empty() {
		eprintln!("Error: No input files");
		exit(-1);
	}
	let mut source = String::new();
	for file in files {
		source.push_str(&fs::read_to_string(file).expect("IO Error: Could not read file"));
		source.push('\n');
	}
	source = parser::remove_comments(&source);
	let parsed = parser::parse(&source).expect("Parse error");
	if flags.type_check {
		let ok = parser::type_check(&parsed, &[], &[]).expect("Name error");
		if !ok {
			eprintln!("Error: type check or name resolution error");
			exit(-1);
		} else if flags.type_check {
			eprintln!("Type check passed");
		}
	}
	if flags.tree {
		dbg!(&parsed);
	}
	let instr = compile_flisp::compile(&parsed, &flags).expect("Compiler error");
	let mut compiled = compile_flisp::instructions_to_text(&instr, &flags).expect("Too long?");
	if flags.debug {
		compiled.insert_str(0, "\tORG\t$20\n");
		compiled.push_str("\ninit\tLDA\t#0\n\tLDX\t#0\n\tLDY\t#0\n\tLDSP\t#$FB\n\tJSR\tmain\nend\tJMP\tend\n\n\tORG\t$FF\n\tFCB\tinit\n");
	}
	if flags.print_result {
		println!("{}", &compiled);
	} else {
		fs::write(&flags.out, &compiled).expect("IO Error: Could not save file");
	}
	if flags.assemble {
		let path = std::env::vars()
			.find(|(name, _)| name == "PATH")
			.map(|(_, paths)| paths);
		if let Some(p) = path
			.as_ref()
			.map(|s| {
				s.split(':').find(|p| {
					let path: PathBuf = (p.to_string() + "/qaflisp").into();
					path.exists()
				})
			})
			.flatten()
		{
			let res = std::process::Command::new(p)
				.arg(flags.out)
				.output()
				.expect("Failed to call qaflisp");
			eprintln!("{:?}", res.stdout);
		} else {
			eprintln!("Couldn't find qaflisp");
			exit(-1);
		}
	}
}

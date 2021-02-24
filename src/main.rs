use std::{
	env, fmt, fs,
	path::PathBuf,
	process::{exit, Command},
};

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
pub mod text;
pub mod token;
pub mod types;

use error::{CompileError, ParseError};
use flags::Flags;
use flisp_instructions::{Addressing, CommentedInstruction, Instruction};
use language_element::LanguageElement;
use statement_element::StatementElement;
use statement_token::StatementToken;
use token::{Token, Token::*};
use types::{Block, Function, Statement, Type, Variable};

//Yes, this is a horrible mess
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
		source.push_str(&fs::read_to_string(file).unwrap_or_else(|e| {
			eprintln!("IO Error: Could not read file ({})", e);
			exit(-1);
		}));
		source.push('\n');
	}
	source = parser::remove_comments(&source);
	let parsed = parser::parse(&source, !flags.debug).unwrap_or_else(|e| {
		eprintln!("Parse Error ({})", e);
		exit(-1);
	});
	if flags.type_check {
		let ok = parser::type_check(&parsed, &[], &[]).unwrap_or_else(|e| {
			eprintln!("Name error ({})", e);
			exit(-1);
		});
		if !ok {
			eprintln!("Error: type check or name resolution error");
			exit(-1);
		}
	}
	if flags.tree {
		dbg!(&parsed);
	}
	let mut instr = compile_flisp::compile(&parsed, &flags).unwrap_or_else(|e| {
		eprintln!("Compilation error ({})", e);
		exit(-1);
	});
	if !flags.debug {
		optimise_flisp::remove_unused_labels(&mut instr);
		optimise_flisp::repeat_rts(&mut instr);
	}
	let mut compiled = text::instructions_to_text(&instr, &flags).unwrap_or_else(|e| {
		eprintln!("Program too large? ({})", e);
		exit(-1);
	});
	text::automatic_imports(&mut compiled, flags.debug);
	if flags.debug {
		compiled.insert_str(0, "\tORG\t$20\n");
	}
	if flags.print_result {
		println!("{}", &compiled);
	}
	if !flags.print_result || flags.assemble {
		fs::write(&flags.out, &compiled).unwrap_or_else(|e| {
			eprintln!("IO Error: Could not save file ({})", e);
			exit(-1);
		});
	}
	if flags.assemble {
		let path = std::env::vars()
			.find(|(name, _)| name == "PATH")
			.map(|(_, paths)| paths);
		if let Some(p) = path
			.as_ref()
			.map(|s| {
				s.split(':')
					.filter_map(|p| {
						let path: PathBuf = (p.to_string() + "/qaflisp").into();
						if path.exists() {
							Some(path)
						} else {
							None
						}
					})
					.next()
			})
			.flatten()
		{
			let res = Command::new(p)
				.arg(flags.out)
				.output()
				.map(|out| String::from_utf8(out.stdout))
				.unwrap_or_else(|_| {
					eprintln!("Failed to call qaflisp");
					exit(-1);
				})
				.unwrap_or_else(|_| {
					eprintln!("Output isn't utf8?");
					exit(-1);
				});
			print!("{}", res);
		} else {
			eprintln!("Couldn't find qaflisp");
			exit(-1);
		}
	}
}

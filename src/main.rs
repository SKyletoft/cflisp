#![feature(slice_as_chunks)]

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
	let parsed = parser::parse(&source, !flags.debug).expect("Parse error");
	if flags.type_check {
		let ok = parser::type_check(&parsed, &[], &[]).expect("Name error");
		if !ok {
			eprintln!("Error: type check or name resolution error");
			exit(-1);
		}
	}
	if flags.tree {
		dbg!(&parsed);
	}
	let mut instr = compile_flisp::compile(&parsed, &flags).expect("Compiler error");
	if !flags.debug {
		optimise_flisp::remove_unused_labels(&mut instr);
		optimise_flisp::repeat_rts(&mut instr);
	}
	let mut compiled = text::instructions_to_text(&instr, &flags).expect("Too long?");
	text::automatic_imports(&mut compiled);
	if flags.debug {
		compiled.insert_str(0, "\tORG\t$20\n");
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

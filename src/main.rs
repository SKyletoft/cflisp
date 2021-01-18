use std::{env, fs};

pub mod compile_flisp;
pub mod error;
pub mod helper;
pub mod language_element;
pub mod parser;
pub mod statement_element;
pub mod statement_token;
pub mod token;
pub mod types;

use error::{CompileError, ParseError};
use language_element::LanguageElement;
use statement_token::StatementToken;
use token::Token;
use types::Variable;

fn main() {
	let args = env::args().skip(1).collect::<Vec<_>>();
	let _flags = args
		.iter()
		.filter(|s| s.starts_with('-'))
		.map(|s| s.as_str())
		.collect::<Vec<&str>>();
	let files = args
		.iter()
		.filter(|s| !s.starts_with('-') && !s.is_empty())
		.map(|s| s.as_str())
		.collect::<Vec<&str>>();
	if files.is_empty() {
		eprintln!("Error: No input files");
		return;
	}
	let mut source = String::new();
	for file in files {
		source.push_str(&fs::read_to_string(file).expect("IO Error: Could not read file"));
		source.push('\n');
	}
	let parsed = parser::parse(&source).expect("Parse error");
	let ok = parser::type_check(&parsed, &[], &[]);
	if !ok {
		eprintln!("Error: type check error");
		return;
	} else {
		println!("type check passed");
	}
	dbg!(&parsed);
	let compiled = compile_flisp::compile(&parsed /*&flags*/).expect("Compiler error");
	fs::write("./a.sflisp", &compiled).expect("IO Error: Could not save file");
}

struct Flags;

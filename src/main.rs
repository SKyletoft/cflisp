#![allow(dead_code)] //Remove once everything actually works

pub mod error;
use error::ParseError;
pub mod helper;
pub mod parser;
pub mod types;

fn main() {
	let args = std::env::args().skip(1).collect::<Vec<_>>();
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
		source.push_str(&std::fs::read_to_string(file).expect("IO Error: Could not read file"));
		source.push('\n');
	}
	let parsed = parser::parse(&source /*&flags*/).expect("Parse error");
	dbg!(&parsed);
	return;
	let compiled = compile(&parsed).expect("Compiler error");
	std::fs::write("./a.sflisp", &compiled).expect("IO Error: Could not save file");
}

struct Flags;

fn compile(_tree: &[types::LanguageElement]) -> Result<String, ParseError> {
	Err(ParseError(
		line!(),
		"Parse finished, continued to compilation. Cancelling",
	))
}

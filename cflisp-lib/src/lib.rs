pub mod error;
pub mod flags;
#[macro_use]
mod helper;
pub mod flisp;
pub mod parsing;
mod printing;
pub mod processing;
pub mod tests;
pub mod types;

use error::{CflispError, ErrorInfo, ErrorTypes, Result};
use flags::Flags;
use flisp::flisp_instructions::{Addressing, CommentedInstruction, Instruction};
use parsing::{
	language_element::LanguageElement,
	parser::{Parsable, StaticConstVolatile},
	statement_element::{MaybeParsed, StatementElement},
	statement_token::StatementToken,
	token::Token,
};
use processing::{
	structless_language::StructlessLanguage, structless_statement::StructlessStatement,
};
use types::{
	BinOp, Block, BlockStructless, Function, NativeType, NativeVariable, Number, NumberType,
	Statement, TokenSlice, Type, Variable,
};

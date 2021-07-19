pub mod error;
pub mod flags;
#[macro_use]
mod helper;
pub mod flisp;
pub mod language_element;
pub mod lexer;
pub mod optimise_language;
pub mod optimise_statement;
pub mod parser;
pub mod statement_element;
pub mod statement_token;
pub mod structless_language;
pub mod structless_statement;
pub mod tests;
pub mod token;
pub mod type_checker;
pub mod types;

use error::{CompileError, ParseError, TypeError};
use flags::Flags;
use flisp::flisp_instructions::{Addressing, CommentedInstruction, Instruction};
use language_element::LanguageElement;
use statement_element::StatementElement;
use statement_token::StatementToken;
use structless_language::StructlessLanguage;
use structless_statement::StructlessStatement;
use token::{Token, Token::*};
use types::{
	BinOp, Block, BlockStructless, Function, NativeType, NativeVariable, Number, NumberType,
	Statement, Type, Variable,
};

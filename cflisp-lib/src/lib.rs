pub mod compile_flisp;
pub mod error;
pub mod flags;
pub mod flisp_instructions;
mod helper;
pub mod language_element;
pub mod lexer;
pub mod optimise_flisp;
pub mod optimise_language;
pub mod optimise_statement;
pub mod parser;
pub mod statement_element;
pub mod statement_token;
pub mod structless;
pub mod tests;
pub mod text;
pub mod token;
pub mod type_checker;
pub mod types;

use error::{CompileError, ParseError};
use flags::Flags;
use flisp_instructions::{Addressing, CommentedInstruction, Instruction};
use language_element::LanguageElement;
use statement_element::StatementElement;
use statement_token::StatementToken;
use structless::{LanguageElementStructless, StatementElementStructless};
use token::{Token, Token::*};
use types::{
	Block, BlockStructless, Function, NativeType, NativeVariable, Statement, Type, Variable,
};

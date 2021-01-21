use crate::*;
use flisp_instructions::Instruction;
use statement_element::StatementElement;

pub(crate) fn compile(program: &[LanguageElement]) -> Result<String, CompileError> {
	Err(CompileError(line!(), "Nothing yet"))
}

pub(crate) fn compile_statement(
	statement: &StatementElement,
) -> Result<Vec<Instruction>, CompileError> {
	if statement.size() > 20 {
		return Err(CompileError(line!(), "Statement is too complex"));
	}
	Err(CompileError(line!(), ""))
}

use std::{error, fmt};

///Error type for parsing
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ParseError {
	TokenFail(u32),
	SwitchStatement(u32),
	BreakContinue(u32),
	MatchFail(u32),
	NonConstantArrayLen(u32),
	FieldAccessOnNonNames(u32),
	InternalTreeFail(u32),
	InternalUnparsed(u32),
	UnknownSymbol(u32),
	InvalidToken(u32),
	UndefinedType(u32),
	UndefinedVariable(u32),
	InternalNotStruct(u32),
	WrongTypeWasNative(u32),
	UndefinedStructField(u32),
	UndefinedFunction(u32),
	IllegalStructLiteral(u32),
	IncompleteStatement(u32),
	InvalidArguments(u32),
	BadStructFields(u32),
	BrokenForLoop(u32),
	BadStructName(u32),
	AddressOfTemporary(u32),
	MisplacedOperators(u32),
	MalformedTernary(u32),
	InternalFailedConst(u32),
	InternalFailedStatic(u32),
	InternalFailedVolatile(u32),
	TreeConstructionFail(u32),
	ReturnStruct(u32),
}

impl fmt::Display for ParseError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use ParseError::*;
		let (line, error) = match self {
			TokenFail(line) => (line, "Couldn't parse token"),
			SwitchStatement(line) => (line, "Switch statements are not supported"),
			BreakContinue(line) => (line, "Break and continue are not supported"),
			MatchFail(line) => (line, "Couldn't match language pattern"),
			NonConstantArrayLen(line) => (line, "Array length wasn't constant"),
			FieldAccessOnNonNames(line) => (line, "Field access between non-names"),
			InternalTreeFail(line) => (line, "Internal tree construction error"),
			IncompleteStatement(line) => (line, "Statement ended early?"),
			InvalidArguments(line) => (line, "Couldn't parse argument list"),
			BadStructFields(line) => (line, "Couldn't parse struct fields"),
			UnknownSymbol(line) => (line, "Unknown symbol"),
			InvalidToken(line) => (line, "Token is not valid in this context"),
			UndefinedType(line) => (line, "Undefined struct type"),
			UndefinedVariable(line) => (line, "Undefined Variable"),
			UndefinedStructField(line) => (line, "Undefined field name"),
			UndefinedFunction(line) => (line, "Undefined function"),
			BrokenForLoop(line) => (line, "For loop was malformed"),
			AddressOfTemporary(line) => (line, "Tried to take address of on a non variable"),
			BadStructName(line) => (line, "Struct doesn't have the same name as its typedef"),
			InternalNotStruct(line) => (line, "Internal error: Type was not a struct type"),
			WrongTypeWasNative(line) => (line, "Variable wasn't of struct or struct pointer type"),
			InternalFailedConst(line) => (line, "Internal error: cannot make element const"),
			InternalFailedStatic(line) => (line, "Internal error: cannot make element static"),
			InternalFailedVolatile(line) => (line, "Internal error: cannot make element volatile"),
			InternalUnparsed(line) => (
				line,
				"Internal error: Last element in statement parsing vector was unparsed",
			),
			IllegalStructLiteral(line) => (
				line,
				"Only struct variables can be passed into functions with struct arguments (not \
				 literals)",
			),
			MisplacedOperators(line) => (
				line,
				"Couldn't construct tree from statement. Are you sure the operators are correctly \
				 placed?",
			),
			MalformedTernary(line) => (
				line,
				"Couldn't construct tree from statement. Element that should've been parsed first \
				 has not been parsed",
			),
			TreeConstructionFail(line) => (
				line,
				"Couldn't construct tree from statement. Element that should've been parsed first \
				 has not been parsed",
			),
			ReturnStruct(line) => (
				line,
				"Cannot return struct from function due to ABI limitation. Maybe try having an \
				 out pointer parametre instead? (Sorry)",
			),
		};
		write!(f, "Parse Error ({}): {}", line, error)
	}
}

impl error::Error for ParseError {}

///Error type for type checking
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TypeError {
	UndefinedVariable(u32),
	TypeMismatch(u32),
	AssignmentToConstant(u32),
	InternalPointerAssignmentToNonPointer(u32),
	IllegalVoidArgument(u32),
	MalformedInterruptHandlerReturn(u32),
	MalformedInterruptHandlerArguments(u32),
	UndefinedType(u32),
	MissingStructFields(u32),
	UndefinedField(u32),
	UndefinedFunction(u32),
	MissingArguments(u32),
}

impl fmt::Display for TypeError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use TypeError::*;
		let (line, error) = match self {
			UndefinedVariable(line) => (line, "Undefined variable"),
			TypeMismatch(line) => (line, "Type mismatch"),
			AssignmentToConstant(line) => (line, "Assignment to constant"),
			InternalPointerAssignmentToNonPointer(line) => {
				(line, "Internal: Pointer assignment to non pointer")
			}
			IllegalVoidArgument(line) => (line, "Function has void argument"),
			MalformedInterruptHandlerReturn(line) => {
				(line, "Interrupt handler does not return void")
			}
			MalformedInterruptHandlerArguments(line) => (line, "Interrupt handler takes arugments"),
			UndefinedType(line) => (line, "Undefined struct type"),
			MissingStructFields(line) => (
				line,
				"Not the correct amount of fields in struct initalisation",
			),
			UndefinedField(line) => (line, "Undefined struct field"),
			UndefinedFunction(line) => (line, "Undefined function"),
			MissingArguments(line) => (line, "Wrong amount of arguments in function call"),
		};
		write!(f, "Type Error ({}): {}", line, error)
	}
}

impl error::Error for TypeError {}

///Error type for compiling and optimisation
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct CompileError(pub u32, pub &'static str);

impl fmt::Display for CompileError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "Compilation Error ({}): {}", self.0, self.1)
	}
}

impl error::Error for CompileError {}

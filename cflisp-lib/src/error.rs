use std::{error, fmt};

///Error type for parsing
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ParseError {
	None,
	TokenFail(u32),
	SwitchStatement(u32),
	BreakContinue(u32),
	MatchFail(u32),
	NonConstantArrayLen(u32),
	FieldAccessOnNonNames(u32),
	InternalTreeFail(u32),
	InternalUnparsed(u32),
	InvalidToken(u32),
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
	MissingName(u32),
	BadType(u32),
	ExcessTokens(u32),
}

impl fmt::Display for ParseError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use ParseError::*;
		let (line, error) = match self {
			None => (&0, "Internal error: ParseError::None exposed"),
			TokenFail(line) => (line, "Couldn't parse token"),
			SwitchStatement(line) => (line, "Switch statements are not supported"),
			BreakContinue(line) => (line, "Break, continue and default are not supported"),
			MatchFail(line) => (line, "Couldn't match language pattern"),
			NonConstantArrayLen(line) => (line, "Array length wasn't constant"),
			FieldAccessOnNonNames(line) => (line, "Field access between non-names"),
			InternalTreeFail(line) => (line, "Internal tree construction error"),
			IncompleteStatement(line) => (line, "Statement ended early?"),
			InvalidArguments(line) => (line, "Couldn't parse argument list"),
			BadStructFields(line) => (line, "Couldn't parse struct fields"),
			InvalidToken(line) => (line, "Token is not valid in this context"),
			BrokenForLoop(line) => (line, "For loop was malformed"),
			AddressOfTemporary(line) => (line, "Tried to take address of on a non variable"),
			BadStructName(line) => (line, "Struct doesn't have the same name as its typedef"),
			InternalFailedConst(line) => (line, "Internal error: cannot make element const"),
			InternalFailedStatic(line) => (line, "Internal error: cannot make element static"),
			InternalFailedVolatile(line) => (line, "Internal error: cannot make element volatile"),
			MissingName(line) => (line, "No name when expected"),
			BadType(line) => (line, "Failed to parse type"),
			ExcessTokens(line) => (line, "Extra tokens in statement"),
			InternalUnparsed(line) => (
				line,
				"Internal error: Last element in statement parsing vector was unparsed",
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
		};
		write!(f, "Parse Error ({}): {}", line, error)
	}
}

impl error::Error for ParseError {}

///Error type for internal transformations
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum IRError {
	UnknownSymbol(u32),
	UndefinedType(u32),
	UndefinedVariable(u32),
	InternalNotStruct(u32),
	WrongTypeWasNative(u32),
	UndefinedStructField(u32),
	UndefinedFunction(u32),
	IllegalStructLiteral(u32),
	BadStructName(u32),
	ReturnStruct(u32),
}

impl fmt::Display for IRError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use IRError::*;
		let (line, error) = match self {
			UnknownSymbol(line) => (line, "Unknown symbol"),
			UndefinedType(line) => (line, "Undefined struct type"),
			UndefinedVariable(line) => (line, "Undefined Variable"),
			UndefinedStructField(line) => (line, "Undefined field name"),
			UndefinedFunction(line) => (line, "Undefined function"),
			BadStructName(line) => (line, "Struct doesn't have the same name as its typedef"),
			InternalNotStruct(line) => (line, "Internal error: Type was not a struct type"),
			WrongTypeWasNative(line) => (line, "Variable wasn't of struct or struct pointer type"),
			IllegalStructLiteral(line) => (
				line,
				"Only struct variables can be passed into functions with struct arguments (not \
				 literals)",
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

impl error::Error for IRError {}

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
			IllegalVoidArgument(line) => (line, "Function has void argument"),
			MalformedInterruptHandlerArguments(line) => (line, "Interrupt handler takes arugments"),
			UndefinedType(line) => (line, "Undefined struct type"),
			UndefinedField(line) => (line, "Undefined struct field"),
			UndefinedFunction(line) => (line, "Undefined function"),
			MissingArguments(line) => (line, "Wrong amount of arguments in function call"),
			InternalPointerAssignmentToNonPointer(line) => {
				(line, "Internal: Pointer assignment to non pointer")
			}
			MalformedInterruptHandlerReturn(line) => {
				(line, "Interrupt handler does not return void")
			}
			MissingStructFields(line) => (
				line,
				"Not the correct amount of fields in struct initalisation",
			),
		};
		write!(f, "Type Error ({}): {}", line, error)
	}
}

impl error::Error for TypeError {}

///Error type for compiling and optimisation
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CompileError {
	ProgramTooLarge(u32),
	DuplicateName(u32),
	LoneGlobalStatement(u32),
	NonConstInConstInit(u32),
	NegativeShift(u32),
	UndefinedVariable(u32),
	IllegalArrayLiteral(u32),
	InvalidAddressOf(u32),
	InternalOpOfFunction(u32),
	InternalOpOfLiteral(u32),
}

impl fmt::Display for CompileError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use CompileError::*;
		let (line, error) = match self {
			ProgramTooLarge(line) => (line, "Program is too large for digiflisp!"),
			DuplicateName(line) => (line, "Name already exists in scope!"),
			LoneGlobalStatement(line) => (line, "Lone statement in global scope"),
			NonConstInConstInit(line) => (line, "Non constant in constant initialisation"),
			NegativeShift(line) => (line, "Cannot shift by negative amount"),
			IllegalArrayLiteral(line) => (line, "Illegal array literal"),
			InternalOpOfFunction(line) => (line, "Internal error: function call, not instruction?"),
			UndefinedVariable(line) => (
				line,
				"Internal: Name resolution failed? This should've failed in typecheck",
			),
			InvalidAddressOf(line) => (
				line,
				"Invalid address of operation (should've failed in parse)",
			),
			InternalOpOfLiteral(line) => (
				line,
				"Internal error: special cases and literals, not instructions?",
			),
		};
		write!(f, "Compile Error ({}): {}", line, error)
	}
}

impl error::Error for CompileError {}

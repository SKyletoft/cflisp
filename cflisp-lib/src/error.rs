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
		let (line, error) = match self {
			ParseError::TokenFail(line) => (line, "Couldn't parse token"),
			ParseError::SwitchStatement(line) => (line, "Switch statements are not supported"),
			ParseError::BreakContinue(line) => (line, "Break and continue are not supported"),
			ParseError::MatchFail(line) => (line, "Couldn't match language pattern"),
			ParseError::NonConstantArrayLen(line) => (line, "Array length wasn't constant"),
			ParseError::FieldAccessOnNonNames(line) => (line, "Field access between non-names"),
			ParseError::InternalTreeFail(line) => (line, "Internal tree construction error"),
			ParseError::IncompleteStatement(line) => (line, "Statement ended early?"),
			ParseError::InvalidArguments(line) => (line, "Couldn't parse argument list"),
			ParseError::BadStructFields(line) => (line, "Couldn't parse struct fields"),
			ParseError::UnknownSymbol(line) => (line, "Unknown symbol"),
			ParseError::InvalidToken(line) => (line, "Token is not valid in this context"),
			ParseError::UndefinedType(line) => (line, "Undefined struct type"),
			ParseError::UndefinedVariable(line) => (line, "Undefined Variable"),
			ParseError::UndefinedStructField(line) => (line, "Undefined field name"),
			ParseError::UndefinedFunction(line) => (line, "Undefined function"),
			ParseError::BrokenForLoop(line) => (line, "For loop was malformed"),
			ParseError::AddressOfTemporary(line) => {
				(line, "Tried to take address of on a non variable")
			}
			ParseError::BadStructName(line) => {
				(line, "Struct doesn't have the same name as its typedef")
			}
			ParseError::InternalUnparsed(line) => (
				line,
				"Internal error: Last element in statement parsing vector was unparsed",
			),
			ParseError::InternalNotStruct(line) => {
				(line, "Internal error: Type was not a struct type")
			}
			ParseError::WrongTypeWasNative(line) => {
				(line, "Variable wasn't of struct or struct pointer type")
			}
			ParseError::IllegalStructLiteral(line) => (
				line,
				"Only struct variables can be passed into functions with struct arguments (not \
				 literals)",
			),
			ParseError::MisplacedOperators(line) => (
				line,
				"Couldn't construct tree from statement. Are you sure the operators are correctly \
				 placed?",
			),
			ParseError::MalformedTernary(line) => (
				line,
				"Couldn't construct tree from statement. Element that should've been parsed first \
				 has not been parsed",
			),
			ParseError::InternalFailedConst(line) => {
				(line, "Internal error: cannot make element const")
			}
			ParseError::InternalFailedStatic(line) => {
				(line, "Internal error: cannot make element static")
			}
			ParseError::InternalFailedVolatile(line) => {
				(line, "Internal error: cannot make element volatile")
			}
			ParseError::TreeConstructionFail(line) => (
				line,
				"Couldn't construct tree from statement. Element that should've been parsed first \
				 has not been parsed",
			),
			ParseError::ReturnStruct(line) => (
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
pub struct TypeError(pub u32, pub &'static str);

impl fmt::Display for TypeError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "Parse Error ({}): {}", self.0, self.1)
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

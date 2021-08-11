use std::{error, fmt, result};

use crate::*;

pub type Result<T> = result::Result<T, CflispError>;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ErrorInfo<'a> {
	Source(String),
	Token(Vec<Token<'a>>),
	StatementToken(Vec<StatementToken<'a>>),
	Statement(StatementElement<'a>),
	MaybeParsed(Vec<MaybeParsed<'a>>),
	Language(LanguageElement<'a>),
	StructlessStatement(StructlessStatement<'a>),
	StructlessLanguage(StructlessLanguage<'a>),
	UnTree(StatementElement<'a>, StatementToken<'a>),
	BinTree(
		StatementElement<'a>,
		StatementElement<'a>,
		StatementToken<'a>,
	),
	Variable(Variable<'a>),
}

impl<'a> From<&'a str> for ErrorInfo<'a> {
	fn from(s: &'a str) -> Self {
		ErrorInfo::Source(s.to_string())
	}
}

impl<'a> From<String> for ErrorInfo<'a> {
	fn from(s: String) -> Self {
		ErrorInfo::Source(s)
	}
}

impl<'a> From<&[Token<'a>]> for ErrorInfo<'a> {
	fn from(slice: &[Token<'a>]) -> Self {
		ErrorInfo::Token(slice.to_owned())
	}
}

impl<'a> From<Vec<Token<'a>>> for ErrorInfo<'a> {
	fn from(slice: Vec<Token<'a>>) -> Self {
		ErrorInfo::Token(slice)
	}
}

impl<'a> From<&[StatementToken<'a>]> for ErrorInfo<'a> {
	fn from(slice: &[StatementToken<'a>]) -> Self {
		ErrorInfo::StatementToken(slice.to_owned())
	}
}

impl<'a> From<Vec<StatementToken<'a>>> for ErrorInfo<'a> {
	fn from(slice: Vec<StatementToken<'a>>) -> Self {
		ErrorInfo::StatementToken(slice)
	}
}

impl<'a> From<&[MaybeParsed<'a>]> for ErrorInfo<'a> {
	fn from(slice: &[MaybeParsed<'a>]) -> Self {
		ErrorInfo::MaybeParsed(slice.to_owned())
	}
}

impl<'a> From<Vec<MaybeParsed<'a>>> for ErrorInfo<'a> {
	fn from(slice: Vec<MaybeParsed<'a>>) -> Self {
		ErrorInfo::MaybeParsed(slice)
	}
}

impl<'a> From<&StatementElement<'a>> for ErrorInfo<'a> {
	fn from(elem: &StatementElement<'a>) -> Self {
		ErrorInfo::Statement(elem.to_owned())
	}
}

impl<'a> From<&LanguageElement<'a>> for ErrorInfo<'a> {
	fn from(elem: &LanguageElement<'a>) -> Self {
		ErrorInfo::Language(elem.to_owned())
	}
}

impl<'a> From<&StructlessLanguage<'a>> for ErrorInfo<'a> {
	fn from(elem: &StructlessLanguage<'a>) -> Self {
		ErrorInfo::StructlessLanguage(elem.to_owned())
	}
}

impl<'a> From<&StructlessStatement<'a>> for ErrorInfo<'a> {
	fn from(elem: &StructlessStatement<'a>) -> Self {
		ErrorInfo::StructlessStatement(elem.to_owned())
	}
}

impl<'a> From<(&StatementElement<'a>, &StatementToken<'a>)> for ErrorInfo<'a> {
	fn from((a, b): (&StatementElement<'a>, &StatementToken<'a>)) -> Self {
		ErrorInfo::UnTree(a.to_owned(), b.to_owned())
	}
}

impl<'a>
	From<(
		&StatementElement<'a>,
		&StatementElement<'a>,
		&StatementToken<'a>,
	)> for ErrorInfo<'a>
{
	fn from(
		(a, b, c): (
			&StatementElement<'a>,
			&StatementElement<'a>,
			&StatementToken<'a>,
		),
	) -> Self {
		ErrorInfo::BinTree(a.to_owned(), b.to_owned(), c.to_owned())
	}
}

impl<'a> From<&Variable<'a>> for ErrorInfo<'a> {
	fn from(v: &Variable<'a>) -> Self {
		ErrorInfo::Variable(v.to_owned())
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct CflispError {
	pub info: String,
	pub line: u32,
	pub file: &'static str,
	pub error: ErrorTypes,
}

impl CflispError {
	pub fn new(info: String, line: u32, file: &'static str, error: ErrorTypes) -> Self {
		CflispError {
			info,
			line,
			file,
			error,
		}
	}
}

//This is a macro so that line and file can be passed in instead of referring to error.rs
#[macro_export]
macro_rules! error {
	($type: ident) => {{
		CflispError::new(
			String::new(), //ErrorInfo::None,
			line!(),
			file!(),
			ErrorTypes::$type,
		)
	}};

	($type: ident, $context: expr) => {{
		CflispError::new(
			format!("{}", ErrorInfo::from($context)),
			line!(),
			file!(),
			ErrorTypes::$type,
		)
	}};
}

///Error type for parsing
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ErrorTypes {
	None,
	TokenFail,
	SwitchStatement,
	BreakContinue,
	MatchFail,
	NonConstantArrayLen,
	FieldAccessOnNonNames,
	InternalTreeFail,
	InternalUnparsed,
	InvalidToken,
	IncompleteStatement,
	InvalidArguments,
	BadStructFields,
	BrokenForLoop,
	BadStructName,
	AddressOfTemporary,
	MisplacedOperators,
	MalformedTernary,
	InternalFailedConst,
	InternalFailedStatic,
	InternalFailedVolatile,
	TreeConstructionFail,
	MissingName,
	BadType,
	ExcessTokens,
	UnknownSymbol,
	UndefinedType,
	UndefinedVariable,
	InternalNotStruct,
	WrongTypeWasNative,
	UndefinedStructField,
	UndefinedFunction,
	IllegalStructLiteral,
	ReturnStruct,
	TypeMismatch,
	AssignmentToConstant,
	InternalPointerAssignmentToNonPointer,
	IllegalVoidArgument,
	MalformedInterruptHandlerReturn,
	MalformedInterruptHandlerArguments,
	MissingStructFields,
	UndefinedField,
	MissingArguments,
	DerefNonPointer,
	ProgramTooLarge,
	DuplicateName,
	LoneGlobalStatement,
	NonConstInConstInit,
	NegativeShift,
	IllegalArrayLiteral,
	InvalidAddressOf,
	InternalOpOfFunction,
	InternalOpOfLiteral,
	InvalidImport,
}

impl fmt::Display for ErrorTypes {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use ErrorTypes::*;
		let msg = match self {
			None => "Internal error: ParseError::None exposed",
			TokenFail => "Couldn't parse token",
			SwitchStatement => "Switch statements are not supported",
			BreakContinue => "Break, continue and default are not supported",
			MatchFail => "Couldn't match language pattern",
			NonConstantArrayLen => "Array length wasn't constant",
			FieldAccessOnNonNames => "Field access between non-names",
			InternalTreeFail => "Internal tree construction error",
			IncompleteStatement => "Statement ended early?",
			InvalidArguments => "Couldn't parse argument list",
			BadStructFields => "Couldn't parse struct fields",
			InvalidToken => "Token is not valid in this context",
			BrokenForLoop => "For loop was malformed",
			AddressOfTemporary => "Tried to take address of on a non variable",
			BadStructName => "Struct doesn't have the same name as its typedef",
			InternalFailedConst => "Internal error: cannot make element const",
			InternalFailedStatic => "Internal error: cannot make element static",
			InternalFailedVolatile => "Internal error: cannot make element volatile",
			MissingName => "No name when expected",
			BadType => "Failed to parse type",
			ExcessTokens => "Extra tokens in statement",
			InternalUnparsed => {
				"Internal error: Last element in statement parsing vector was unparsed"
			}
			MisplacedOperators => {
				"Couldn't construct tree from statement. Are you sure the operators are correctly \
				 placed?"
			}
			MalformedTernary => {
				"Couldn't construct tree from statement. Element that should've been parsed first \
				 has not been parsed"
			}
			TreeConstructionFail => {
				"Couldn't construct tree from statement. Element that should've been parsed first \
				 has not been parsed"
			}
			UnknownSymbol => "Unknown symbol",
			UndefinedType => "Undefined struct type",
			UndefinedVariable => "Undefined Variable",
			UndefinedStructField => "Undefined field name",
			UndefinedFunction => "Undefined function",
			InternalNotStruct => "Internal error: Type was not a struct type",
			WrongTypeWasNative => "Variable wasn't of struct or struct pointer type",
			IllegalStructLiteral => {
				"Only struct variables can be passed into functions with struct arguments (not \
				 literals)"
			}
			ReturnStruct => {
				"Cannot return struct from function due to ABI limitation. Maybe try having an out \
				 pointer parametre instead? (Sorry)"
			}
			TypeMismatch => "Type mismatch",
			AssignmentToConstant => "Assignment to constant",
			IllegalVoidArgument => "Function has void argument",
			MalformedInterruptHandlerArguments => "Interrupt handler takes arugments",
			UndefinedField => "Undefined struct field",
			MissingArguments => "Wrong amount of arguments in function call",
			DerefNonPointer => "Dereference of non-pointer",
			InternalPointerAssignmentToNonPointer => "Internal: Pointer assignment to non pointer",
			MalformedInterruptHandlerReturn => "Interrupt handler does not return void",
			MissingStructFields => "Not the correct amount of fields in struct initalisation",
			ProgramTooLarge => "Program is too large for digiflisp!",
			DuplicateName => "Name already exists in scope!",
			LoneGlobalStatement => "Lone statement in global scope",
			NonConstInConstInit => "Non constant in constant initialisation",
			NegativeShift => "Cannot shift by negative amount",
			IllegalArrayLiteral => "Illegal array literal",
			InternalOpOfFunction => "Internal error: function call, not instruction?",
			InvalidAddressOf => "Invalid address of operation (should've failed in parse)",
			InternalOpOfLiteral => "Internal error: special cases and literals, not instructions?",
			InvalidImport => "Cannot find file to import",
		};
		write!(f, "{}", msg)
	}
}

impl fmt::Display for CflispError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(
			f,
			"Error: {} on {:?} ({}:{})",
			self.error, self.info, self.file, self.line
		)
	}
}

impl error::Error for CflispError {}

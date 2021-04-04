use std::{error, fmt};

///Error type for parsing, type checking and IR optimisation
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ParseError(pub u32, pub &'static str);

impl fmt::Display for ParseError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "Parse Error ({}): {}", self.0, self.1)
	}
}

impl error::Error for ParseError {}

///Error type for compiling and optimisation
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct CompileError(pub u32, pub &'static str);

impl fmt::Display for CompileError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "Compilation Error ({}): {}", self.0, self.1)
	}
}

impl error::Error for CompileError {}

use std::{error, fmt};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ParseError(pub u32);

impl fmt::Display for ParseError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "Parse Error ({})", self.0)
	}
}

impl error::Error for ParseError {}

use crate::*;
use types::{LanguageElement, StatementToken, StatementToken::*, Token, Token::*, Type, Type::*};

pub(crate) fn parse(source: &str) -> Result<Vec<LanguageElement>, ParseError> {
	let tokens = Token::parse_str_to_vec(source)?;
	let elements = {
		match tokens.as_slice() {
			[If, Parens(_), ..] => {todo!()}
			_ => {
				return Err(ParseError(
					line!(),
					"Couldn't match tokens into LanguageElement pattern",
				))
			}
		}
	};
	Err(ParseError(line!(), "no error, just end of parse"))
}

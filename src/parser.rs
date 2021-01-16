use crate::*;
use types::{
	LanguageElement, StatementElement, StatementToken, StatementToken::*, Token, Token::*, Type,
	Type::*,
};

pub(crate) fn parse<'a>(source: &'a str) -> Result<Vec<LanguageElement<'a>>, ParseError> {
	let tokens: Vec<Token<'a>> = Token::parse_str_to_vec(source)?;
	construct_block(&tokens)
}

pub(crate) fn construct_block<'a>(
	tokens: &[Token<'a>],
) -> Result<Vec<LanguageElement<'a>>, ParseError> {
	let mut res = Vec::new();
	for token in tokens.split(|t| t == &NewLine).filter(|t| !t.is_empty()) {
		res.push(construct_structure_from_tokens(token)?);
	}
	Ok(res)
}

pub(crate) fn construct_structure_from_tokens<'a>(
	tokens: &[Token<'a>],
) -> Result<LanguageElement<'a>, ParseError> {
	let element = {
		match tokens {
			[Decl(t), Token::Name(n), Args(args), UnparsedBlock(code)] => {
				let code_tokenised = Token::parse_block_tokens(UnparsedBlock(code))?;
				let code_parsed = construct_block(&code_tokenised)?;
				LanguageElement::FunctionDeclaration {
					typ: *t,
					name: *n,
					args: args.clone(),
					block: code_parsed,
				}
			}
			[Decl(t), Token::Name(n), UnparsedBlock(args), UnparsedBlock(code)] => {
				let args_parsed = Token::parse_argument_list_tokens(UnparsedBlock(args))?;
				let code_parsed = parse(code)?;
				LanguageElement::FunctionDeclaration {
					typ: *t,
					name: *n,
					args: args_parsed,
					block: code_parsed,
				}
			}
			[Decl(t), Token::Name(n), Assign, ..] => {
				let rhs = &tokens[3..];
				let rhs_verified = StatementToken::from_tokens(rhs)?;
				let rhs_parsed = StatementElement::from_tokens(rhs_verified)?;
				LanguageElement::VariableDecarationAssignment {
					typ: *t,
					name: *n,
					value: rhs_parsed,
				}
			}
			[If, Parens(_), ..] => {
				todo!()
			}
			_ => {
				dbg!(tokens);
				return Err(ParseError(
					line!(),
					"Couldn't match tokens into LanguageElement pattern",
				));
			}
		}
	};
	Ok(element)
}

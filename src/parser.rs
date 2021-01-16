use crate::*;
use helper::remove_parentheses;
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
			[Token::Name(n), Assign, ..] => {
				let rhs = &tokens[2..];
				let rhs_verified = StatementToken::from_tokens(rhs)?;
				let rhs_parsed = StatementElement::from_tokens(rhs_verified)?;
				LanguageElement::VariableAssignment {
					name: *n,
					value: rhs_parsed,
				}
			}
			[Decl(t), Token::Name(n)] => LanguageElement::VariableDeclaration { typ: *t, name: *n },
			[If, UnparsedBlock(cond), UnparsedBlock(code)] => {
				let condition = Token::parse_statement_tokens(UnparsedBlock(cond))?;
				let condition_parsed = StatementElement::from_tokens(condition)?;
				let then_parsed = parse(helper::remove_parentheses(code))?;
				LanguageElement::IfStatement {
					condition: condition_parsed,
					then: then_parsed,
					else_then: None,
				}
			}
			[If, UnparsedBlock(cond), UnparsedBlock(then_code), Else, UnparsedBlock(else_code)] => {
				let condition = Token::parse_statement_tokens(UnparsedBlock(cond))?;
				let condition_parsed = StatementElement::from_tokens(condition)?;
				let then_parsed = parse(helper::remove_parentheses(then_code))?;
				let else_parsed = parse(helper::remove_parentheses(else_code))?;
				LanguageElement::IfStatement {
					condition: condition_parsed,
					then: then_parsed,
					else_then: Some(else_parsed),
				}
			}
			[If, UnparsedBlock(cond), UnparsedBlock(then_code), Else, If, ..] => {
				let condition = Token::parse_statement_tokens(UnparsedBlock(cond))?;
				let condition_parsed = StatementElement::from_tokens(condition)?;
				let then_parsed = parse(then_code)?;
				let else_if_tokens = &tokens[5..];
				let else_if_parsed = construct_structure_from_tokens(else_if_tokens)?;
				LanguageElement::IfStatement {
					condition: condition_parsed,
					then: then_parsed,
					else_then: Some(vec![else_if_parsed]),
				}
			}
			_ => {
				return Err(ParseError(
					line!(),
					"Couldn't match tokens into LanguageElement pattern",
				));
			}
		}
	};
	Ok(element)
}

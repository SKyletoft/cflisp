use crate::*;
use language_element::LanguageElement;
use statement_element::StatementElement;
use statement_token::StatementToken;
use token::{Token, Token::*};
use types::{Function, Type};

pub(crate) fn parse<'a>(source: &'a str) -> Result<Vec<LanguageElement<'a>>, ParseError> {
	let tokens: Vec<Token<'a>> = Token::parse_str_to_vec(source)?;
	construct_block(&tokens)
}

pub(crate) fn construct_block<'a>(
	tokens: &[Token<'a>],
) -> Result<Vec<LanguageElement<'a>>, ParseError> {
	let mut res = Vec::new();
	//REVISIT LATER. SPLITTING AT NEWLINE CAUSES ERRORS WITH ELSE BLOCKS
	for token in tokens.split(|t| t == &NewLine).filter(|t| !t.is_empty()) {
		res.push(construct_structure_from_tokens(token)?);
	}
	statement_element::move_declarations_first(&mut res);
	Ok(res)
}

pub(crate) fn construct_structure_from_tokens<'a>(
	tokens: &[Token<'a>],
) -> Result<LanguageElement<'a>, ParseError> {
	let element = {
		match tokens {
			//Function declaration
			[Decl(t), Token::Name(n), Args(args), UnparsedBlock(code)] => {
				let code_tokenised = Token::parse_block_tokens(UnparsedBlock(code))?;
				let code_parsed = construct_block(&code_tokenised)?;
				LanguageElement::FunctionDeclaration {
					typ: t.clone(),
					name: *n,
					args: args.clone(),
					block: code_parsed,
				}
			}
			//Function declaration?
			[Decl(t), Token::Name(n), UnparsedBlock(args), UnparsedBlock(code)] => {
				let args_parsed = Token::parse_argument_list_tokens(UnparsedBlock(args))?;
				let code_parsed = parse(code)?;
				LanguageElement::FunctionDeclaration {
					typ: t.clone(),
					name: *n,
					args: args_parsed,
					block: code_parsed,
				}
			}
			//Pointer variabler declaration
			[Decl(t), Deref(d), Assign, ..] => {
				let mut typ = t.clone();
				let mut r = d;
				let mut name = "";
				while let [UnparsedBlock(b)] = r.as_ref() {
					typ = Type::Ptr(Box::new(typ));
					if let [Token::Name(n)] = Token::parse_str_to_vec(b)?.as_slice() {
						name = *n;
						break;
					} else if let [Token::Deref(d)] = r.as_ref() {
						r = d;
					} else {
						dbg!(tokens);
						return Err(ParseError(line!(), "Couldn't parse name/pointer type"));
					}
				}
				if name.is_empty() {
					dbg!(tokens);
					return Err(ParseError(line!(), "Couldn't parse name/pointer type"));
				}
				let rhs = &tokens[3..];
				let rhs_verified = StatementToken::from_tokens(rhs)?;
				let rhs_parsed = StatementElement::from_tokens(rhs_verified)?;
				LanguageElement::VariableDecarationAssignment {
					typ,
					name,
					value: rhs_parsed,
				}
			}
			//Variable declaration
			[Decl(t), Token::Name(n), Assign, ..] => {
				let rhs = &tokens[3..];
				let rhs_verified = StatementToken::from_tokens(rhs)?;
				let rhs_parsed = StatementElement::from_tokens(rhs_verified)?;
				LanguageElement::VariableDecarationAssignment {
					typ: t.clone(),
					name: *n,
					value: rhs_parsed,
				}
			}
			//Variable assignment
			[Token::Name(n), Assign, ..] => {
				let rhs = &tokens[2..];
				let rhs_verified = StatementToken::from_tokens(rhs)?;
				let rhs_parsed = StatementElement::from_tokens(rhs_verified)?;
				LanguageElement::VariableAssignment {
					name: *n,
					value: rhs_parsed,
				}
			}
			//Pointer assignment
			[Deref(d), Assign, ..] => {
				let ptr = StatementElement::from_tokens(StatementToken::from_tokens(d.as_ref())?)?;
				let rhs = &tokens[2..];
				let rhs_verified = StatementToken::from_tokens(rhs)?;
				let rhs_parsed = StatementElement::from_tokens(rhs_verified)?;
				LanguageElement::PointerAssignment {
					ptr,
					value: rhs_parsed,
				}
			}
			//Variable declaration (without init)
			[Decl(t), Token::Name(n)] => LanguageElement::VariableDeclaration {
				typ: t.clone(),
				name: *n,
			},
			//If else if
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
			//If else
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
			//If
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
			//For
			[For, UnparsedBlock(init_cond_post), UnparsedBlock(code)] => {
				let split = helper::remove_parentheses(init_cond_post)
					.split(';')
					.collect::<Vec<_>>();
				if split.len() != 3 {
					return Err(ParseError(
						line!(),
						"For loop didn't contain three sections!",
					));
				}
				let mut init_vec = parse(split[0])?;
				if init_vec.len() != 1 {
					dbg!(init_vec);
					return Err(ParseError(line!(), "For loop init failed"));
				}
				let init_parsed = init_vec.pop().expect("Above check failed?");
				let post_vec = parse(split[2])?;
				let condition = Token::parse_statement_tokens(UnparsedBlock(split[1]))?;
				let condition_parsed = StatementElement::from_tokens(condition)?;
				let body_parsed = parse(helper::remove_parentheses(code))?;

				LanguageElement::For {
					init: Box::new(init_parsed),
					condition: condition_parsed,
					after: post_vec,
					body: body_parsed,
				}
			}
			//While
			[While, UnparsedBlock(cond), UnparsedBlock(code)] => {
				let condition = Token::parse_statement_tokens(UnparsedBlock(cond))?;
				let condition_parsed = StatementElement::from_tokens(condition)?;
				let body_parsed = parse(helper::remove_parentheses(code))?;
				LanguageElement::While {
					condition: condition_parsed,
					body: body_parsed,
				}
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

pub(crate) fn type_check(
	block: &[LanguageElement],
	upper_variables: &[Variable],
	outer_functions: &[Function],
) -> Result<bool, ParseError> {
	let mut variables = upper_variables.to_owned();
	let mut functions = outer_functions.to_owned();

	for line in block {
		match line {
			LanguageElement::VariableDeclaration { typ, name } => variables.push(Variable {
				typ: typ.clone(),
				name: *name,
			}),
			LanguageElement::VariableAssignment { name: _, value } => {
				if !value.type_check(&variables, &functions)? {
					return Ok(false);
				}
			}
			LanguageElement::VariableDecarationAssignment { typ, name, value } => {
				variables.push(Variable {
					typ: typ.clone(),
					name: *name,
				});
				if !value.type_check(&variables, &functions)? {
					return Ok(false);
				}
			}
			LanguageElement::PointerAssignment { ptr, value } => {
				if !ptr.type_check(&variables, &functions)?
					|| !value.type_check(&variables, &functions)?
				{
					return Ok(false);
				}
			}
			LanguageElement::FunctionDeclaration {
				typ,
				name,
				args,
				block,
			} => {
				functions.push(Function {
					return_type: typ.clone(),
					name: *name,
					parametres: args.clone(),
				});
				if !type_check(block, &variables, &functions)? {
					return Ok(false);
				}
			}
			LanguageElement::IfStatement {
				condition,
				then,
				else_then,
			} => {
				if !condition.type_check(&variables, &functions)?
					|| !type_check(then, &variables, &functions)?
					|| !else_then
						.as_deref()
						.map(|v| type_check(v, &variables, &functions))
						.unwrap_or(Ok(true))?
				{
					return Ok(false);
				}
			}
			LanguageElement::For {
				init,
				condition,
				after,
				body,
			} => {
				let init_as_slice = std::slice::from_ref(init.as_ref());
				if !condition.type_check(&variables, &functions)?
					|| !type_check(init_as_slice, &variables, &functions)?
					|| !type_check(after, &variables, &functions)?
					|| !type_check(body, &variables, &functions)?
				{
					return Ok(false);
				}
			}
			LanguageElement::While { condition, body } => {
				if !condition.type_check(&variables, &functions)?
					|| !type_check(body, &variables, &functions)?
				{
					return Ok(false);
				}
			}
		}
	}

	Ok(true)
}

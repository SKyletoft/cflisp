use crate::*;

pub(crate) fn parse<'a>(
	source: &'a str,
	move_first: bool,
) -> Result<Vec<LanguageElement<'a>>, ParseError> {
	let tokens: Vec<Token<'a>> = Token::parse_str_to_vec(source)?;
	construct_block(&tokens, move_first)
}

fn construct_block<'a>(
	tokens: &[Token<'a>],
	move_first: bool,
) -> Result<Vec<LanguageElement<'a>>, ParseError> {
	let mut res = Vec::new();
	for token in split_token_lines(tokens) {
		res.push(construct_structure_from_tokens(token, move_first)?);
	}
	if move_first {
		statement_element::move_declarations_first(&mut res);
	}
	Ok(res)
}

fn construct_structure_from_tokens<'a>(
	tokens: &[Token<'a>],
	move_first: bool,
) -> Result<LanguageElement<'a>, ParseError> {
	let element = {
		match tokens {
			//Function declaration
			[Decl(t), Token::Name(n), Args(args), UnparsedBlock(code)] => {
				let code_tokenised = Token::parse_block_tokens(UnparsedBlock(code))?;
				let code_parsed = construct_block(&code_tokenised, move_first)?;
				LanguageElement::FunctionDeclaration {
					typ: t.clone(),
					name: *n,
					args: args.clone(),
					block: code_parsed,
				}
			}

			//Function declaration
			[Decl(t), Token::Name(n), UnparsedBlock(args), UnparsedBlock(code)] => {
				let args_parsed = Token::parse_argument_list_tokens(UnparsedBlock(args))?;
				let code_tokenised = Token::parse_block_tokens(UnparsedBlock(code))?;
				let code_parsed = construct_block(&code_tokenised, move_first)?;
				LanguageElement::FunctionDeclaration {
					typ: t.clone(),
					name: *n,
					args: args_parsed,
					block: code_parsed,
				}
			}

			//Pointer variable declaration
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
				LanguageElement::VariableDeclarationAssignment {
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
				LanguageElement::VariableDeclarationAssignment {
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
				let then_parsed = parse(helper::remove_parentheses(then_code), move_first)?;
				let else_if_tokens = &tokens[4..];
				let else_if_parsed = construct_structure_from_tokens(else_if_tokens, move_first)?;
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
				let then_parsed = parse(helper::remove_parentheses(then_code), move_first)?;
				let else_parsed = parse(helper::remove_parentheses(else_code), move_first)?;
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
				let then_parsed = parse(helper::remove_parentheses(code), move_first)?;
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
					.map(str::trim)
					.collect::<Vec<_>>();
				if split.len() != 3 {
					return Err(ParseError(
						line!(),
						"For loop didn't contain three sections!",
					));
				}

				let condition_tokens = Token::parse_str_to_vec(split[1])?;
				let condition_statement_tokens = StatementToken::from_tokens(&condition_tokens)?;
				let condition = StatementElement::from_tokens(condition_statement_tokens)?;

				let init = parse(split[0], move_first)?;
				let post = parse(split[2], move_first)?;
				let body = parse(helper::remove_parentheses(code), move_first)?;

				LanguageElement::For {
					init,
					condition,
					post,
					body,
				}
			}

			//While
			[While, UnparsedBlock(cond), UnparsedBlock(code)] => {
				let condition = Token::parse_statement_tokens(UnparsedBlock(cond))?;
				let condition_parsed = StatementElement::from_tokens(condition)?;
				let body_parsed = parse(helper::remove_parentheses(code), move_first)?;
				LanguageElement::While {
					condition: condition_parsed,
					body: body_parsed,
				}
			}

			[Return] => LanguageElement::Return(None),

			[Return, ..] => {
				let return_value = StatementToken::from_tokens(&tokens[1..])?;
				let return_statement = StatementElement::from_tokens(return_value)?;
				LanguageElement::Return(Some(return_statement))
			}

			_ => {
				let tokenised = StatementToken::from_tokens(tokens)?;
				let element = StatementElement::from_tokens(tokenised)?;
				LanguageElement::Statement(element)
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

			LanguageElement::VariableDeclarationAssignment { typ, name, value } => {
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
				post,
				body,
			} => {
				if !condition.type_check(&variables, &functions)?
					|| !type_check(init, &variables, &functions)?
					|| !type_check(post, &variables, &functions)?
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

			LanguageElement::Return(_) => {
				//Is handled by function def instead
			}

			LanguageElement::Statement(statement) => {
				if !statement.type_check(&variables, &functions)? {
					return Ok(false);
				}
			}
		}
	}

	Ok(true)
}

fn split_token_lines<'a, 'b>(tokens: &'a [Token<'b>]) -> Vec<&'a [Token<'b>]> {
	let mut vec = Vec::new();
	let mut last = 0;
	for idx in 0..tokens.len() {
		if matches!(tokens[idx], Token::NewLine) {
			let slice = &tokens[last..idx];
			if !slice.is_empty() {
				vec.push(slice);
			}
			last = idx + 1;
		}
		if matches!(tokens[idx], Token::UnparsedBlock(raw) if helper::is_block(raw))
			&& !matches!(tokens.get(idx + 1), Some(Token::Else))
		{
			let slice = &tokens[last..=idx];
			if !slice.is_empty() {
				vec.push(slice);
			}
			last = idx + 1;
		}
	}
	//And add whatever remains
	let slice = &tokens[last..];
	if !slice.is_empty() {
		vec.push(slice);
	}
	vec
}

pub(crate) fn remove_comments(s: &str) -> String {
	let mut out = String::new();
	for line in s.lines() {
		let comment_start = line.find("//").unwrap_or_else(|| line.len());
		out.push_str(&line[..comment_start]);
	}
	out
}

use crate::*;

pub(crate) fn parse<'a>(
	source: &'a str,
	move_first: bool,
) -> Result<Vec<LanguageElement<'a>>, ParseError> {
	let tokens: Vec<Token<'a>> = Token::parse_str_to_vec(source)?;
	construct_block(&tokens, move_first)
}

///Wrapper around `construct_structure_from_tokens` to construct an entire block.
/// Does its own line split
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

///Matches a line of `Token`s into a `LanguageElement`, defaulting to a statement if no match can be made.
/// Does recursively parse all contained parts so a returned LanguageElement can be trusted as valid, apart
/// from type checking.
fn construct_structure_from_tokens<'a>(
	tokens: &[Token<'a>],
	move_first: bool,
) -> Result<LanguageElement<'a>, ParseError> {
	let element = {
		match tokens {
			//Function declaration
			[Decl(t), Token::Name(n), UnparsedParentheses(args), UnparsedBlock(code)] => {
				let args_parsed = Token::parse_argument_list_tokens(args)?;
				let code_tokenised = Token::parse_block_tokens(code)?;
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
				let mut typ = Type::Ptr(Box::new(t.clone()));
				let mut r = d.as_ref();
				let name;
				while let [Deref(b)] = r.as_ref() {
					typ = Type::Ptr(Box::new(typ));
					r = b;
				}
				if let [Token::Name(n)] = r {
					name = *n;
				} else {
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
					is_static: false,
				}
			}

			//Static pointer variable declaration
			[Static, Decl(t), Deref(d), Assign, ..] => {
				let mut typ = Type::Ptr(Box::new(t.clone()));
				let mut r = d.as_ref();
				let name;
				while let [Deref(b)] = r.as_ref() {
					typ = Type::Ptr(Box::new(typ));
					r = b;
				}
				if let [Token::Name(n)] = r {
					name = *n;
				} else {
					dbg!(tokens);
					return Err(ParseError(line!(), "Couldn't parse name/pointer type"));
				}
				let rhs = &tokens[4..];
				let rhs_verified = StatementToken::from_tokens(rhs)?;
				let rhs_parsed = StatementElement::from_tokens(rhs_verified)?;
				LanguageElement::VariableDeclarationAssignment {
					typ,
					name,
					value: rhs_parsed,
					is_static: true,
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
					is_static: false,
				}
			}

			//Static variable declaration
			[Static, Decl(t), Token::Name(n), Assign, ..] => {
				let rhs = &tokens[4..];
				let rhs_verified = StatementToken::from_tokens(rhs)?;
				let rhs_parsed = StatementElement::from_tokens(rhs_verified)?;
				LanguageElement::VariableDeclarationAssignment {
					typ: t.clone(),
					name: *n,
					value: rhs_parsed,
					is_static: true,
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
				is_static: false,
			},

			//Static variable declaration (without init)
			[Static, Decl(t), Token::Name(n)] => LanguageElement::VariableDeclaration {
				typ: t.clone(),
				name: *n,
				is_static: true,
			},

			//If else if
			[If, UnparsedParentheses(cond), UnparsedBlock(then_code), Else, If, ..] => {
				let condition = Token::parse_statement_tokens(cond)?;
				let condition_parsed = StatementElement::from_tokens(condition)?;
				let then_parsed = parse(then_code, move_first)?;
				let else_if_tokens = &tokens[4..];
				let else_if_parsed = construct_structure_from_tokens(else_if_tokens, move_first)?;
				LanguageElement::IfStatement {
					condition: condition_parsed,
					then: then_parsed,
					else_then: Some(vec![else_if_parsed]),
				}
			}

			//If else
			[If, UnparsedParentheses(cond), UnparsedBlock(then_code), Else, UnparsedBlock(else_code)] =>
			{
				let condition = Token::parse_statement_tokens(cond)?;
				let condition_parsed = StatementElement::from_tokens(condition)?;
				let then_parsed = parse(then_code, move_first)?;
				let else_parsed = parse(else_code, move_first)?;
				LanguageElement::IfStatement {
					condition: condition_parsed,
					then: then_parsed,
					else_then: Some(else_parsed),
				}
			}

			//If
			[If, UnparsedParentheses(cond), UnparsedBlock(code)] => {
				let condition = Token::parse_statement_tokens(cond)?;
				let condition_parsed = StatementElement::from_tokens(condition)?;
				let then_parsed = parse(code, move_first)?;
				LanguageElement::IfStatement {
					condition: condition_parsed,
					then: then_parsed,
					else_then: None,
				}
			}

			//For
			[For, UnparsedParentheses(init_cond_post), UnparsedBlock(code)] => {
				let split = init_cond_post.split(';').map(str::trim).collect::<Vec<_>>();
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
				let body = parse(code, move_first)?;

				LanguageElement::For {
					init,
					condition,
					post,
					body,
				}
			}

			//While
			[While, UnparsedParentheses(cond), UnparsedBlock(code)] => {
				let condition = Token::parse_statement_tokens(cond)?;
				let condition_parsed = StatementElement::from_tokens(condition)?;
				let body_parsed = parse(code, move_first)?;
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

			[TypeDef, Struct, Name(name), UnparsedBlock(members), Name(name2)] => {
				if name != name2 {
					return Err(ParseError(
						line!(),
						"Struct doesn't have the same name as its typedef",
					));
				}
				return Err(ParseError(line!(), "structs are not yet supported"));
			}
			[Struct, Name(name), UnparsedBlock(members)] => {
				return Err(ParseError(line!(), "structs are not yet supported"))
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

///Mostly broken type check. While this is technically correct, it relies on a very broken type check for statements
pub(crate) fn type_check(
	block: &[LanguageElement],
	upper_variables: &[Variable],
	outer_functions: &[Function],
) -> Result<bool, ParseError> {
	let mut variables = upper_variables.to_owned();
	let mut functions = outer_functions.to_owned();

	for line in block {
		match line {
			LanguageElement::VariableDeclaration {
				typ,
				name,
				is_static: _,
			} => variables.push(Variable {
				typ: typ.clone(),
				name: *name,
			}),

			LanguageElement::VariableAssignment { name: _, value } => {
				if !value.type_check(&variables, &functions)? {
					return Ok(false);
				}
			}

			LanguageElement::VariableDeclarationAssignment {
				typ,
				name,
				value,
				is_static: _,
			} => {
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

///Splits tokens at NewLines and after `UnparsedBlock`s that are *not* followed by `Else`
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
		if matches!(tokens[idx], Token::UnparsedBlock(_))
			&& !matches!(tokens.get(idx + 1), Some(Token::Else))
			&& !matches!(
				(tokens.get(idx + 1), tokens.get(idx + 2)),
				(Some(Token::Name(_)), Some(Token::NewLine))
			) {
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

///Takes the entire source code and removes the rest of the line for each line with a `//`.
/// Does *not* handle multiline comments
pub(crate) fn remove_comments(s: &str) -> String {
	let mut out = String::new();
	for line in s.lines() {
		let comment_start = line.find("//").unwrap_or_else(|| line.len());
		out.push_str(&line[..comment_start]);
	}
	out
}

use crate::*;
use std::borrow::Cow;

pub fn parse<'a>(
	source: &'a str,
	move_first: bool,
) -> Result<Vec<LanguageElement<'a>>, ParseError> {
	let tokens: Vec<Token<'a>> = Token::by_byte(source)?;
	construct_block(&tokens, move_first)
}

///Wrapper around `construct_structure_from_tokens` to construct an entire block.
/// Does its own line split
fn construct_block<'a>(
	tokens: &[Token<'a>],
	move_first: bool,
) -> Result<Vec<LanguageElement<'a>>, ParseError> {
	let mut res = split_token_lines(tokens)
		.into_iter()
		.map(|token| construct_structure_from_tokens(token, move_first))
		.collect::<Result<Vec<_>, _>>()?;
	if move_first {
		statement_element::move_declarations_first(&mut res);
	}
	Ok(res)
}

fn construct_structure_from_tokens<'a>(
	tokens: &[Token<'a>],
	move_first: bool,
) -> Result<LanguageElement<'a>, ParseError> {
	construct_structure_with_pointers_from_tokens(tokens, move_first)
		.unwrap_or_else(|| construct_structure_from_tokens_via_pattern(tokens, move_first))
}

///Matches a line of `Token`s into a `LanguageElement`, defaulting to a statement if no match can be made.
/// Does recursively parse all contained parts so a returned LanguageElement can be trusted as valid, apart
/// from type checking. (Struct types and fields count as type checking).
/// Struct and pointer patterns are not in this function.
fn construct_structure_from_tokens_via_pattern<'a>(
	tokens: &[Token<'a>],
	move_first: bool,
) -> Result<LanguageElement<'a>, ParseError> {
	//Todo patterns might be invalidated by the pointer versions
	let element = {
		match tokens {
			//Struct member assignment
			[Token::Name(n), FieldAccess, Token::Name(field), Assign, ..] => {
				let name = helper::merge_name_and_field(n, field);
				let rhs = StatementElement::from_tokens(&tokens[4..])?;
				LanguageElement::VariableAssignment { name, value: rhs }
			}

			//Struct member assignment through pointer
			[Token::Name(n), FieldPointerAccess, Token::Name(field), Assign, ..] => {
				let rhs = StatementElement::from_tokens(&tokens[4..])?;
				LanguageElement::StructFieldPointerAssignment {
					name: Cow::Borrowed(n),
					field: Cow::Borrowed(field),
					value: rhs,
				}
			}

			//Variable assignment
			[Token::Name(n), Assign, ..] => {
				let rhs = StatementElement::from_tokens(&tokens[2..])?;
				LanguageElement::VariableAssignment {
					name: Cow::Borrowed(n),
					value: rhs,
				}
			}

			//If else if
			[If, UnparsedParentheses(cond), UnparsedBlock(then_code), Else, If, ..] => {
				let condition_parsed = StatementElement::from_source_str(cond)?;
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
				let condition_parsed = StatementElement::from_source_str(cond)?;
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
				let condition_parsed = StatementElement::from_source_str(cond)?;
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

				//let condition_tokens = Token::parse_str_to_vec(split[1])?;
				let condition = StatementElement::from_source_str(split[1])?;

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
				let condition_parsed = StatementElement::from_source_str(cond)?;
				let body_parsed = parse(code, move_first)?;
				LanguageElement::While {
					condition: condition_parsed,
					body: body_parsed,
				}
			}

			[Return] => LanguageElement::Return(None),

			[Return, ..] => {
				let return_statement = StatementElement::from_tokens(&tokens[1..])?;
				LanguageElement::Return(Some(return_statement))
			}

			[TypeDef, Struct, Name(name), UnparsedBlock(members), Name(name2)] => {
				if name != name2 {
					return Err(ParseError(
						line!(),
						"Struct doesn't have the same name as its typedef",
					));
				}
				//Reuse second definition
				construct_structure_from_tokens(
					&[Struct, Name(name), UnparsedBlock(members)],
					move_first,
				)?
			}

			[Struct, Name(name), UnparsedBlock(members)] => {
				let members_parsed = Token::parse_struct_member_tokens(members)?;
				LanguageElement::StructDefinition {
					name: Cow::Borrowed(*name),
					members: members_parsed,
				}
			}

			_ if !tokens.contains(&Assign) => {
				let element = StatementElement::from_tokens(tokens)?;
				LanguageElement::Statement(element)
			}

			_ => {
				dbg!(tokens);
				return Err(ParseError(line!(), "Couldn't match language pattern"));
			}
		}
	};
	Ok(element)
}

//Non pointer types are also handled here ~~by accident~~ and are treated as 0-deep pointers. Whoops
///Tries to construct pointer variables, pointer structs or pointer assignments
fn construct_structure_with_pointers_from_tokens<'a>(
	tokens: &[Token<'a>],
	move_first: bool,
) -> Option<Result<LanguageElement<'a>, ParseError>> {
	if tokens.is_empty() {
		return None;
	}
	/*
	 *		type* name;
	 *		type* name =
	 *
	 *		*(expr) =
	 *
	 *		struct_name* name =
	 *		struct_name* name;
	 *		struct struct_name* name =
	 *		struct struct_name* name;
	 *
	 *		static type* name =
	 *		static type* name;
	 *		static struct struct_name* name =
	 *		static struct struct_name* name;
	 *		static struct_name* name =
	 *		static struct_name* name;
	 */
	match &tokens[0] {
		Static => construct_structure_with_pointers_from_tokens(&tokens[1..], move_first)
			.map(|res| res.and_then(LanguageElement::make_static)),
		//	`type* name` and `type* name` =
		Decl(t) => {
			let mut tokens_slice = &tokens[1..];
			let mut t = t.into();
			while let Some(Mul) = tokens_slice.get(0) {
				tokens_slice = &tokens_slice[1..];
				t = Type::ptr(t);
			}
			match tokens_slice {
				[Name(n)] => Some(Ok(LanguageElement::VariableDeclaration {
					typ: t,
					name: Cow::Borrowed(n),
					is_static: false,
				})),
				[Name(n), Assign, ..] => {
					let res = StatementElement::from_tokens(&tokens_slice[2..]).map(|statement| {
						LanguageElement::VariableDeclarationAssignment {
							typ: t,
							name: Cow::Borrowed(n),
							value: statement,
							is_static: false,
						}
					});
					Some(res)
				}
				[Name(n), UnparsedParentheses(args), UnparsedBlock(code)] => {
					let res = Token::parse_argument_list_tokens(args).and_then(|args_parsed| {
						Token::parse_block_tokens(code).and_then(|code_tokenised| {
							construct_block(&code_tokenised, move_first).map(|code_parsed| {
								LanguageElement::FunctionDeclaration {
									typ: t,
									name: Cow::Borrowed(n),
									args: args_parsed,
									block: code_parsed,
								}
							})
						})
					});
					Some(res)
				}
				_ => None,
			}
		}
		//Struct type name
		Name(n) => {
			let mut tokens_slice = &tokens[1..];
			let mut t = Type::Struct(n);
			while let Some(Mul) = tokens_slice.get(0) {
				tokens_slice = &tokens_slice[1..];
				t = Type::ptr(t);
			}
			match tokens_slice {
				[Name(n)] => Some(Ok(LanguageElement::VariableDeclaration {
					typ: t,
					name: Cow::Borrowed(n),
					is_static: false,
				})),
				[Name(n), Assign, UnparsedBlock(s)] => {
					let res = Token::parse_arguments_tokens(s).and_then(|tokens| {
						tokens
							.into_iter()
							.map(StatementElement::from_statement_tokens)
							.collect::<Result<Vec<_>, _>>()
							.map(|members| LanguageElement::StructDeclarationAssignment {
								typ: t,
								name: Cow::Borrowed(n),
								value: members,
								is_static: false,
							})
					});
					Some(res)
				}
				[Name(n), Assign, ..] => {
					let res = StatementElement::from_tokens(&tokens_slice[2..]).map(|rhs| {
						LanguageElement::VariableDeclarationAssignment {
							typ: t,
							name: Cow::Borrowed(n),
							value: rhs,
							is_static: false,
						}
					});
					Some(res)
				}
				[Name(n), UnparsedParentheses(args), UnparsedBlock(code)] => {
					let res = Token::parse_argument_list_tokens(args).and_then(|args_parsed| {
						Token::parse_block_tokens(code).and_then(|code_tokenised| {
							construct_block(&code_tokenised, move_first).map(|code_parsed| {
								LanguageElement::FunctionDeclaration {
									typ: t,
									name: Cow::Borrowed(n),
									args: args_parsed,
									block: code_parsed,
								}
							})
						})
					});
					Some(res)
				}
				_ => None,
			}
		}
		Mul => {
			let assign_idx = tokens.iter().position(|t| t == &Assign);
			assign_idx.map(|assign_idx| {
				//Skip the first deref as the LanaguageElement::PointerAssignment::ptr already expects a pointer
				let lhs = StatementElement::from_tokens(&tokens[1..assign_idx]);
				let rhs = StatementElement::from_tokens(&tokens[assign_idx + 1..]);
				lhs.and_then(|lhs| {
					rhs.map(|rhs| LanguageElement::PointerAssignment {
						ptr: lhs,
						value: rhs,
					})
				})
			})
		}
		Struct => {
			if !matches!(tokens.get(1), Some(Name(_))) {
				return None;
			}
			construct_structure_with_pointers_from_tokens(&tokens[1..], move_first)
		}
		_ => None,
	}
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

//Rewrite to return a `Cow<str>` instead?
///Takes the entire source code and removes the rest of the line for each line with a `//`.
/// Does *not* handle multiline comments
pub fn remove_comments(s: &str) -> String {
	let mut out = String::new();
	for line in s.lines() {
		let comment_start = line.find("//").unwrap_or_else(|| line.len());
		out.push_str(&line[..comment_start]);
	}
	out
}

pub fn remove_multiline_comments(s: &str) -> Result<Cow<str>, ParseError> {
	const OPEN: &str = "/*";
	const CLOSE: &str = "*/";
	let next = s.find(OPEN);
	if next.is_none() {
		return Ok(Cow::Borrowed(s));
	}
	let next = next.unwrap();
	let after = s[(next + 2)..].find(OPEN);
	let close = s[(next + 2)..].find(CLOSE);
	if close.is_none() {
		return Ok(Cow::Borrowed(s));
	}
	let close = close.unwrap();
	if let Some(after) = after {
		if after < close {
			let new_s = remove_multiline_comments(&s[(next + 2)..])?;
			let new_close = (&new_s).find(CLOSE).expect("");
		}
	}

	todo!()
}

use std::borrow::Cow;

use crate::*;

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
	let element = {
		match tokens {
			//Struct member assignment
			[Token::Name(n), Token::FieldAccess, Token::Name(field), Token::Assign, ..] => {
				let name = helper::merge_name_and_field(n, field);
				let rhs = StatementElement::from_tokens(&tokens[4..])?;
				LanguageElement::VariableAssignment { name, value: rhs }
			}

			//Struct member assignment through pointer
			[Token::Name(n), Token::FieldPointerAccess, Token::Name(field), Token::Assign, ..] => {
				let rhs = StatementElement::from_tokens(&tokens[4..])?;
				LanguageElement::StructFieldPointerAssignment {
					name: Cow::Borrowed(n),
					field: Cow::Borrowed(field),
					value: rhs,
				}
			}

			//Variable assignment
			[Token::Name(n), Token::Assign, ..] => {
				let rhs = StatementElement::from_tokens(&tokens[2..])?;
				LanguageElement::VariableAssignment {
					name: Cow::Borrowed(n),
					value: rhs,
				}
			}

			//Array assignment
			[Token::Name(n), Token::UnparsedArrayAccess(idx), Token::Assign, ..] => {
				let lhs = StatementElement::from_source_str(idx)?;
				let rhs = StatementElement::from_tokens(&tokens[3..])?;
				LanguageElement::PointerAssignment {
					ptr: StatementElement::Add {
						lhs: Box::new(lhs),
						rhs: Box::new(StatementElement::Var(Cow::Borrowed(n))),
					},
					value: rhs,
				}
			}

			//Switch statement
			[Token::Switch, Token::UnparsedParentheses(expr), Token::UnparsedBlock(cases)] => {
				let _expr = StatementElement::from_source_str(expr)?;
				let _cases_parsed = parse(cases, move_first)?;
				return Err(ParseError::SwitchStatement(line!()));
			}

			//Case
			[Token::Case, constant, Token::Colon, ..]
				if matches!(constant, Token::Bool(_) | Token::Char(_) | Token::Num(_)) =>
			{
				return Err(ParseError::SwitchStatement(line!()));
			}

			[Token::Break, ..] | [Token::Continue, ..] => {
				return Err(ParseError::BreakContinue(line!()));
			}

			//If else if
			[Token::If, Token::UnparsedParentheses(cond), Token::UnparsedBlock(then_code), Token::Else, Token::If, ..] =>
			{
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
			[Token::If, Token::UnparsedParentheses(cond), Token::UnparsedBlock(then_code), Token::Else, Token::UnparsedBlock(else_code)] =>
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
			[Token::If, Token::UnparsedParentheses(cond), Token::UnparsedBlock(code)] => {
				let condition_parsed = StatementElement::from_source_str(cond)?;
				let then_parsed = parse(code, move_first)?;
				LanguageElement::IfStatement {
					condition: condition_parsed,
					then: then_parsed,
					else_then: None,
				}
			}

			//For
			[Token::For, Token::UnparsedParentheses(init_cond_post), Token::UnparsedBlock(code)] => {
				let split = init_cond_post.split(';').map(str::trim).collect::<Vec<_>>();
				if split.len() != 3 {
					return Err(ParseError::BrokenForLoop(line!()));
				}

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
			[Token::While, Token::UnparsedParentheses(cond), Token::UnparsedBlock(code)] => {
				let condition_parsed = StatementElement::from_source_str(cond)?;
				let body_parsed = parse(code, move_first)?;
				LanguageElement::While {
					condition: condition_parsed,
					body: body_parsed,
				}
			}

			[Token::Return] => LanguageElement::Return(None),

			[Token::Return, ..] => {
				let return_statement = StatementElement::from_tokens(&tokens[1..])?;
				LanguageElement::Return(Some(return_statement))
			}

			[Token::TypeDef, Token::Struct, Token::Name(name), Token::UnparsedBlock(members), Token::Name(name2)] =>
			{
				if name != name2 {
					return Err(ParseError::BadStructName(line!()));
				}
				//Reuse second definition
				construct_structure_from_tokens(
					&[
						Token::Struct,
						Token::Name(name),
						Token::UnparsedBlock(members),
					],
					move_first,
				)?
			}

			[Token::Struct, Token::Name(name), Token::UnparsedBlock(members)] => {
				let members_parsed = Token::parse_struct_member_tokens(members)?;
				LanguageElement::StructDefinition {
					name: Cow::Borrowed(name),
					members: members_parsed,
				}
			}

			_ if !tokens.contains(&Token::Assign) => {
				let element = StatementElement::from_tokens(tokens)?;
				LanguageElement::Statement(element)
			}

			_ => {
				dbg!(tokens);
				return Err(ParseError::MatchFail(line!()));
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
		Token::Static => construct_structure_with_pointers_from_tokens(&tokens[1..], move_first)
			.map(|res| res.and_then(LanguageElement::make_static)),
		Token::Const => construct_structure_with_pointers_from_tokens(&tokens[1..], move_first)
			.map(|res| res.and_then(LanguageElement::make_const)),
		Token::Volatile => construct_structure_with_pointers_from_tokens(&tokens[1..], move_first)
			.map(|res| res.and_then(LanguageElement::make_volatile)),
		//	`type* name` and `type* name` =
		Token::Decl(t) => {
			let mut tokens_slice = &tokens[1..];
			let mut t = t.into();
			while let Some(Token::Mul) = tokens_slice.get(0) {
				tokens_slice = &tokens_slice[1..];
				t = Type::ptr(t);
			}
			match tokens_slice {
				[Token::Name(n)] => Some(Ok(LanguageElement::VariableDeclaration {
					typ: t,
					name: Cow::Borrowed(n),
					is_static: false,
					is_const: false,
					is_volatile: false,
				})),
				[Token::Name(n), Token::UnparsedArrayAccess(len)] => {
					let res = len
						.parse::<isize>()
						.map_err(|_| ParseError::NonConstantArrayLen(line!()))
						.map(|len| {
							t = Type::Arr(Box::new(t), len);
							LanguageElement::VariableDeclaration {
								typ: t,
								name: Cow::Borrowed(n),
								is_static: false,
								is_const: false,
								is_volatile: false,
							}
						});
					Some(res)
				}
				[Token::Name(n), Token::Assign, ..] => {
					let res = StatementElement::from_tokens(&tokens_slice[2..]).map(|statement| {
						LanguageElement::VariableDeclarationAssignment {
							typ: t,
							name: Cow::Borrowed(n),
							value: statement,
							is_static: false,
							is_const: false,
							is_volatile: false,
						}
					});
					Some(res)
				}
				[Token::Name(n), Token::UnparsedArrayAccess(len), Token::Assign, ..] => {
					let res = len
						.parse::<isize>()
						.map_err(|_| ParseError::NonConstantArrayLen(line!()))
						.and_then(|len| {
							t = Type::Arr(Box::new(t), len);
							StatementElement::from_tokens(&tokens_slice[3..]).map(|statement| {
								LanguageElement::VariableDeclarationAssignment {
									typ: t,
									name: Cow::Borrowed(n),
									value: statement,
									is_static: false,
									is_const: false,
									is_volatile: false,
								}
							})
						});
					Some(res)
				}
				//Function
				[Token::Name(n), Token::UnparsedParentheses(args), Token::UnparsedBlock(code)] => {
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
		Token::Name(n) => {
			let mut tokens_slice = &tokens[1..];
			let mut t = Type::Struct(n);
			while let Some(Token::Mul) = tokens_slice.get(0) {
				tokens_slice = &tokens_slice[1..];
				t = Type::ptr(t);
			}
			match tokens_slice {
				[Token::Name(n)] => Some(Ok(LanguageElement::VariableDeclaration {
					typ: t,
					name: Cow::Borrowed(n),
					is_static: false,
					is_const: false,
					is_volatile: false,
				})),
				[Token::Name(n), Token::Assign, Token::UnparsedBlock(s)] => {
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
								is_const: false,
								is_volatile: false,
							})
					});
					Some(res)
				}
				[Token::Name(n), Token::Assign, ..] => {
					let res = StatementElement::from_tokens(&tokens_slice[2..]).map(|rhs| {
						LanguageElement::VariableDeclarationAssignment {
							typ: t,
							name: Cow::Borrowed(n),
							value: rhs,
							is_static: false,
							is_const: false,
							is_volatile: false,
						}
					});
					Some(res)
				}
				//Function
				[Token::Name(n), Token::UnparsedParentheses(args), Token::UnparsedBlock(code)] => {
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
		Token::Mul => {
			let assign_idx = tokens.iter().position(|t| t == &Token::Assign);
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
		Token::Struct => {
			if !matches!(tokens.get(1), Some(Token::Name(_))) {
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

///Removes all comments from the source code.
/// Recursive multiline comments are treated properly.
pub fn remove_comments(s: &str) -> String {
	let cow_str = Cow::Borrowed(s);
	let no_multilines = remove_multiline_comments(cow_str);
	let no_single_lines = remove_single_line_comments(no_multilines);
	match no_single_lines {
		Cow::Owned(s) => s,
		Cow::Borrowed(s) => s.to_owned(),
	}
}

///Takes the entire source code and removes the rest of the line for each line with a `//`.
fn remove_single_line_comments(mut s: Cow<str>) -> Cow<str> {
	//Could be sped up if we didn't start the search over each time,
	// but apparently I can't write code that doesn't crash
	while let Some(idx) = s.find("//") {
		let end = s[idx..]
			.find('\n')
			.map(|v| v + idx)
			.unwrap_or_else(|| s.len());
		match &mut s {
			Cow::Owned(slice) => slice.replace_range(idx..end, ""),
			Cow::Borrowed(slice) => s = Cow::Owned(slice[..idx].to_string() + &slice[end..]),
		}
	}
	s
}

///Takes the entire source code and removes everything between `/*` and `*/`.
/// Works with recursive comments.
/// Should probably be run before the single line version?
fn remove_multiline_comments(mut s: Cow<str>) -> Cow<str> {
	const OPEN: &str = "/*";
	const CLOSE: &str = "*/";
	loop {
		//Find the first comment ending and find the last start before it,
		// remove everything between them, repeat till there are no comments.
		if s.is_empty() {
			return s;
		}
		let first_end = s.find(CLOSE);
		//No end: return early, even if it might be unbalanced
		if first_end.is_none() {
			return s;
		}
		let first_end = first_end.unwrap();
		let mut latest_start = 0;
		let mut maybe_latest_start = s[latest_start..first_end].find(OPEN);
		//No start: return as it is, even though we know it's unbalanced
		if maybe_latest_start.is_none() {
			return s;
		}
		while let Some(start) = maybe_latest_start {
			latest_start += start + 2;
			maybe_latest_start = s[latest_start..first_end].find(OPEN);
		}
		if let Cow::Owned(s) = &mut s {
			//Replace with `remove_range` is something like that ever gets stabilised?
			let from = latest_start - 2;
			let to = first_end + 2;
			s.replace_range(from..to, "");
		} else {
			s = Cow::Owned(s[..latest_start - 2].to_string() + &s[first_end + 2..]);
		}
	}
}

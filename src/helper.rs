use crate::*;

///Splits a string slice into words. Will keep anything in parentheses as a single
/// word so you will have to recall this function after removing parentheses.
/// `(){}[]""` count as parentheses, not just `()`
pub(crate) fn split(s: &str) -> Result<Vec<&str>, ParseError> {
	let keep_closure = |slice: &str| slice.chars().any(|c| !c.is_whitespace());
	let mut vec = Vec::new();
	let mut parentheses = 0;
	let mut brackets = 0;
	let mut curlies = 0;
	let mut start = 0;
	let mut quotes = 0;
	let mut escape = false;
	for (i, c) in s.char_indices() {
		match (curlies, brackets, parentheses, quotes, c) {
			(0, 0, 0, 0, '{') => {
				let slice = &s[start..i];
				if keep_closure(slice) {
					vec.push(slice);
				}
				start = i;
				curlies += 1;
			}
			(_, 0, 0, 0, '{') => {
				curlies += 1;
			}

			(1, 0, 0, 0, '}') => {
				let slice = &s[start..=i];
				if keep_closure(slice) {
					vec.push(slice);
				}
				start = i + 1;
				curlies -= 1;
			}
			(_, 0, 0, 0, '}') => {
				curlies -= 1;
			}

			(0, 0, 0, 0, '[') => {
				let slice = &s[start..i];
				if keep_closure(slice) {
					vec.push(slice);
				}
				start = i;
				brackets += 1;
			}
			(0, _, 0, 0, '[') => {
				brackets += 1;
			}
			(0, 1, 0, 0, ']') => {
				let slice = &s[start..=i];
				if keep_closure(slice) {
					vec.push(slice);
				}
				start = i + 1;
				brackets -= 1;
			}
			(0, _, 0, 0, ']') => {
				brackets -= 1;
			}

			(0, 0, 0, 0, '(') => {
				let slice = &s[start..i];
				if keep_closure(slice) {
					vec.push(slice);
				}
				start = i;
				parentheses += 1;
			}
			(0, 0, _, 0, '(') => {
				parentheses += 1;
			}
			(0, 0, 1, 0, ')') => {
				let slice = &s[start..=i];
				if keep_closure(slice) {
					vec.push(slice);
				}
				start = i + 1;
				parentheses -= 1;
			}
			(0, 0, _, 0, ')') => {
				parentheses -= 1;
			}

			(0, 0, 0, 0, '"') if !escape => {
				let slice = &s[start..i];
				if keep_closure(slice) {
					vec.push(slice);
				}
				start = i;
				quotes += 1;
			}
			(0, 0, 0, 1, '"') if !escape => {
				let slice = &s[start..=i];
				if keep_closure(slice) {
					vec.push(slice);
				}
				start = i + 1;
				quotes -= 1;
			}

			(0, 0, 0, 0, _) if c.is_whitespace() => {
				let slice = &s[start..i];
				if keep_closure(slice) {
					vec.push(slice);
				}
				start = i + 1;
			}
			(0, 0, 0, 0, ';') => {
				let slice = &s[start..i];
				if keep_closure(slice) {
					vec.push(slice);
				}
				vec.push(&s[i..i + 1]);
				start = i + 1;
			}

			(0, 0, 0, 1, '\\') => {
				escape = true;
				continue;
			}

			(0, 0, 0, 0, c) if "+-*/^.".contains(c) => {
				let slice = &s[start..i];
				if keep_closure(slice) {
					vec.push(slice);
				}
				vec.push(&s[i..i + 1]);
				start = i + 1;
			}

			_ => {}
		}
		escape = false;
	}
	let slice = &s[start..];
	if keep_closure(slice) {
		vec.push(slice);
	}

	if parentheses == 0 && brackets == 0 && quotes == 0 {
		Ok(vec)
	} else {
		dbg!(s);
		Err(ParseError(
			line!(),
			"Couldn't split string into tokens due to uneven parentheses/brackets/quotes",
		))
	}
}

///Removes the first and last characters. Panics if the string is too short
pub(crate) fn remove_parentheses(s: &str) -> &str {
	assert!(s.len() >= 2);
	s[1..s.len() - 1].trim()
}

fn get_number(s: &str) -> Option<(Token, &str)> {
	if s.starts_with("0x") && s.as_bytes().get(2).map(|d| d.is_ascii_hexdigit()) == Some(true) {
		let mut len = 0;
		let num = s
			.bytes()
			.skip(2)
			.take_while(|d| d.is_ascii_hexdigit())
			.fold(0, |acc, curr| {
				len += 1;
				acc * 16 + (curr - b'0') as isize
			});
		Some((Token::Num(num), &s[len..]))
	} else if s.starts_with('-') && s.as_bytes().get(1).map(|d| d.is_ascii_digit()) == Some(true) {
		let mut len = 0;
		let num = s
			.bytes()
			.skip(1)
			.take_while(|d| d.is_ascii_digit())
			.fold(0, |acc, curr| {
				len += 1;
				acc * 10 + (curr - b'0') as isize
			});
		Some((Token::Num(-num), &s[len..]))
	} else if s.as_bytes().get(0).map(|d| d.is_ascii_digit()) == Some(true) {
		let mut len = 0;
		let num = s
			.bytes()
			.take_while(|d| d.is_ascii_digit())
			.fold(0, |acc, curr| {
				len += 1;
				acc * 10 + (curr - b'0') as isize
			});
		Some((Token::Num(num), &s[len..]))
	} else {
		None
	}
}

fn get_parenthesis(s: &str) -> Option<(Token, &str)> {
	if !s.starts_with('(') {
		return None;
	}
	let mut parentheses = 0;
	let mut len = 0;
	for c in s.chars() {
		len += 1;
		match c {
			'(' => parentheses += 1,
			')' => parentheses -= 1,
			_ => {}
		}
		if parentheses == 0 {
			break;
		}
	}
	let token = Token::UnparsedParentheses(&s[1..len - 1]);
	let rest = &s[len..];
	Some((token, rest))
}

fn get_block(s: &str) -> Option<(Token, &str)> {
	if !s.starts_with('{') {
		return None;
	}
	let mut parentheses = 0;
	let mut len = 0;
	for c in s.chars() {
		len += 1;
		match c {
			'{' => parentheses += 1,
			'}' => parentheses -= 1,
			_ => {}
		}
		if parentheses == 0 {
			break;
		}
	}
	let token = Token::UnparsedBlock(&s[1..len - 1]);
	let rest = &s[len..];
	Some((token, rest))
}

fn get_array_access(s: &str) -> Option<(Token, &str)> {
	if !s.starts_with('[') {
		return None;
	}
	let mut parentheses = 0;
	let mut len = 0;
	for c in s.chars() {
		len += 1;
		match c {
			'[' => parentheses += 1,
			']' => parentheses -= 1,
			_ => {}
		}
		if parentheses == 0 {
			break;
		}
	}
	let token = Token::UnparsedArrayAccess(&s[1..len - 1]);
	let rest = &s[len..];
	Some((token, rest))
}

fn get_name(s: &str) -> Option<(Token, &str)> {
	let ws = s
		.chars()
		.take_while(|d| !d.is_whitespace() && !FORBIDDEN_CHARACTERS.contains(d))
		.count();
	let token = Token::Name(&s[..ws]);
	let rest = &s[ws..];
	Some((token, rest))
}

fn get_single_token_match(s: &str) -> Option<(Token, &str)> {
	PATTERNS
		.iter()
		.find(|(pat, whitespace, _)| {
			s.starts_with(pat)
				&& if *whitespace {
					s.as_bytes().get(pat.len()).map(u8::is_ascii_whitespace) != Some(false)
				} else {
					true
				}
		})
		.map(|(pat, _, t)| (t(), s[pat.len()..].trim()))
}

pub(crate) fn get_token(s: &str) -> Result<(Token, &str), ParseError> {
	None.or_else(|| get_single_token_match(s))
		.or_else(|| get_number(s))
		.or_else(|| get_parenthesis(s))
		.or_else(|| get_block(s))
		.or_else(|| get_array_access(s))
		.or_else(|| get_name(s))
		.ok_or(ParseError(line!(), "Couldn't parse token"))
}

const FORBIDDEN_CHARACTERS: &[char] = &[
	'+', '-', '/', '*', '.', ',', '!', '~', '<', '>', '&', '|', '\\', '\'', '"', '(', ')', '[',
	']', '{', '}', '`', '´', '?', '=', '@', '£', '#', '$', '¤', '%', '¨', '§', ';', ':',
];

const PATTERNS: [(&str, bool, fn() -> Token<'static>); 48] = [
	("true", true, || Bool(true)),
	("false", true, || Bool(false)),
	("return", true, || Return),
	("for", false, || For),
	("while", false, || While),
	("break", true, || Break),
	("continue", true, || Continue),
	("switch", false, || Switch),
	("static", true, || Static),
	("struct", true, || Struct),
	("typedef", true, || TypeDef),
	(";", false, || NewLine),
	("int*", true, || Decl(Type::Ptr(Box::new(Type::Int)))),
	("bool*", true, || Decl(Type::Ptr(Box::new(Type::Bool)))),
	("char*", true, || Decl(Type::Ptr(Box::new(Type::Char)))),
	("uint*", true, || Decl(Type::Ptr(Box::new(Type::Uint)))),
	("void*", true, || Decl(Type::Ptr(Box::new(Type::Void)))),
	("int", true, || Decl(Type::Int)),
	("bool", true, || Decl(Type::Bool)),
	("char", true, || Decl(Type::Char)),
	("uint", true, || Decl(Type::Uint)),
	("void", true, || Decl(Type::Void)),
	("if", false, || If),
	("else", false, || Else),
	("==", false, || Cmp),
	("!=", false, || NotCmp),
	(">=", false, || GreaterThanEqual),
	("<=", false, || LessThanEqual),
	("->", false, || FieldPointerAccess),
	("<<", false, || LShift),
	(">>", false, || RShift),
	("&&", false, || BoolAnd),
	("||", false, || BoolOr),
	("=", false, || Assign),
	("+", false, || Add),
	("-", false, || Sub),
	("*", false, || Mul),
	("/", false, || Div),
	("%", false, || Mod),
	(">", false, || GreaterThan),
	("<", false, || LessThan),
	("&", false, || BitAnd),
	("|", false, || BitOr),
	("^", false, || Xor),
	("!", false, || BoolNot),
	("~", false, || BitNot),
	(".", false, || FieldAccess),
	(",", false, || Comma),
];

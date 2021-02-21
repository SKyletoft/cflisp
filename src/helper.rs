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

///A block starts with `{` and ends with `}`
pub(crate) fn is_block(s: &str) -> bool {
	s.starts_with('{') && s.ends_with('}')
}

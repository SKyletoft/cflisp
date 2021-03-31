use crate::*;

///Removes the first and last characters. Panics if the string is too short
pub(crate) fn remove_parentheses(s: &str) -> &str {
	assert!(s.len() >= 2);
	s[1..s.len() - 1].trim()
}

fn get_hex_number(s: &str) -> Option<(Token, &str)> {
	if s.starts_with("0x") && s.as_bytes().get(2).map(|d| d.is_ascii_hexdigit()) == Some(true) {
		let mut len = 2;
		let num = s
			.bytes()
			.skip(2)
			.take_while(|d| d.is_ascii_hexdigit())
			.fold(0, |acc, curr| {
				len += 1;
				acc * 16 + (curr - b'0') as isize
			});
		Some((Token::Num(num), &s[len..]))
	} else {
		None
	}
}

fn get_negative_number(s: &str) -> Option<(Token, &str)> {
	if s.starts_with('-') && s.as_bytes().get(1).map(|d| d.is_ascii_digit()) == Some(true) {
		let mut len = 1;
		let num = s
			.bytes()
			.skip(1)
			.take_while(|d| d.is_ascii_digit())
			.fold(0, |acc, curr| {
				len += 1;
				acc * 10 + (curr - b'0') as isize
			});
		Some((Token::Num(-num), &s[len..]))
	} else {
		None
	}
}

fn get_positive_number(s: &str) -> Option<(Token, &str)> {
	if s.as_bytes().get(0).map(|d| d.is_ascii_digit()) == Some(true) {
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

fn get_number(s: &str) -> Option<(Token, &str)> {
	None.or_else(|| get_hex_number(s))
		.or_else(|| get_negative_number(s))
		.or_else(|| get_positive_number(s))
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

fn get_char(s: &str) -> Option<(Token, &str)> {
	if s.len() < 3 {
		return None;
	}
	if let Some(&[b'\'', c, b'\'']) = s.as_bytes().get(0..3) {
		return Some((Token::Char(c as char), &s[3..]));
	}
	None
}

///Names must start with an alphabetic character, continues till
/// it hits a forbidden character or whitespace.
/// ('A'..='Z'), ('a'..='z')
fn get_name(s: &str) -> Option<(Token, &str)> {
	if s.starts_with(|c: char| !c.is_ascii_alphabetic()) {
		return None;
	}
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
				&& (!*whitespace //Trust the shortcircuit
					|| s.as_bytes().get(pat.len()).map(u8::is_ascii_whitespace) != Some(false))
		})
		.map(|(pat, _, t)| (t(), s[pat.len()..].trim()))
}

pub(crate) fn get_token(s: &str) -> Result<(Token, &str), ParseError> {
	None.or_else(|| get_single_token_match(s))
		.or_else(|| get_number(s))
		.or_else(|| get_parenthesis(s))
		.or_else(|| get_block(s))
		.or_else(|| get_array_access(s))
		.or_else(|| get_char(s))
		.or_else(|| get_name(s))
		.ok_or(ParseError(line!(), "Couldn't parse token"))
}

const FORBIDDEN_CHARACTERS: &[char] = &[
	'+', '-', '/', '*', '.', ',', '!', '~', '<', '>', '&', '|', '\\', '\'', '"', '(', ')', '[',
	']', '{', '}', '`', '´', '?', '=', '@', '£', '#', '$', '¤', '%', '¨', '§', ';', ':',
];

//Literal, needs following whitespace, closure to return token
//Closure needed instead of values because pointer types are boxed
type TokenFunction = fn() -> Token<'static>;
const PATTERNS: [(&str, bool, TokenFunction); 84] = [
	("_Alignas", true, || AlignAs),
	("_Alignof", true, || AlignOf),
	("_Atomic", true, || Atomic),
	("_Bool", true, || Decl(NativeType::Bool)),
	("_Complex", true, || Complex),
	("_Generic", true, || Generic),
	("_Imaginary", true, || Imaginary),
	("_Noreturn", true, || NoReturn),
	("_Static_assert", true, || StaticAssert),
	("_Thread_local", true, || ThreadLocal),
	("auto", true, || Auto),
	("break", true, || Break),
	("case", true, || Case),
	("complex", true, || Complex),
	("const", true, || Const),
	("continue", true, || Continue),
	("default", false, || Default),
	("do", false, || Do),
	("double", true, || Double),
	("else", false, || Else),
	("enum", true, || Enum),
	("extern", true, || Extern),
	("false", true, || Bool(false)),
	("float", true, || Float),
	("for", false, || For),
	("generic", true, || Generic),
	("goto", true, || Goto),
	("if", false, || If),
	("imaginary", true, || Imaginary),
	("inline", true, || Inline),
	("long", true, || Long),
	("noreturn", true, || NoReturn),
	("register", true, || Register),
	("restrict", true, || Restrict),
	("return", true, || Return),
	("short", true, || Short),
	("signed", true, || Signed),
	("sizeof", true, || SizeOf),
	("static_assert", true, || StaticAssert),
	("static", true, || Static),
	("struct", true, || Struct),
	("switch", false, || Switch),
	("true", true, || Bool(true)),
	("typedef", true, || TypeDef),
	("union", true, || Union),
	("unsigned", true, || Unsigned),
	("volatile", true, || Volatile),
	("while", false, || While),
	("int*", true, || Decl(NativeType::ptr(NativeType::Int))),
	("bool*", true, || Decl(NativeType::ptr(NativeType::Bool))),
	("char*", true, || Decl(NativeType::ptr(NativeType::Char))),
	("uint*", true, || Decl(NativeType::ptr(NativeType::Uint))),
	("void*", true, || Decl(NativeType::ptr(NativeType::Void))),
	("int", true, || Decl(NativeType::Int)),
	("bool", true, || Decl(NativeType::Bool)),
	("char", true, || Decl(NativeType::Char)),
	("uint", true, || Decl(NativeType::Uint)),
	("void", true, || Decl(NativeType::Void)),
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
	(";", false, || NewLine),
	(":", false, || Colon),
];

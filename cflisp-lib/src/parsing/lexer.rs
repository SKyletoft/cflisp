use crate::*;

//Correctness says return a Result. Ergonomics say trust the user to read the docs
///Returns 0 on invalid
fn hex_digit_value(digit: u8) -> isize {
	match digit {
		b'0'..=b'9' => (digit - b'0') as isize,
		b'A'..=b'F' => (digit - b'A' + 10) as isize,
		b'a'..=b'f' => (digit - b'a' + 10) as isize,
		_ => 0,
	}
}

///Gets a hex number from the source and returns the remaining source and a number token. Unsigned only
fn get_hex_number(s: &str) -> Option<(Token, &str)> {
	if (s.starts_with("0x") || s.starts_with("0X"))
		&& s.as_bytes()
			.get(2)
			.map(|d| d.is_ascii_hexdigit())
			.unwrap_or(false)
	{
		let (len, num) = s
			.bytes()
			.enumerate()
			.skip(2)
			.take_while(|(_, d)| d.is_ascii_hexdigit())
			.fold((0, 0), |(_, acc), (len, curr)| {
				(len, acc * 16 + hex_digit_value(curr))
			});
		Some((
			Token::Num(Number::new(num, NumberType::Unsigned)),
			&s[(len + 1)..],
		))
	} else {
		None
	}
}

//Get rid of and rely on optimisation and negate operation?
///Gets a hex number from the source and returns the remaining source and a number token. Negative only
fn get_negative_number(s: &str) -> Option<(Token, &str)> {
	if s.starts_with('-')
		&& s.as_bytes()
			.get(1)
			.map(|d| d.is_ascii_digit())
			.unwrap_or(false)
	{
		let mut len = 1;
		let num = s
			.bytes()
			.skip(1)
			.take_while(|d| d.is_ascii_digit())
			.fold(0, |acc, curr| {
				len += 1;
				acc * 10 + (curr - b'0') as isize
			});
		Some((Token::Num(Number::new(-num, NumberType::Signed)), &s[len..]))
	} else {
		None
	}
}

///Gets a hex number from the source and returns the remaining source and a number token. Unsigned only
fn get_positive_number(s: &str) -> Option<(Token, &str)> {
	if s.as_bytes()
		.get(0)
		.map(|d| d.is_ascii_digit())
		.unwrap_or(false)
	{
		let mut len = 0;
		let num = s
			.bytes()
			.take_while(|d| d.is_ascii_digit())
			.fold(0, |acc, curr| {
				len += 1;
				acc * 10 + (curr - b'0') as isize
			});
		Some((Token::Num(num.into()), &s[len..]))
	} else {
		None
	}
}

///Wrapper around get_hex_number, get_negative_number and get_positive_number
fn get_number(s: &str) -> Option<(Token, &str)> {
	None.or_else(|| get_hex_number(s))
		.or_else(|| get_negative_number(s))
		.or_else(|| get_positive_number(s))
}

fn get_prio_section(start: char, end: char, s: &str) -> Option<(&str, &str)> {
	if !s.starts_with(start) {
		return None;
	}
	let mut parentheses = 1;
	let mut len = 1;
	for c in s.chars().skip(1) {
		len += 1;
		//Check end first because start might be equal to end (in the case of strings for instance)
		//Todo: Handle nested strings
		if c == end {
			parentheses -= 1;
		} else if c == start {
			parentheses += 1;
		}
		if parentheses == 0 {
			break;
		}
	}
	if parentheses != 0 {
		return None;
	}
	let token = &s[1..len - 1];
	let rest = &s[len..];
	Some((token, rest))
}

fn get_string_literal(s: &str) -> Option<(Token, &str)> {
	get_prio_section('"', '"', s).map(|(a, b)| (Token::StringLiteral(a), b))
}

///Gets a complete statement in parenthesis. Returns None on unmatched parentheses.
fn get_parenthesis(s: &str) -> Option<(Token, &str)> {
	let (paren, tail) = get_prio_section('(', ')', s)?;
	let tokenised = Token::by_byte(paren).ok()?;
	Some((Token::Parentheses(tokenised), tail))
}

///Gets a complete statement in curly brackets. Returns None on unmatched brackets.
/// The brackets are not in the string in the UnparsedBlock token
fn get_block(s: &str) -> Option<(Token, &str)> {
	let (paren, tail) = get_prio_section('{', '}', s)?;
	let tokenised = Token::by_byte(paren).ok()?;
	Some((Token::Block(tokenised), tail))
}

///Gets the middle part of a ternary statement to later be used as a binary operator
/// if the expression lack a colon None will be returned.
/// `?` and `:` are not included in the string
fn get_ternary_op(s: &str) -> Option<(Token, &str)> {
	let (paren, tail) = get_prio_section('?', ':', s)?;
	let tokenised = Token::by_byte(paren).ok()?;
	Some((Token::Ternary(tokenised), tail))
}

///Gets a complete statement in square brackets. Returns None on unmatched brackets.
/// The brackets are not in the string in the UnparsedArrayAccess token
fn get_array_access(s: &str) -> Option<(Token, &str)> {
	let (paren, tail) = get_prio_section('[', ']', s)?;
	let tokenised = Token::by_byte(paren).ok()?;
	Some((Token::ArrayAccess(tokenised), tail))
}

///Gets char in apostrophes. returns None on unmatched apostrophes
fn get_char(s: &str) -> Option<(Token, &str)> {
	if s.len() < 3 {
		return None;
	}
	if let Some(&[b'\'', c, b'\'']) = s.as_bytes().get(0..3) {
		return Some((Token::Char(c as char), &s[3..]));
	}
	if let Some(&[b'\'', b'\\', c, b'\'']) = s.as_bytes().get(0..4) {
		//from: https://en.wikipedia.org/wiki/Escape_sequences_in_C
		let res = match c {
			b'a' => 0x07 as char,
			b'b' => 0x08 as char,
			b'e' => 0x1B as char,
			b'f' => 0x0C as char,
			b'n' => 0x0A as char,
			b'r' => 0x0D as char,
			b't' => 0x09 as char,
			b'v' => 0x0B as char,
			b'\\' => 0x5C as char,
			b'\'' => 0x27 as char,
			b'"' => 0x22 as char,
			b'?' => 0x3F as char,
			_ => {
				return None;
			}
		};
		return Some((Token::Char(res), &s[4..]));
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

///Matches against a single pattern in the list of keywords and its potential followed whitespace
fn get_single_token_match(s: &str) -> Option<(Token, &str)> {
	let get_keyword = || {
		KEYWORDS
			.iter()
			.find(|(pat, _)| {
				s.starts_with(pat)
					&& !s[pat.len()..].starts_with(|c: char| c.is_alphanumeric() || c == '_')
			})
			.map(|(pat, token)| (token(), s[pat.len()..].trim()))
	};
	let get_operator = || {
		OPERATORS
			.iter()
			.find(|(pat, _)| s.starts_with(pat))
			.map(|(pat, token)| (token(), s[pat.len()..].trim()))
	};
	get_keyword().or_else(get_operator)
}

pub(crate) fn get_token(s: &str) -> Result<(Token, &str)> {
	None.or_else(|| get_number(s))
		.or_else(|| get_single_token_match(s))
		.or_else(|| get_parenthesis(s))
		.or_else(|| get_block(s))
		.or_else(|| get_array_access(s))
		.or_else(|| get_ternary_op(s))
		.or_else(|| get_string_literal(s))
		.or_else(|| get_char(s))
		.or_else(|| get_name(s))
		.ok_or_else(|| error!(TokenFail, s))
}

const FORBIDDEN_CHARACTERS: &[char] = &[
	'+', '-', '/', '*', '.', ',', '!', '~', '<', '>', '&', '|', '\\', '\'', '"', '(', ')', '[',
	']', '{', '}', '`', '´', '?', '=', '@', '£', '#', '$', '¤', '%', '¨', '§', ';', ':',
];

//Closure needed instead of values because pointer types are boxed
type TokenFunction = fn() -> Token<'static>;
const KEYWORDS: [(&str, TokenFunction); 59] = [
	("_Alignas", || Token::AlignAs),
	("_Alignof", || Token::AlignOf),
	("_Atomic", || Token::Atomic),
	("_Bool", || Token::Decl(NativeType::Bool)),
	("_Complex", || Token::Complex),
	("_Generic", || Token::Generic),
	("_Imaginary", || Token::Imaginary),
	("_Noreturn", || Token::NoReturn),
	("_Static_assert", || Token::StaticAssert),
	("_Thread_local", || Token::ThreadLocal),
	("auto", || Token::Auto),
	("break", || Token::Break),
	("case", || Token::Case),
	("complex", || Token::Complex),
	("const", || Token::Const),
	("continue", || Token::Continue),
	("default", || Token::Default),
	("do", || Token::Do),
	("double", || Token::Double),
	("else", || Token::Else),
	("enum", || Token::Enum),
	("extern", || Token::Extern),
	("false", || Token::Bool(false)),
	("float", || Token::Float),
	("for", || Token::For),
	("generic", || Token::Generic),
	("goto", || Token::Goto),
	("if", || Token::If),
	("imaginary", || Token::Imaginary),
	("inline", || Token::Inline),
	("long", || Token::Long),
	("namespace", || Token::Namespace),
	("noreturn", || Token::NoReturn),
	("register", || Token::Register),
	("restrict", || Token::Restrict),
	("return", || Token::Return),
	("short", || Token::Short),
	("signed", || Token::Signed),
	("sizeof", || Token::SizeOf),
	("static_assert", || Token::StaticAssert),
	("static", || Token::Static),
	("struct", || Token::Struct),
	("switch", || Token::Switch),
	("true", || Token::Bool(true)),
	("typedef", || Token::TypeDef),
	("union", || Token::Union),
	("unsigned", || Token::Unsigned),
	("volatile", || Token::Volatile),
	("while", || Token::While),
	("int*", || Token::Decl(NativeType::ptr(NativeType::Int))),
	("bool*", || Token::Decl(NativeType::ptr(NativeType::Bool))),
	("char*", || Token::Decl(NativeType::ptr(NativeType::Char))),
	("uint*", || Token::Decl(NativeType::ptr(NativeType::Uint))),
	("void*", || Token::Decl(NativeType::ptr(NativeType::Void))),
	("int", || Token::Decl(NativeType::Int)),
	("bool", || Token::Decl(NativeType::Bool)),
	("char", || Token::Decl(NativeType::Char)),
	("uint", || Token::Decl(NativeType::Uint)),
	("void", || Token::Decl(NativeType::Void)),
];

const OPERATORS: [(&str, TokenFunction); 32] = [
	("(any)", || Token::AnyCast),
	("(bool)", || Token::BoolCast),
	("(char)", || Token::CharCast),
	("(int)", || Token::IntCast),
	("(uint)", || Token::UintCast),
	("==", || Token::Cmp),
	("!=", || Token::NotCmp),
	(">=", || Token::GreaterThanEqual),
	("<=", || Token::LessThanEqual),
	("->", || Token::FieldPointerAccess),
	("<<", || Token::LShift),
	(">>", || Token::RShift),
	("&&", || Token::BoolAnd),
	("||", || Token::BoolOr),
	("=", || Token::Assign),
	("+", || Token::Add),
	("-", || Token::Sub),
	("*", || Token::Mul),
	("/", || Token::Div),
	("%", || Token::Mod),
	(">", || Token::GreaterThan),
	("<", || Token::LessThan),
	("&", || Token::BitAnd),
	("|", || Token::BitOr),
	("^", || Token::Xor),
	("!", || Token::BoolNot),
	("~", || Token::BitNot),
	(".", || Token::FieldAccess),
	(",", || Token::Comma),
	(";", || Token::NewLine),
	("::", || Token::NamespaceSplitter),
	(":", || Token::Colon),
];

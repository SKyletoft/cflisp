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
		&& s.as_bytes().get(2).map(|d| d.is_ascii_hexdigit()) == Some(true)
	{
		let (len, num) = s
			.bytes()
			.enumerate()
			.skip(2)
			.take_while(|(_, d)| d.is_ascii_hexdigit())
			.fold((0, 0), |(_, acc), (len, curr)| {
				(len, acc * 16 + hex_digit_value(curr))
			});
		Some((Token::Num(num), &s[(len + 1)..]))
	} else {
		None
	}
}

//Get rid of and rely on optimisation and negate operation?
///Gets a hex number from the source and returns the remaining source and a number token. Negative only
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

///Gets a hex number from the source and returns the remaining source and a number token. Unsigned only
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
	let mut parentheses = 0;
	let mut len = 0;
	for c in s.chars() {
		len += 1;
		if c == start {
			parentheses += 1;
		} else if c == end {
			parentheses -= 1;
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
/// The parentheses are not in the string in the UnparsedParentheses token
fn get_parenthesis(s: &str) -> Option<(Token, &str)> {
	get_prio_section('(', ')', s).map(|(a, b)| (Token::UnparsedParentheses(a), b))
}

///Gets a complete statement in curly brackets. Returns None on unmatched brackets.
/// The brackets are not in the string in the UnparsedBlock token
fn get_block(s: &str) -> Option<(Token, &str)> {
	get_prio_section('{', '}', s).map(|(a, b)| (Token::UnparsedBlock(a), b))
}

///Gets the middle part of a ternary statement to later be used as a binary operator
/// if the expression lack a colon None will be returned.
/// `?` and `:` are not included in the string
fn get_ternary_op(s: &str) -> Option<(Token, &str)> {
	get_prio_section('?', ':', s).map(|(a, b)| (Token::Ternary(a), b))
}

///Gets a complete statement in square brackets. Returns None on unmatched brackets.
/// The brackets are not in the string in the UnparsedArrayAccess token
fn get_array_access(s: &str) -> Option<(Token, &str)> {
	get_prio_section('[', ']', s).map(|(a, b)| (Token::UnparsedArrayAccess(a), b))
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

pub(crate) fn get_token(s: &str) -> Result<(Token, &str), ParseError> {
	None.or_else(|| get_number(s))
		.or_else(|| get_single_token_match(s))
		.or_else(|| get_parenthesis(s))
		.or_else(|| get_block(s))
		.or_else(|| get_array_access(s))
		.or_else(|| get_ternary_op(s))
		.or_else(|| get_string_literal(s))
		.or_else(|| get_char(s))
		.or_else(|| get_name(s))
		.ok_or_else(|| {
			dbg!(s);
			ParseError(line!(), "Couldn't parse token")
		})
}

const FORBIDDEN_CHARACTERS: &[char] = &[
	'+', '-', '/', '*', '.', ',', '!', '~', '<', '>', '&', '|', '\\', '\'', '"', '(', ')', '[',
	']', '{', '}', '`', '´', '?', '=', '@', '£', '#', '$', '¤', '%', '¨', '§', ';', ':',
];

//Literal, needs following whitespace, closure to return token
//Closure needed instead of values because pointer types are boxed
type TokenFunction = fn() -> Token<'static>;
const KEYWORDS: [(&str, TokenFunction); 59] = [
	("_Alignas", || AlignAs),
	("_Alignof", || AlignOf),
	("_Atomic", || Atomic),
	("_Bool", || Decl(NativeType::Bool)),
	("_Complex", || Complex),
	("_Generic", || Generic),
	("_Imaginary", || Imaginary),
	("_Noreturn", || NoReturn),
	("_Static_assert", || StaticAssert),
	("_Thread_local", || ThreadLocal),
	("auto", || Auto),
	("break", || Break),
	("case", || Case),
	("complex", || Complex),
	("const", || Const),
	("continue", || Continue),
	("default", || Default),
	("do", || Do),
	("double", || Double),
	("else", || Else),
	("enum", || Enum),
	("extern", || Extern),
	("false", || Bool(false)),
	("float", || Float),
	("for", || For),
	("generic", || Generic),
	("goto", || Goto),
	("if", || If),
	("imaginary", || Imaginary),
	("inline", || Inline),
	("long", || Long),
	("namespace", || Namespace),
	("noreturn", || NoReturn),
	("register", || Register),
	("restrict", || Restrict),
	("return", || Return),
	("short", || Short),
	("signed", || Signed),
	("sizeof", || SizeOf),
	("static_assert", || StaticAssert),
	("static", || Static),
	("struct", || Struct),
	("switch", || Switch),
	("true", || Bool(true)),
	("typedef", || TypeDef),
	("union", || Union),
	("unsigned", || Unsigned),
	("volatile", || Volatile),
	("while", || While),
	("int*", || Decl(NativeType::ptr(NativeType::Int))),
	("bool*", || Decl(NativeType::ptr(NativeType::Bool))),
	("char*", || Decl(NativeType::ptr(NativeType::Char))),
	("uint*", || Decl(NativeType::ptr(NativeType::Uint))),
	("void*", || Decl(NativeType::ptr(NativeType::Void))),
	("int", || Decl(NativeType::Int)),
	("bool", || Decl(NativeType::Bool)),
	("char", || Decl(NativeType::Char)),
	("uint", || Decl(NativeType::Uint)),
	("void", || Decl(NativeType::Void)),
];

const OPERATORS: [(&str, TokenFunction); 31] = [
	("(bool)", || BoolCast),
	("(int)", || IntCast),
	("(char)", || CharCast),
	("(any)", || AnyCast),
	("==", || Cmp),
	("!=", || NotCmp),
	(">=", || GreaterThanEqual),
	("<=", || LessThanEqual),
	("->", || FieldPointerAccess),
	("<<", || LShift),
	(">>", || RShift),
	("&&", || BoolAnd),
	("||", || BoolOr),
	("=", || Assign),
	("+", || Add),
	("-", || Sub),
	("*", || Mul),
	("/", || Div),
	("%", || Mod),
	(">", || GreaterThan),
	("<", || LessThan),
	("&", || BitAnd),
	("|", || BitOr),
	("^", || Xor),
	("!", || BoolNot),
	("~", || BitNot),
	(".", || FieldAccess),
	(",", || Comma),
	(";", || NewLine),
	("::", || NamespaceSplitter),
	(":", || Colon),
];

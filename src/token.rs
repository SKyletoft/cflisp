use crate::*;
use types::{Statement, Type};
use Token::*;

///All possible tokens in the source (after comments have been removed)
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Token<'a> {
	Decl(Type),
	If,
	Else,
	Assign,
	Add,
	Sub,
	Mul,
	Div,
	Mod,
	Cmp,
	GT,
	LT,
	RShift,
	LShift,
	Array(Vec<&'a str>),
	StringLit,
	Char(char),
	Bool(bool),
	AdrOf(&'a str),
	Deref(&'a str),
	Name(&'a str),
	And,
	Or,
	Return,
	Num(isize),
	For,
	While,
	Args(Vec<Variable<'a>>),
	UnparsedBlock(&'a str),
	NewLine,
	Xor,
	Not,
}

impl<'a> Token<'a> {
	pub(crate) fn parse_str_to_vec(source: &'a str) -> Result<Vec<Token<'a>>, ParseError> {
		helper::split(source)?
			.into_iter()
			.map(Token::parse)
			.collect::<Result<_, _>>()
	}
	pub(crate) fn parse(name: &'a str) -> Result<Self, ParseError> {
		let token = match name {
			";" => NewLine,
			"int" => Decl(Type::Int),
			"bool" => Decl(Type::Bool),
			"char" => Decl(Type::Char),
			"uint" => Decl(Type::Uint),
			"int*" => Decl(Type::IntPtr),
			"bool*" => Decl(Type::BoolPtr),
			"char*" => Decl(Type::CharPtr),
			"uint*" => Decl(Type::UintPtr),
			"void" => Decl(Type::Void),
			"void*" => Decl(Type::VoidPtr),
			"if" => If,
			"else" => Else,
			"=" => Assign,
			"+" => Add,
			"-" => Sub,
			"*" => Mul,
			"/" => Div,
			"%" => Mod,
			"==" => Cmp,
			">" => GT,
			"<" => LT,
			"&&" => And,
			"&" => And,
			"|" => Or,
			"||" => Or,
			"^" => Xor,
			"!" => Not,
			"~" => Not,
			"true" => Bool(true),
			"false" => Bool(false),
			"return" => Return,
			"<<" => LShift,
			">>" => RShift,
			"()" => Args(vec![]),
			"for" => For,
			"while" => While,
			n if n.starts_with('&') => AdrOf(&n[1..]),
			n if n.starts_with('*') => Deref(&n[1..]),
			n if n.starts_with('(') || n.starts_with('{') || n.starts_with('[') => UnparsedBlock(n),
			n if n.starts_with('"') => {
				return Err(ParseError(line!(), "Strings are not supported (yet?)"));
				//StringLit
			}
			n if n.starts_with('\'') && n.len() == 3 && n.ends_with('\'') => {
				Token::Char(n.as_bytes()[1] as char)
			}
			n => {
				if n.starts_with(|n: char| n == '-' || n.is_ascii_digit())
					&& n.chars().skip(1).all(|r| r.is_ascii_digit())
				{
					Num(n.parse::<isize>().map_err(|_| {
						ParseError(line!(), "Number parse error: Is the number too large?")
					})?)
				} else {
					Token::Name(n)
				}
			}
		};
		Ok(token)
	}

	///Parses UnparsedBlock and makes sure it has `{}`. Allows multiple lines
	pub(crate) fn parse_block_tokens(t: Token<'a>) -> Result<Vec<Token<'a>>, ParseError> {
		if let UnparsedBlock(s) = t {
			if !(s.starts_with('{') && s.ends_with('}')) {
				Err(ParseError(
					line!(),
					"Expected block. Didn't find block. Are you forgetting the closing }}?",
				))
			} else {
				let short = helper::remove_parentheses(s);
				Token::parse_str_to_vec(short)
			}
		} else {
			Err(ParseError(
				line!(),
				"Expected block. Didn't find block. Are you forgetting the opening {{?",
			))
		}
	}

	pub(crate) fn parse_statement_tokens(
		t: Token<'a>,
	) -> Result<Vec<StatementToken<'a>>, ParseError> {
		if let UnparsedBlock(s) = t {
			if !(s.starts_with('(') && s.ends_with(')')) {
				Err(ParseError(
					line!(),
					"Expected block. Didn't find block. Are you forgetting the closing )?",
				))
			} else {
				let short = helper::remove_parentheses(s);
				let res = Token::parse_str_to_vec(short)?;
				if res.contains(&NewLine) {
					return Err(ParseError(line!(), "Statement ended early?"));
				}
				StatementToken::from_tokens(&res)
			}
		} else {
			Err(ParseError(
				line!(),
				"Expected block. Didn't find block. Are you forgetting the opening (?",
			))
		}
	}

	pub(crate) fn parse_arguments_tokens(t: Token<'a>) -> Result<Vec<Statement<'a>>, ParseError> {
		if let UnparsedBlock(s) = t {
			if !(s.starts_with('(') && s.ends_with(')')) {
				Err(ParseError(
					line!(),
					"Expected block. Didn't find block. Are you forgetting the closing )?",
				))
			} else {
				helper::remove_parentheses(s)
					.split(',')
					.map(|slice| {
						Token::parse_str_to_vec(slice).map(|t| StatementToken::from_tokens(&t))
					})
					.collect::<Result<Result<Vec<Statement<'a>>, _>, _>>()?
			}
		} else {
			Err(ParseError(
				line!(),
				"Expected arguments. Didn't find block. Are you forgetting the opening (?",
			))
		}
	}

	pub(crate) fn parse_argument_list_tokens(
		t: Token<'a>,
	) -> Result<Vec<Variable<'a>>, ParseError> {
		if let UnparsedBlock(s) = t {
			if !(s.starts_with('(') && s.ends_with(')')) {
				Err(ParseError(
					line!(),
					"Expected block. Didn't find block. Are you forgetting the closing )?",
				))
			} else {
				let short = helper::remove_parentheses(s);
				let tokens = Token::parse_str_to_vec(short)?;
				if tokens.contains(&NewLine) {
					return Err(ParseError(line!(), "Statement ended early?"));
				}
				let mut arguments = Vec::new();
				for (type_token, name_token) in tokens
					.iter()
					.step_by(2)
					.zip(tokens.iter().skip(1).step_by(2))
				{
					if let (Decl(t), Name(n)) = (type_token, name_token) {
						arguments.push(Variable { typ: *t, name: *n })
					} else {
						return Err(ParseError(line!(), "Couldn't parse argument list"));
					}
				}
				Ok(arguments)
			}
		} else {
			Err(ParseError(
				line!(),
				"Expected arguments. Didn't find block. Are you forgetting the opening (?",
			))
		}
	}
}

use crate::*;

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
	NotCmp,
	GreaterThan,
	GreaterThanEqual,
	LessThan,
	LessThanEqual,
	RShift,
	LShift,
	Char(char),
	Bool(bool),
	AdrOf(&'a str),
	Deref(Box<[Token<'a>; 1]>),
	Name(&'a str),
	And,
	Or,
	Return,
	Num(isize),
	For,
	While,
	Break,
	Continue,
	Switch,
	UnparsedSource(&'a str),
	UnparsedBlock(&'a str),
	UnparsedParentheses(&'a str),
	UnparsedArrayAccess(&'a str),
	NewLine,
	Xor,
	Not,
}

impl<'a> Token<'a> {
	//It was at this point I realised that having clang-format work on all my test cases
	// sheltered me from realising that I was only splitting at whitespace
	///Splits a string into a list of tokens, splitting at whitespace and removing trailing commas
	pub(crate) fn parse_str_to_vec(source: &'a str) -> Result<Vec<Token<'a>>, ParseError> {
		helper::split(source)?
			.into_iter()
			.map(|s| s.strip_suffix(',').unwrap_or(s))
			.map(Token::parse)
			.collect::<Result<_, _>>()
			.map(Token::fix_deref)
	}

	//Takes ownership and returns it instead of taking a reference for easier use in a map
	///Fixes operator misread as multiplication into deref
	fn fix_deref(mut vec: Vec<Token<'a>>) -> Vec<Token<'a>> {
		//dbg!(&vec);
		let mut i = 0;
		while i < vec.len() {
			if vec[i] == Token::Mul
				&& matches!(
					vec.get(i + 1),
					Some(UnparsedParentheses(_)) | Some(Num(_)) | Some(Name(_)) | Some(Deref(_))
				) && vec.get(i.wrapping_sub(1)).map(Token::is_op) != Some(false)
			{
				let following = vec.remove(i + 1);
				vec[i] = Token::Deref(Box::new([following]));
				i = i.saturating_sub(2);
			}
			i += 1;
		}
		if vec[0] == Token::Mul
			|| vec
				.windows(2)
				.any(|win| win[0] == NewLine && win[1] == Token::Mul)
		{
			dbg!(vec);
			panic!();
		}
		vec
	}

	///Maps words to tokens and parses literals
	pub(crate) fn parse(name: &'a str) -> Result<Self, ParseError> {
		let token =
			match name {
				";" => NewLine,
				"int" => Decl(Type::Int),
				"bool" => Decl(Type::Bool),
				"char" => Decl(Type::Char),
				"uint" => Decl(Type::Uint),
				"void" => Decl(Type::Void),
				"int*" => Decl(Type::Ptr(Box::new(Type::Int))),
				"bool*" => Decl(Type::Ptr(Box::new(Type::Bool))),
				"char*" => Decl(Type::Ptr(Box::new(Type::Char))),
				"uint*" => Decl(Type::Ptr(Box::new(Type::Uint))),
				"void*" => Decl(Type::Ptr(Box::new(Type::Void))),
				"if" => If,
				"else" => Else,
				"=" => Assign,
				"+" => Add,
				"-" => Sub,
				"*" => Mul,
				"/" => Div,
				"%" => Mod,
				"==" => Cmp,
				"!=" => NotCmp,
				">" => GreaterThan,
				">=" => GreaterThanEqual,
				"<" => LessThan,
				"<=" => LessThanEqual,
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
				"for" => For,
				"while" => While,
				"break" => Break,
				"continue" => Continue,
				"switch" => Switch,
				n if n.starts_with('&') => AdrOf(&n[1..]),
				n if n.starts_with('*') => Deref(Box::new([UnparsedBlock(&n[1..])])),
				n if n.starts_with('(') && n.ends_with(')') && n.len() >= 2 => {
					UnparsedParentheses(&n[1..n.len() - 1].trim())
				}
				n if n.starts_with('{') && n.ends_with('}') && n.len() >= 2 => {
					UnparsedBlock(&n[1..n.len() - 1].trim())
				}
				n if n.starts_with('[') && n.ends_with(']') && n.len() >= 2 => {
					UnparsedArrayAccess(&n[1..n.len() - 1].trim())
				}
				n if n.starts_with('"') && n.ends_with('"') && n.len() >= 2 => {
					return Err(ParseError(line!(), "Strings are not supported (yet?)"));
					//StringLit(&n[1..n.len()-1].trim())
				}
				n if n.starts_with('\'') && n.len() == 3 && n.ends_with('\'') => {
					Token::Char(n.as_bytes()[1] as char)
				}
				n if n.starts_with(|c: char| c == '-' || c.is_ascii_digit())
					&& n.chars().skip(1).all(|r| r.is_ascii_digit()) =>
				{
					Num(n.parse::<isize>().map_err(|_| {
						ParseError(line!(), "Number parse error: Is the number too large?")
					})?)
				}
				n if n.starts_with("0x")
					&& n.len() > 2 && n.chars().skip(2).all(|r| r.is_ascii_hexdigit()) =>
				{
					Num(isize::from_str_radix(&n[2..], 16).map_err(|_| {
						ParseError(
							line!(),
							"Number parse error: Is the number too large? (hex)",
						)
					})?)
				}
				n if n.starts_with('(')
					|| n.ends_with(')') || n.starts_with('{')
					|| n.ends_with('}') || n.starts_with('[')
					|| n.ends_with(']') || n.starts_with('"')
					|| n.ends_with('"') || n.starts_with(|d: char| d.is_ascii_digit()) =>
				{
					UnparsedSource(n)
				}
				n => Token::Name(n),
			};
		Ok(token)
	}

	///Parses string from `Token::UnparsedBlock`. Allows multiple lines
	pub(crate) fn parse_block_tokens(s: &'a str) -> Result<Vec<Token<'a>>, ParseError> {
		if s.is_empty() {
			return Ok(Vec::new());
		}
		Token::parse_str_to_vec(s)
	}

	///Converts string from `Token::UnparsedBlock` into `StatementToken`s
	pub(crate) fn parse_statement_tokens(
		s: &'a str,
	) -> Result<Vec<StatementToken<'a>>, ParseError> {
		let res = Token::parse_str_to_vec(s)?;
		if res.contains(&NewLine) {
			return Err(ParseError(line!(), "Statement ended early?"));
		}
		StatementToken::from_tokens(&res)
	}

	///Parses `Token::UnparsedBlock` as a list of statements. (Function *call*, not declaration)
	pub(crate) fn parse_arguments_tokens(s: &'a str) -> Result<Vec<Statement<'a>>, ParseError> {
		s.split(',')
			.map(|slice| Token::parse_str_to_vec(slice).map(|t| StatementToken::from_tokens(&t)))
			.collect::<Result<Result<Vec<Statement<'a>>, _>, _>>()?
	}

	///Parses string from `Token::UnparsedParentheses` as a list of types and names. (Function *declaration*, not call)
	pub(crate) fn parse_argument_list_tokens(s: &'a str) -> Result<Vec<Variable<'a>>, ParseError> {
		if s.is_empty() {
			return Ok(Vec::new());
		}
		let tokens = Token::parse_str_to_vec(s)?;
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
				arguments.push(Variable {
					typ: t.clone(),
					name: *n,
				})
			} else {
				return Err(ParseError(line!(), "Couldn't parse argument list"));
			}
		}
		Ok(arguments)
	}

	//Should & and * be considered operators?
	///Is the token an operator? Address of (&) and derefrencing (*) are not considered operators
	fn is_op(&self) -> bool {
		matches!(
			self,
			Add | Sub
				| Mul | Div | Mod
				| Cmp | GreaterThan
				| LessThan | RShift
				| LShift | And | Or
				| Xor | Not | Assign
				| NewLine | Decl(_)
				| UnparsedBlock(_)
		)
	}
}

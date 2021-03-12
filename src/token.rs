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
	AdrOf,
	Deref,
	Name(&'a str),
	BitAnd,
	BitOr,
	BoolAnd,
	BoolOr,
	Xor,
	BoolNot,
	BitNot,
	Return,
	Num(isize),
	For,
	While,
	Break,
	Continue,
	Switch,
	Static,
	Struct,
	TypeDef,
	FieldAccess,
	FieldPointerAccess,
	UnparsedSource(&'a str),
	UnparsedBlock(&'a str),
	UnparsedParentheses(&'a str),
	UnparsedArrayAccess(&'a str),
	NewLine,
	Comma,
}

impl<'a> Token<'a> {
	///Splits a string into a list of tokens by matching pattern by pattern instead of
	/// splitting and then converting into tokens
	pub(crate) fn by_byte(source: &'a str) -> Result<Vec<Token<'a>>, ParseError> {
		let mut src = source.trim_start();
		let mut vec = Vec::new();
		while !src.is_empty() {
			let (token, rest) = helper::get_token(src)?;
			vec.push(token);
			src = rest.trim_start();
		}
		//vec = Token::fix_deref(vec);
		dbg!(source, &vec);
		Ok(vec)
	}

	//Doesn't work with structs
	//Takes ownership and returns it instead of taking a reference for easier use in a map
	///Fixes operator misread as multiplication into deref and bitand into adrof
	fn fix_deref(mut vec: Vec<Token>) -> Vec<Token> {
		for i in (1..vec.len() - 1).rev() {
			if vec[i] == Mul && vec[i - 1].is_op() {
				vec[i] = Deref;
			}
			if vec[i] == BitAnd && vec[i - 1].is_op() {
				vec[i] = AdrOf;
			}
		}
		vec
	}

	/*///Maps words to tokens and parses literals
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
				"&&" => BoolAnd,
				"&" => BitAnd,
				"|" => BitOr,
				"||" => BoolOr,
				"^" => Xor,
				"!" => BoolNot,
				"~" => BitNot,
				"." => FieldAccess,
				"->" => FieldPointerAccess,
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
				"static" => Static,
				"struct" => Struct,
				"typedef" => TypeDef,
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
	}*/

	///Parses string from `Token::UnparsedBlock`. Allows multiple lines
	pub(crate) fn parse_block_tokens(s: &'a str) -> Result<Vec<Token<'a>>, ParseError> {
		if s.is_empty() {
			return Ok(Vec::new());
		}
		//Token::parse_str_to_vec(s)
		Token::by_byte(s)
	}

	///Converts string from `Token::UnparsedBlock` into `StatementToken`s
	pub(crate) fn parse_statement_tokens(
		s: &'a str,
	) -> Result<Vec<StatementToken<'a>>, ParseError> {
		//let res = Token::parse_str_to_vec(s)?;
		let res = Token::by_byte(s)?;
		if res.contains(&NewLine) {
			return Err(ParseError(line!(), "Statement ended early?"));
		}
		StatementToken::from_tokens(&res)
	}

	///Parses `Token::UnparsedParentheses` as a list of statements. (Function *call*, not declaration)
	pub(crate) fn parse_arguments_tokens(s: &'a str) -> Result<Vec<Statement<'a>>, ParseError> {
		eprintln!("Expected error here");
		dbg!(s);
		s.split(',')
			//.map(|slice| Token::parse_str_to_vec(slice).map(|t| StatementToken::from_tokens(&t)))
			.map(|slice| Token::by_byte(slice).map(|t| StatementToken::from_tokens(&t)))
			.collect::<Result<Result<Vec<Statement<'a>>, _>, _>>()?
	}

	///Parses string from `Token::UnparsedParentheses` as a list of types and names. (Function *declaration*, not call)
	pub(crate) fn parse_argument_list_tokens(s: &'a str) -> Result<Vec<Variable<'a>>, ParseError> {
		if s.is_empty() {
			return Ok(Vec::new());
		}
		let tokens = Token::by_byte(s)?;
		if tokens.contains(&NewLine) {
			return Err(ParseError(line!(), "Statement ended early?"));
		}
		if tokens.len() % 2 != 0 {
			return Err(ParseError(
				line!(),
				"Wrong amount of tokens in function argument declaration",
			));
		}
		let mut arguments = Vec::new();
		for slice in tokens.windows(2).step_by(2) {
			if let [Decl(t), Name(n)] = slice {
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

	///Parses string from `Token::UnparsedParentheses` as a list of types and names. (Struct *declaration*, not construction)
	pub(crate) fn parse_struct_member_tokens(s: &'a str) -> Result<Vec<Variable<'a>>, ParseError> {
		if s.is_empty() {
			return Ok(Vec::new());
		}
		let tokens = Token::by_byte(s)?;
		if tokens.len() % 3 != 0 {
			return Err(ParseError(
				line!(),
				"Wrong amount of tokens in struct declaration",
			));
		}
		let mut arguments = Vec::new();
		for slice in tokens.windows(3).step_by(3) {
			if let [Decl(t), Name(n), NewLine] = slice {
				arguments.push(Variable {
					typ: t.clone(),
					name: *n,
				})
			} else {
				dbg!(s, tokens);
				return Err(ParseError(line!(), "Couldn't struct members"));
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
				| LShift | BitAnd
				| BoolAnd | BitOr
				| BoolOr | Xor | BitNot
				| BoolNot | Assign
				| NewLine | Decl(_)
				| UnparsedBlock(_)
		)
	}
}

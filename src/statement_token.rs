use crate::*;

///A reduced set of tokens for use in statements
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum StatementToken<'a> {
	Num(isize),
	Bool(bool),
	Char(char),
	Var(&'a str),
	FunctionCall(&'a str, Vec<Vec<StatementToken<'a>>>),
	Add,
	Sub,
	Mul,
	Div,
	Mod,
	And,
	Or,
	Xor,
	RShift,
	LShift,
	GreaterThan,
	GreaterThanEqual,
	LessThan,
	LessThanEqual,
	Cmp,
	NotCmp,
	Not,
	Parentheses(Vec<StatementToken<'a>>),
	AdrOf(&'a str),
	Deref(Vec<StatementToken<'a>>),
	ArrayAccess {
		ptr: &'a str,
		idx: Vec<StatementToken<'a>>,
	},
	Array(Vec<Vec<StatementToken<'a>>>),
}

impl<'a> StatementToken<'a> {
	pub(crate) fn from_tokens(tokens: &[Token<'a>]) -> Result<Statement<'a>, ParseError> {
		let mut res = Vec::new();
		for token in tokens {
			let last = res.len().wrapping_sub(1);
			let new = match token {
				Token::Bool(b) => StatementToken::Bool(*b),
				Token::Num(n) => StatementToken::Num(*n),
				Token::Char(c) => StatementToken::Char(*c),
				Token::Name(n) => StatementToken::Var(n),
				Token::Add => StatementToken::Add,
				Token::Sub => StatementToken::Sub,
				Token::Mul => StatementToken::Mul,
				Token::Div => StatementToken::Div,
				Token::Mod => StatementToken::Mod,
				Token::And => StatementToken::And,
				Token::Or => StatementToken::Or,
				Token::Xor => StatementToken::Xor,
				Token::Not => StatementToken::Not,
				Token::LShift => StatementToken::LShift,
				Token::RShift => StatementToken::RShift,
				Token::GreaterThan => StatementToken::GreaterThan,
				Token::LessThan => StatementToken::LessThan,
				Token::GreaterThanEqual => StatementToken::GreaterThanEqual,
				Token::LessThanEqual => StatementToken::LessThanEqual,
				Token::Cmp => StatementToken::Cmp,
				Token::NotCmp => StatementToken::NotCmp,
				Token::AdrOf(n) => StatementToken::AdrOf(n),
				Token::Deref(b) => StatementToken::Deref(StatementToken::from_tokens(b.as_ref())?),
				Token::UnparsedParentheses(b) => {
					if let Some(StatementToken::Var(n)) = res.get(last) {
						res[last] =
							StatementToken::FunctionCall(n, Token::parse_arguments_tokens(b)?);
						continue;
					} else {
						let tokenised = Token::parse_str_to_vec(helper::remove_parentheses(b))?;
						let as_statement = StatementToken::from_tokens(&tokenised)?;
						StatementToken::Parentheses(as_statement)
					}
				}
				Token::UnparsedArrayAccess(b) => {
					if let Some(StatementToken::Var(n)) = res.get(last) {
						let idx = Token::parse_str_to_vec(helper::remove_parentheses(b))?;
						let as_statement = StatementToken::from_tokens(&idx)?;
						res[last] = StatementToken::ArrayAccess {
							ptr: n,
							idx: as_statement,
						};
						continue;
					} else {
						return Err(ParseError(
							line!(),
							"Not (yet?) implemented: Array access to any pointer",
						));
					}
				}
				Token::UnparsedBlock(b) => {
					let items = helper::remove_parentheses(b)
						.split(',')
						.map(|s| s.trim())
						.collect::<Vec<_>>();
					let mut v = Vec::new();
					for item in items {
						v.push(StatementToken::from_tokens(&Token::parse_str_to_vec(
							item,
						)?)?);
					}
					StatementToken::Array(v)
				}
				Token::UnparsedSource(b) => {
					StatementToken::Parentheses(StatementToken::from_tokens(&[Token::parse(b)?])?)
				}
				_ => {
					dbg!(tokens);
					dbg!(token);
					return Err(ParseError(line!(), "Token is not valid in this context"));
				}
			};
			res.push(new);
		}
		Ok(res)
	}
}

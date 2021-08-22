use super::*;
use crate::*;

///A reduced set of tokens for use in statements
#[derive(Debug, Clone, PartialEq)]
pub enum StatementToken<'a> {
	Num(Number),
	Bool(bool),
	Char(char),
	Var(&'a str),
	FunctionCall(&'a str, Vec<Vec<StatementToken<'a>>>),
	Cast(NativeType),
	Add,
	Sub,
	Mul,
	Div,
	Mod,
	BoolAnd,
	BoolOr,
	BitOr,
	Xor,
	RShift,
	LShift,
	GreaterThan,
	GreaterThanEqual,
	LessThan,
	LessThanEqual,
	Cmp,
	NotCmp,
	BoolNot,
	BitNot,
	Increment,
	Decrement,
	Parentheses(Vec<StatementToken<'a>>),
	Ternary(Vec<StatementToken<'a>>),
	BitAnd,
	FieldAccess,
	FieldPointerAccess,
	ArrayAccess(Vec<StatementToken<'a>>),
	Array(Vec<Vec<StatementToken<'a>>>),
}

impl<'a> StatementToken<'a> {
	pub(crate) fn from_tokens(tokens: &[Token<'a>]) -> Result<Statement<'a>> {
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
				Token::BitAnd => StatementToken::BitAnd,
				Token::BitOr => StatementToken::BitOr,
				Token::BoolAnd => StatementToken::BoolAnd,
				Token::BoolOr => StatementToken::BoolOr,
				Token::Xor => StatementToken::Xor,
				Token::BoolNot => StatementToken::BoolNot,
				Token::BitNot => StatementToken::BitNot,
				Token::LShift => StatementToken::LShift,
				Token::RShift => StatementToken::RShift,
				Token::GreaterThan => StatementToken::GreaterThan,
				Token::LessThan => StatementToken::LessThan,
				Token::GreaterThanEqual => StatementToken::GreaterThanEqual,
				Token::LessThanEqual => StatementToken::LessThanEqual,
				Token::Cmp => StatementToken::Cmp,
				Token::NotCmp => StatementToken::NotCmp,
				Token::FieldAccess => StatementToken::FieldAccess,
				Token::FieldPointerAccess => StatementToken::FieldPointerAccess,
				Token::BoolCast => StatementToken::Cast(NativeType::Bool),
				Token::CharCast => StatementToken::Cast(NativeType::Char),
				Token::IntCast => StatementToken::Cast(NativeType::Int),
				Token::UintCast => StatementToken::Cast(NativeType::Uint),
				Token::Increment => StatementToken::Increment,
				Token::Decrement => StatementToken::Decrement,
				Token::Ternary(b) => {
					let as_statement = StatementToken::from_tokens(b)?;
					StatementToken::Ternary(as_statement)
				}
				Token::Parentheses(tokenised) => {
					if let Some(StatementToken::Var(n)) = res.get(last) {
						let arguments = parser::parse_tokens_to_statements(tokenised)?;
						res[last] = StatementToken::FunctionCall(n, arguments);
						continue;
					} else {
						let as_statement = StatementToken::from_tokens(tokenised)?;
						StatementToken::Parentheses(as_statement)
					}
				}
				Token::ArrayAccess(idx) => {
					let as_statement = StatementToken::from_tokens(idx)?;
					StatementToken::ArrayAccess(as_statement)
				}
				Token::Block(b) => {
					let items = b.split(|t| t == &Token::Comma).collect::<Vec<_>>();
					let mut v = Vec::new();
					for item in items {
						v.push(StatementToken::from_tokens(item)?);
					}
					StatementToken::Array(v)
				}
				Token::Source(tokens) => {
					StatementToken::Parentheses(StatementToken::from_tokens(tokens)?)
				}
				Token::StringLiteral(s) => StatementToken::Array(
					s.chars()
						.map(|c| vec![StatementToken::Char(c)])
						.chain([vec![StatementToken::Char(0 as char)]])
						.collect::<Vec<_>>(),
				),
				_ => {
					return Err(error!(InvalidToken, tokens));
				}
			};
			res.push(new);
		}
		Ok(res)
	}

	pub(crate) fn is_op(&self) -> bool {
		matches!(
			self,
			StatementToken::Add
				| StatementToken::Sub
				| StatementToken::Mul
				| StatementToken::Div
				| StatementToken::Mod
				| StatementToken::BitAnd
				| StatementToken::BitOr
				| StatementToken::BoolAnd
				| StatementToken::BoolOr
				| StatementToken::Xor
				| StatementToken::RShift
				| StatementToken::LShift
				| StatementToken::GreaterThan
				| StatementToken::GreaterThanEqual
				| StatementToken::LessThan
				| StatementToken::LessThanEqual
				| StatementToken::Cmp
				| StatementToken::NotCmp
				| StatementToken::BitNot
				| StatementToken::BoolNot
		)
	}
}

use std::{borrow::Cow, collections::HashMap, fmt};

use crate::*;

///Tree structure to represent a statement.
#[derive(Debug, Clone, PartialEq)]
pub enum StatementElement<'a> {
	Add {
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	Sub {
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	Mul {
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	Div {
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	Mod {
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	LShift {
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	RShift {
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	BitAnd {
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	BitOr {
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	BoolAnd {
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	BoolOr {
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	Xor {
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	GreaterThan {
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	LessThan {
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	GreaterThanEqual {
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	LessThanEqual {
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	Cmp {
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	NotCmp {
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	Cast {
		typ: NativeType,
		value: Box<StatementElement<'a>>,
	},
	Ternary {
		cond: Box<StatementElement<'a>>,
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	FunctionCall {
		name: Cow<'a, str>,
		parametres: Vec<StatementElement<'a>>,
	},
	BitNot(Box<StatementElement<'a>>),
	BoolNot(Box<StatementElement<'a>>),
	Var(Cow<'a, str>),
	Num(Number),
	Char(char),
	Bool(bool),
	Array(Vec<StatementElement<'a>>),
	Deref(Box<StatementElement<'a>>),
	AdrOf(Cow<'a, str>),
	FieldPointerAccess(Cow<'a, str>, Cow<'a, str>),
}

///Takes two `StatementElement`s and returns a single `StatementElement`. All lifetimes are the same
type OpFnPtr<'a> =
	fn(lhs: StatementElement<'a>, rhs: StatementElement<'a>) -> Result<StatementElement<'a>>;

///Takes one `StatementElement`s and returns a single `StatementElement`. All lifetimes are the same
type UnOpFnPtr<'a> = fn(lhs: StatementElement<'a>) -> Result<StatementElement<'a>>;

///Enum type for Work-In-Progress parsing. Either a parsed StatementElement or a unparsed *single* token.
/// Both types are exported.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum MaybeParsed<'a> {
	Parsed(StatementElement<'a>),
	Unparsed(StatementToken<'a>),
}
use MaybeParsed::*;

impl<'a> MaybeParsed<'a> {
	fn map_unparsed<T>(&self, f: fn(&StatementToken<'a>) -> T) -> Option<T> {
		if let Unparsed(internal) = self {
			Some(f(internal))
		} else {
			None
		}
	}
}

impl<'a> StatementElement<'a> {
	pub(crate) fn rename(&mut self, from: &str, to: &str) {
		match self {
			StatementElement::Add { lhs, rhs }
			| StatementElement::Sub { lhs, rhs }
			| StatementElement::Mul { lhs, rhs }
			| StatementElement::Div { lhs, rhs }
			| StatementElement::Mod { lhs, rhs }
			| StatementElement::LShift { lhs, rhs }
			| StatementElement::RShift { lhs, rhs }
			| StatementElement::BitAnd { lhs, rhs }
			| StatementElement::BitOr { lhs, rhs }
			| StatementElement::BoolAnd { lhs, rhs }
			| StatementElement::BoolOr { lhs, rhs }
			| StatementElement::Xor { lhs, rhs }
			| StatementElement::GreaterThan { lhs, rhs }
			| StatementElement::LessThan { lhs, rhs }
			| StatementElement::GreaterThanEqual { lhs, rhs }
			| StatementElement::LessThanEqual { lhs, rhs }
			| StatementElement::Cmp { lhs, rhs }
			| StatementElement::NotCmp { lhs, rhs } => {
				lhs.rename(from, to);
				rhs.rename(from, to);
			}

			StatementElement::Ternary { cond, lhs, rhs } => {
				cond.rename(from, to);
				lhs.rename(from, to);
				rhs.rename(from, to);
			}
			StatementElement::FunctionCall { name, parametres } => {
				if name == from {
					*name = Cow::Owned(to.to_string());
				}
				parametres
					.iter_mut()
					.for_each(|parametre| parametre.rename(from, to));
			}

			StatementElement::BitNot(lhs)
			| StatementElement::BoolNot(lhs)
			| StatementElement::Deref(lhs) => {
				lhs.rename(from, to);
			}

			StatementElement::Array(arr) => {
				arr.iter_mut()
					.for_each(|parametre| parametre.rename(from, to));
			}

			StatementElement::Var(name)
			| StatementElement::AdrOf(name)
			| StatementElement::FieldPointerAccess(name, _) => {
				if name == from {
					*name = Cow::Owned(to.to_string());
				}
			}

			_ => {}
		}
	}

	fn from_token(token: StatementToken<'a>) -> Result<MaybeParsed<'a>> {
		let res = match token {
			StatementToken::Bool(b) => Parsed(StatementElement::Bool(b)),
			StatementToken::Char(c) => Parsed(StatementElement::Char(c)),
			StatementToken::Num(n) => Parsed(StatementElement::Num(n)),
			StatementToken::Var(v) => Parsed(StatementElement::Var(Cow::Borrowed(v))),

			StatementToken::FunctionCall(name, ts) => {
				let parametres = ts
					.into_iter()
					.map(StatementElement::from_statement_tokens)
					.collect::<Result<Vec<_>>>()?;
				Parsed(StatementElement::FunctionCall {
					name: Cow::Borrowed(name),
					parametres,
				})
			}

			StatementToken::Array(arr) => {
				let elements = arr
					.into_iter()
					.map(StatementElement::from_statement_tokens)
					.collect::<Result<Vec<_>>>()?;
				Parsed(StatementElement::Array(elements))
			}

			t => Unparsed(t),
		};
		Ok(res)
	}

	pub(crate) fn from_statement_tokens(
		tokens: Vec<StatementToken<'a>>,
	) -> Result<StatementElement<'a>> {
		let mut working_tokens = tokens
			.into_iter()
			.map(StatementElement::from_token)
			.collect::<Result<Vec<_>>>()?;

		for token in working_tokens.iter_mut() {
			if let Unparsed(StatementToken::Parentheses(p)) = token {
				let next = StatementElement::from_statement_tokens(p.clone())?;
				*token = Parsed(next);
			}
		}

		let ptr_ops: [(MaybeParsed, OpFnPtr); 2] = [
			(Unparsed(StatementToken::FieldAccess), |l, r| {
				if let (StatementElement::Var(lhs), StatementElement::Var(rhs)) = (&l, &r) {
					Ok(StatementElement::Var(helper::merge_name_and_field(
						lhs, rhs,
					)))
				} else {
					Err(error!(
						FieldAccessOnNonNames,
						(&l, &r, &StatementToken::FieldAccess)
					))
				}
			}),
			(Unparsed(StatementToken::FieldPointerAccess), |l, r| {
				if let (StatementElement::Var(lhs), StatementElement::Var(rhs)) = (&l, &r) {
					//Because of the l -> r pattern, these are guaranteed to be Cow::Borrowed, so cloning isn't too dumb
					Ok(StatementElement::FieldPointerAccess(
						lhs.clone(),
						rhs.clone(),
					))
				} else {
					Err(error!(
						FieldAccessOnNonNames,
						(&l, &r, &StatementToken::FieldPointerAccess)
					))
				}
			}),
		];

		macro_rules! gen_bin_op {
			($i:ident) => {
				(Unparsed(StatementToken::$i), |l, r| {
					Ok(StatementElement::$i {
						lhs: Box::new(l),
						rhs: Box::new(r),
					})
				})
			};
		}
		macro_rules! gen_unary_op {
			($i:ident) => {
				(Unparsed(StatementToken::$i), |l| {
					Ok(StatementElement::$i(Box::new(l)))
				})
			};
		}

		let un_ops: [(MaybeParsed, UnOpFnPtr); 5] = [
			gen_unary_op!(BitNot),
			gen_unary_op!(BoolNot),
			(Unparsed(StatementToken::Sub), |r| {
				Ok(StatementElement::Sub {
					lhs: Box::new(StatementElement::Num(Number::ZERO)),
					rhs: Box::new(r),
				})
			}),
			(Unparsed(StatementToken::Mul), |r| {
				Ok(StatementElement::Deref(Box::new(r)))
			}),
			(Unparsed(StatementToken::BitAnd), |r| {
				if let StatementElement::Var(n) = r {
					Ok(StatementElement::AdrOf(n))
				} else {
					Err(error!(AddressOfTemporary, (&r, &StatementToken::BitAnd)))
				}
			}),
		];
		let bin_ops: [(MaybeParsed, OpFnPtr); 18] = [
			gen_bin_op!(Mul),
			gen_bin_op!(Div),
			gen_bin_op!(Mod),
			gen_bin_op!(Add),
			gen_bin_op!(Sub),
			gen_bin_op!(LShift),
			gen_bin_op!(RShift),
			gen_bin_op!(LessThan),
			gen_bin_op!(LessThanEqual),
			gen_bin_op!(GreaterThan),
			gen_bin_op!(GreaterThanEqual),
			gen_bin_op!(Cmp),
			gen_bin_op!(NotCmp),
			gen_bin_op!(BitAnd),
			gen_bin_op!(Xor),
			gen_bin_op!(BitOr),
			gen_bin_op!(BoolAnd),
			gen_bin_op!(BoolOr),
		];

		for (from, to) in ptr_ops.iter() {
			do_binary_operation(&mut working_tokens, from, *to)?;
		}
		do_array_access(&mut working_tokens)?;
		do_cast(&mut working_tokens)?;
		for (from, to) in un_ops.iter() {
			do_unary_operation_left(&mut working_tokens, from, *to)?;
		}
		for (from, to) in bin_ops.iter() {
			do_binary_operation(&mut working_tokens, from, *to)?;
		}

		do_ternary_op(&mut working_tokens)?;

		if working_tokens.len() != 1 {
			return Err(error!(InternalTreeFail, working_tokens.as_slice()));
		}

		if let Parsed(elem) = working_tokens.remove(0) {
			Ok(elem)
		} else {
			Err(error!(InternalUnparsed, working_tokens.as_slice()))
		}
	}

	pub(crate) fn signedness(
		&self,
		symbols: &HashMap<Cow<'a, str>, NativeType>,
	) -> Result<NumberType> {
		let res = match self {
			StatementElement::Add { lhs, rhs }
			| StatementElement::Sub { lhs, rhs }
			| StatementElement::Mul { lhs, rhs }
			| StatementElement::Div { lhs, rhs }
			| StatementElement::Mod { lhs, rhs }
			| StatementElement::BitAnd { lhs, rhs }
			| StatementElement::BitOr { lhs, rhs }
			| StatementElement::Xor { lhs, rhs } => {
				lhs.signedness(symbols)?.promote(rhs.signedness(symbols)?)
			}
			StatementElement::LShift { lhs, .. }
			| StatementElement::RShift { lhs, .. }
			| StatementElement::BitNot(lhs) => lhs.signedness(symbols)?,
			StatementElement::GreaterThan { .. }
			| StatementElement::LessThan { .. }
			| StatementElement::GreaterThanEqual { .. }
			| StatementElement::LessThanEqual { .. }
			| StatementElement::Cmp { .. }
			| StatementElement::NotCmp { .. }
			| StatementElement::Bool(_) => NumberType::BOOL_SIGNEDNESS,
			StatementElement::FunctionCall { name, .. } | StatementElement::Var(name) => symbols
				.get(name)
				.ok_or_else(|| error!(UnknownSymbol, name.to_string()))? //Maybe add `symbols`
				.into(),
			StatementElement::Num(Number { signedness, .. }) => *signedness,
			StatementElement::Char(_) => NumberType::CHAR_SIGNEDNESS,
			StatementElement::Array(arr) => arr
				.first()
				.map(|first| first.signedness(symbols))
				.unwrap_or(Ok(NumberType::Unknown))?,
			StatementElement::Deref(_) => NumberType::Unknown,
			StatementElement::AdrOf(_) => NumberType::Unsigned,

			StatementElement::BoolAnd { .. }
			| StatementElement::BoolOr { .. }
			| StatementElement::BoolNot(_) => NumberType::BOOL_SIGNEDNESS,
			StatementElement::Ternary { lhs, .. } => lhs.signedness(symbols)?,
			StatementElement::FieldPointerAccess(name, field) => symbols
				.get(&helper::merge_name_and_field(name, field))
				.map(|typ| typ.into())
				.unwrap_or(NumberType::Unknown),
			StatementElement::Cast { typ, .. } => typ.into(),
		};
		Ok(res)
	}
}

impl<'a, 'b> Parsable<'a, 'b> for StatementElement<'a> {
	///Runs to semicolon, comma or end of stream
	fn parse(tokens: TokenSlice<'a, 'b>) -> Result<(StatementElement<'a>, TokenSlice<'a, 'b>)> {
		let to_take = tokens
			.iter()
			.position(|t| matches!(t, Token::NewLine | Token::Comma))
			.unwrap_or(tokens.len());
		let (statement, tail) = tokens.split_at(to_take);
		let tail = match tail {
			[Token::Comma | Token::NewLine, rest @ ..] => rest,
			[] => &[],
			_ => return Err(error!(ExcessTokens, tail)),
		};
		let statement_tokens = StatementToken::from_tokens(statement)?;
		let element = StatementElement::from_statement_tokens(statement_tokens)?;
		Ok((element, tail))
	}
}

impl<'a, 'b> Parsable<'a, 'b> for Vec<StatementElement<'a>> {
	fn parse(tokens: TokenSlice<'a, 'b>) -> Result<(Self, TokenSlice<'a, 'b>)> {
		let filter = |slice| match StatementElement::parse(slice) {
			Ok((element, [])) => Ok(element),
			Ok(_) => Err(error!(ExcessTokens, slice)),
			Err(e) => Err(e),
		};
		let vec = tokens
			.split(|t| t == &Token::Comma)
			.map(filter)
			.collect::<Result<Vec<_>>>()?;
		Ok((vec, &[]))
	}
}

impl fmt::Display for StatementElement<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			StatementElement::Add { lhs, rhs } => write!(f, "({}) + ({})", lhs, rhs),
			StatementElement::Sub { lhs, rhs } => write!(f, "({}) - ({})", lhs, rhs),
			StatementElement::Mul { lhs, rhs } => write!(f, "({}) * ({})", lhs, rhs),
			StatementElement::Div { lhs, rhs } => write!(f, "({}) / ({})", lhs, rhs),
			StatementElement::Mod { lhs, rhs } => write!(f, "({}) % ({})", lhs, rhs),
			StatementElement::LShift { lhs, rhs } => write!(f, "({}) << ({})", lhs, rhs),
			StatementElement::RShift { lhs, rhs } => write!(f, "({}) >> ({})", lhs, rhs),
			StatementElement::BitAnd { lhs, rhs } => write!(f, "({}) & ({})", lhs, rhs),
			StatementElement::BitOr { lhs, rhs } => write!(f, "({}) | ({})", lhs, rhs),
			StatementElement::BoolAnd { lhs, rhs } => write!(f, "({}) && ({})", lhs, rhs),
			StatementElement::BoolOr { lhs, rhs } => write!(f, "({}) || ({})", lhs, rhs),
			StatementElement::Xor { lhs, rhs } => write!(f, "({}) ^ ({})", lhs, rhs),
			StatementElement::GreaterThan { lhs, rhs } => write!(f, "({}) > ({})", lhs, rhs),
			StatementElement::LessThan { lhs, rhs } => write!(f, "({}) < ({})", lhs, rhs),
			StatementElement::GreaterThanEqual { lhs, rhs } => write!(f, "({}) <= ({})", lhs, rhs),
			StatementElement::LessThanEqual { lhs, rhs } => write!(f, "({}) >= ({})", lhs, rhs),
			StatementElement::Cmp { lhs, rhs } => write!(f, "({}) == ({})", lhs, rhs),
			StatementElement::NotCmp { lhs, rhs } => write!(f, "({}) != ({})", lhs, rhs),
			StatementElement::Cast { typ, value } => write!(f, "(({}) ({}))", typ, value),
			StatementElement::Ternary { cond, lhs, rhs } => {
				write!(f, "(({}) ? ({}) : ({}))", cond, lhs, rhs)
			}
			StatementElement::FunctionCall { name, parametres } => {
				write!(f, "({}(", name)?;
				for parametre in parametres.iter() {
					write!(f, "({}), ", parametre)?;
				}
				write!(f, "))")
			}
			StatementElement::BitNot(val) => write!(f, "(~{})", val),
			StatementElement::BoolNot(val) => write!(f, "(!{})", val),
			StatementElement::Var(val) => write!(f, "{}", val),
			StatementElement::Num(val) => write!(f, "{}", val),
			StatementElement::Char(val) => write!(f, "'{}'", val),
			StatementElement::Bool(val) => write!(f, "{}", val),
			StatementElement::Array(arr) => {
				write!(f, "[")?;
				for item in arr.iter() {
					write!(f, "{}, ", item)?;
				}
				write!(f, "]")
			}
			StatementElement::Deref(val) => write!(f, "(*{})", val),
			StatementElement::AdrOf(val) => write!(f, "(&{})", val),
			StatementElement::FieldPointerAccess(ptr, field) => write!(f, "({}->{})", ptr, field),
		}
	}
}

fn do_binary_operation<'a>(
	tokens: &mut Vec<MaybeParsed<'a>>,
	op_from: &MaybeParsed,
	op_to: fn(lhs: StatementElement<'a>, rhs: StatementElement<'a>) -> Result<StatementElement<'a>>,
) -> Result<()> {
	while let Some(idx) = tokens.iter().position(|t| t == op_from) {
		if idx == 0 || idx + 1 == tokens.len() {
			return Err(error!(MisplacedOperators, tokens.as_slice()));
		}
		let right = tokens.remove(idx + 1);
		let left = tokens.remove(idx - 1);
		if let (Parsed(lhs), Parsed(rhs)) = (left, right) {
			//The removal of the left item offset the index by one
			tokens[idx - 1] = Parsed(op_to(lhs, rhs)?);
		} else {
			return Err(error!(TreeConstructionFail, tokens.as_slice()));
		}
	}
	Ok(())
}

fn do_ternary_op<'a>(tokens: &mut Vec<MaybeParsed<'a>>) -> Result<()> {
	let mut idx = tokens.len().wrapping_sub(1);
	while idx != usize::MAX {
		if let Some(Unparsed(StatementToken::Ternary(lhs))) = tokens.get(idx) {
			let lhs = StatementElement::from_statement_tokens(lhs.clone())?;
			let right = tokens.remove(idx + 1);
			let cond = tokens.remove(idx - 1);
			if let (Parsed(cond), Parsed(rhs)) = (cond, right) {
				//The removal of the left item offset the index by one
				tokens[idx - 1] = Parsed(StatementElement::Ternary {
					cond: Box::new(cond),
					lhs: Box::new(lhs),
					rhs: Box::new(rhs),
				});
			} else {
				return Err(error!(MalformedTernary, tokens.as_slice()));
			}
		}
		idx = idx.wrapping_sub(1);
	}
	Ok(())
}

/// Left as in [OP] [TARGET]
fn do_unary_operation_left<'a>(
	tokens: &mut Vec<MaybeParsed<'a>>,
	op_from: &MaybeParsed,
	op_to: fn(lhs: StatementElement<'a>) -> Result<StatementElement<'a>>,
) -> Result<()> {
	let mut idx = tokens.len().wrapping_sub(1);
	while idx != usize::MAX {
		if !matches!(tokens.get(idx), Some(token) if token == op_from) {
			idx = idx.wrapping_sub(1);
			continue;
		}
		let prev_is_op = tokens
			.get(idx.wrapping_sub(1))
			.map(
				|token| token.map_unparsed(StatementToken::is_op).unwrap_or(false), //Parsed is false
			)
			.unwrap_or(true); //Non existant is true
		if prev_is_op {
			let next = tokens.remove(idx + 1);
			if let Parsed(right) = next {
				tokens[idx] = Parsed(op_to(right)?);
			} else {
				return Err(error!(TreeConstructionFail, tokens.as_slice()));
			}
		}
		idx = idx.wrapping_sub(1);
	}
	Ok(())
}

/// Left as in [OP] [TARGET]
fn do_cast<'a>(tokens: &mut Vec<MaybeParsed<'a>>) -> Result<()> {
	let mut idx = tokens.len().wrapping_sub(1);
	while idx != usize::MAX {
		let typ = match tokens.get(idx) {
			Some(MaybeParsed::Unparsed(StatementToken::Cast(t))) => t.clone(),
			_ => {
				idx = idx.wrapping_sub(1);
				continue;
			}
		};
		let prev_is_op = tokens
			.get(idx.wrapping_sub(1))
			.map(
				|token| token.map_unparsed(StatementToken::is_op).unwrap_or(false), //Parsed is false
			)
			.unwrap_or(true); //Non existant is true
		if prev_is_op {
			let next = tokens.remove(idx + 1);
			if let Parsed(value) = next {
				tokens[idx] = Parsed(StatementElement::Cast {
					typ,
					value: Box::new(value),
				});
			} else {
				return Err(error!(TreeConstructionFail, tokens.as_slice()));
			}
		}
		idx = idx.wrapping_sub(1);
	}
	Ok(())
}

fn do_array_access<'a>(tokens: &mut Vec<MaybeParsed<'a>>) -> Result<()> {
	let mut idx = tokens.len().wrapping_sub(1);
	//yes, stop once too early
	while idx < tokens.len() {
		if let Some(Unparsed(StatementToken::ArrayAccess(i))) = tokens.get(idx) {
			let i = StatementElement::from_statement_tokens(i.clone())?;
			let prev = tokens.remove(idx - 1);
			if let Parsed(right) = prev {
				tokens[idx - 1] =
					Parsed(StatementElement::Deref(Box::new(StatementElement::Add {
						lhs: Box::new(right),
						rhs: Box::new(i),
					})));
			} else {
				return Err(error!(TreeConstructionFail, tokens.as_slice()));
			}
		}
		idx = idx.wrapping_sub(1);
	}
	Ok(())
}

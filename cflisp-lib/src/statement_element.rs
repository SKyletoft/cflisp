use crate::*;
use std::borrow::Cow;

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
	Num(isize),
	Char(char),
	Bool(bool),
	Array(Vec<StatementElement<'a>>),
	Deref(Box<StatementElement<'a>>),
	AdrOf(Cow<'a, str>),
	FieldPointerAccess(Cow<'a, str>, Cow<'a, str>),
}

///Takes two `StatementElement`s and returns a single `StatementElement`. All lifetimes are the same
type OpFnPtr<'a> = fn(
	lhs: StatementElement<'a>,
	rhs: StatementElement<'a>,
) -> Result<StatementElement<'a>, ParseError>;

///Takes one `StatementElement`s and returns a single `StatementElement`. All lifetimes are the same
type UnOpFnPtr<'a> = fn(lhs: StatementElement<'a>) -> Result<StatementElement<'a>, ParseError>;

///Enum type for Work-In-Progress parsing. Either a parsed StatementElement or a unparsed *single* token.
/// Both types are exported.
#[derive(Debug, Clone, PartialEq)]
enum MaybeParsed<'a> {
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
	fn from_token(token: StatementToken<'a>) -> Result<MaybeParsed<'a>, ParseError> {
		let res = match token {
			StatementToken::Bool(b) => Parsed(StatementElement::Bool(b)),
			StatementToken::Char(c) => Parsed(StatementElement::Char(c)),
			StatementToken::Num(n) => Parsed(StatementElement::Num(n)),
			StatementToken::Var(v) => Parsed(StatementElement::Var(Cow::Borrowed(v))),

			StatementToken::FunctionCall(name, ts) => {
				let parametres = ts
					.into_iter()
					.map(StatementElement::from_statement_tokens)
					.collect::<Result<Vec<_>, _>>()?;
				Parsed(StatementElement::FunctionCall {
					name: Cow::Borrowed(name),
					parametres,
				})
			}

			StatementToken::Array(arr) => {
				let elements = arr
					.into_iter()
					.map(StatementElement::from_statement_tokens)
					.collect::<Result<Vec<_>, _>>()?;
				Parsed(StatementElement::Array(elements))
			}

			t => Unparsed(t),
		};
		Ok(res)
	}

	//Cannot be implemented as the FromStr trait as that trait doesn't allow a lifetime on the string
	pub(crate) fn from_source_str(s: &'a str) -> Result<StatementElement<'a>, ParseError> {
		let tokens = Token::parse_statement_tokens(s)?;
		StatementElement::from_statement_tokens(tokens)
	}

	pub(crate) fn from_tokens(tokens: &[Token<'a>]) -> Result<StatementElement<'a>, ParseError> {
		let statement_tokens = StatementToken::from_tokens(tokens)?;
		StatementElement::from_statement_tokens(statement_tokens)
	}

	pub(crate) fn from_statement_tokens(
		tokens: Vec<StatementToken<'a>>,
	) -> Result<StatementElement<'a>, ParseError> {
		let mut working_tokens: Vec<MaybeParsed<'a>> = tokens
			.into_iter()
			.map(StatementElement::from_token)
			.collect::<Result<_, _>>()?;

		for token in working_tokens.iter_mut() {
			if let Unparsed(StatementToken::Parentheses(p)) = token {
				let next = StatementElement::from_statement_tokens(p.clone())?;
				*token = Parsed(next);
			}
		}

		let ptr_ops: [(MaybeParsed, OpFnPtr); 2] = [
			(Unparsed(StatementToken::FieldAccess), |l, r| {
				if let (StatementElement::Var(lhs), StatementElement::Var(rhs)) = (l, r) {
					Ok(StatementElement::Var(helper::merge_name_and_field(
						&lhs, &rhs,
					)))
				} else {
					Err(ParseError(line!(), "Field access between non-names"))
				}
			}),
			(Unparsed(StatementToken::FieldPointerAccess), |l, r| {
				if let (StatementElement::Var(lhs), StatementElement::Var(rhs)) = (l, r) {
					Ok(StatementElement::FieldPointerAccess(lhs, rhs))
				} else {
					Err(ParseError(line!(), "Field access between non-names"))
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
					lhs: Box::new(StatementElement::Num(0)),
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
					Err(ParseError(
						line!(),
						"Internal error: Tried to take address of on a non variable",
					))
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
		for (from, to) in un_ops.iter() {
			do_unary_operation_left(&mut working_tokens, from, *to)?;
		}
		for (from, to) in bin_ops.iter() {
			do_binary_operation(&mut working_tokens, from, *to)?;
		}

		do_ternary_op(&mut working_tokens)?;

		if working_tokens.len() != 1 {
			dbg!(working_tokens);
			return Err(ParseError(line!(), "Internal tree construction error"));
		}
		if let Parsed(elem) = working_tokens.remove(0) {
			Ok(elem)
		} else {
			dbg!(working_tokens);
			Err(ParseError(
				line!(),
				"Internal error: Last element in statement parsing vector was unparsed",
			))
		}
	}
}

fn do_binary_operation<'a>(
	tokens: &mut Vec<MaybeParsed<'a>>,
	op_from: &MaybeParsed,
	op_to: fn(
		lhs: StatementElement<'a>,
		rhs: StatementElement<'a>,
	) -> Result<StatementElement<'a>, ParseError>,
) -> Result<(), ParseError> {
	while let Some(idx) = tokens.iter().position(|t| t == op_from) {
		if idx == 0 || idx + 1 == tokens.len() {
			dbg!(tokens);
			return Err(ParseError(
				line!(),
				"Couldn't construct tree from statement. Are you sure the operators are correctly placed?",
			));
		}
		let right = tokens.remove(idx + 1);
		let left = tokens.remove(idx - 1);
		if let (Parsed(lhs), Parsed(rhs)) = (left, right) {
			//The removal of the left item offset the index by one
			tokens[idx - 1] = Parsed(op_to(lhs, rhs)?);
		} else {
			return Err(ParseError(
				line!(),
				"Couldn't construct tree from statement. Element that \
				should've been parsed first has not been parsed",
			));
		}
	}
	Ok(())
}

fn do_ternary_op(tokens: &mut Vec<MaybeParsed>) -> Result<(), ParseError> {
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
				return Err(ParseError(
					line!(),
					"Couldn't construct tree from statement. Element that \
					should've been parsed first has not been parsed",
				));
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
	op_to: fn(lhs: StatementElement<'a>) -> Result<StatementElement<'a>, ParseError>,
) -> Result<(), ParseError> {
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
				return Err(ParseError(
					line!(),
					"Couldn't construct tree from statement. Element that \
					should've been parsed first has not been parsed",
				));
			}
		}
		idx = idx.wrapping_sub(1);
	}
	Ok(())
}

fn do_array_access(tokens: &mut Vec<MaybeParsed>) -> Result<(), ParseError> {
	let mut idx = tokens.len().wrapping_sub(1);
	//yes, stop once too early
	while idx >= 1 && idx < tokens.len() {
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
				return Err(ParseError(
					line!(),
					"Couldn't construct tree from statement. Element that \
				should've been parsed first has not been parsed",
				));
			}
		} else {
			idx = idx.wrapping_sub(1);
		}
	}
	Ok(())
}

pub(crate) fn move_declarations_first(block: &mut Block) {
	let give_value = |element: &LanguageElement| -> usize {
		match element {
			LanguageElement::StructDefinition { .. } => 0,
			LanguageElement::StructDeclarationAssignment {
				is_static: true, ..
			} => 1,
			LanguageElement::VariableDeclaration {
				is_static: true, ..
			} => 1,

			LanguageElement::VariableDeclarationAssignment {
				is_static: true, ..
			} => 1,

			LanguageElement::StructDeclarationAssignment {
				is_static: false, ..
			} => 1,

			LanguageElement::VariableDeclaration {
				is_static: false, ..
			} => 2,

			LanguageElement::VariableDeclarationAssignment {
				is_static: false, ..
			} => 2,

			LanguageElement::FunctionDeclaration { .. } => 3,
			_ => 4,
		}
	};
	block.sort_by_key(give_value);
}

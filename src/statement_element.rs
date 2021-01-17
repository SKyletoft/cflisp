use crate::*;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum StatementElement<'a> {
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
	And {
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	Or {
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	Xor {
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	Not {
		lhs: Box<StatementElement<'a>>,
	},
	GT {
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	LT {
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	Cmp {
		lhs: Box<StatementElement<'a>>,
		rhs: Box<StatementElement<'a>>,
	},
	FunctionCall {
		name: &'a str,
		parametres: Vec<StatementElement<'a>>,
	},
	Var(&'a str),
	Num(isize),
	Char(char),
	Bool(bool),
	Array(Vec<StatementElement<'a>>),
	Deref(Box<StatementElement<'a>>),
}

type OpFnPtr<'a> = fn(lhs: StatementElement<'a>, rhs: StatementElement<'a>) -> StatementElement<'a>;
#[derive(Debug, Clone, PartialEq)]
enum MaybeParsed<'a> {
	Parsed(StatementElement<'a>),
	Unparsed(StatementToken<'a>),
}
use MaybeParsed::*;

impl<'a> StatementElement<'a> {
	pub(crate) fn from_tokens(
		tokens: Vec<StatementToken<'a>>,
	) -> Result<StatementElement<'a>, ParseError> {
		dbg!(&tokens);
		let mut working_tokens = Vec::new();
		for token in tokens.into_iter() {
			working_tokens.push(match token {
				StatementToken::Bool(b) => Parsed(StatementElement::Bool(b)),
				StatementToken::Char(c) => Parsed(StatementElement::Char(c)),
				StatementToken::Num(n) => Parsed(StatementElement::Num(n)),
				StatementToken::Var(v) => Parsed(StatementElement::Var(v)),
				StatementToken::FunctionCall(name, ts) => {
					let parametres = ts
						.into_iter()
						.map(StatementElement::from_tokens)
						.collect::<Result<Vec<_>, _>>()?;
					Parsed(StatementElement::FunctionCall { name, parametres })
				}
				StatementToken::Array(arr) => {
					let elements = arr
						.into_iter()
						.map(StatementElement::from_tokens)
						.collect::<Result<Vec<_>, _>>()?;
					Parsed(StatementElement::Array(elements))
				}
				StatementToken::ArrayAccess { ptr, idx } => {
					Parsed(StatementElement::Deref(Box::new(StatementElement::Add {
						lhs: Box::new(StatementElement::Var(ptr)),
						rhs: Box::new(StatementElement::from_tokens(idx)?),
					})))
				}
				t => Unparsed(t),
			});
		}
		dbg!(&working_tokens);
		let operations: [(MaybeParsed, OpFnPtr); 13] = [
			(Unparsed(StatementToken::Mul), |l, r| {
				StatementElement::Mul {
					lhs: Box::new(l),
					rhs: Box::new(r),
				}
			}),
			(Unparsed(StatementToken::Div), |l, r| {
				StatementElement::Div {
					lhs: Box::new(l),
					rhs: Box::new(r),
				}
			}),
			(Unparsed(StatementToken::Mod), |l, r| {
				StatementElement::Mod {
					lhs: Box::new(l),
					rhs: Box::new(r),
				}
			}),
			(Unparsed(StatementToken::Add), |l, r| {
				StatementElement::Add {
					lhs: Box::new(l),
					rhs: Box::new(r),
				}
			}),
			(Unparsed(StatementToken::Sub), |l, r| {
				StatementElement::Sub {
					lhs: Box::new(l),
					rhs: Box::new(r),
				}
			}),
			(Unparsed(StatementToken::LShift), |l, r| {
				StatementElement::LShift {
					lhs: Box::new(l),
					rhs: Box::new(r),
				}
			}),
			(Unparsed(StatementToken::RShift), |l, r| {
				StatementElement::RShift {
					lhs: Box::new(l),
					rhs: Box::new(r),
				}
			}),
			(Unparsed(StatementToken::LT), |l, r| StatementElement::LT {
				lhs: Box::new(l),
				rhs: Box::new(r),
			}),
			(Unparsed(StatementToken::GT), |l, r| StatementElement::GT {
				lhs: Box::new(l),
				rhs: Box::new(r),
			}),
			(Unparsed(StatementToken::Cmp), |l, r| {
				StatementElement::Cmp {
					lhs: Box::new(l),
					rhs: Box::new(r),
				}
			}),
			(Unparsed(StatementToken::And), |l, r| {
				StatementElement::And {
					lhs: Box::new(l),
					rhs: Box::new(r),
				}
			}),
			(Unparsed(StatementToken::Xor), |l, r| {
				StatementElement::Xor {
					lhs: Box::new(l),
					rhs: Box::new(r),
				}
			}),
			(Unparsed(StatementToken::Or), |l, r| StatementElement::Or {
				lhs: Box::new(l),
				rhs: Box::new(r),
			}),
		];
		for token in working_tokens.iter_mut() {
			if let Unparsed(StatementToken::Parentheses(p)) = token {
				let next = StatementElement::from_tokens(p.clone())?;
				*token = Parsed(next);
			}
		}
		while let Some(idx) = working_tokens
			.iter()
			.position(|t| t == &Unparsed(StatementToken::Not))
		{
			if idx + 1 == working_tokens.len() {
				return Err(ParseError(
					line!(),
					"Couldn't construct tree from statement. Are you sure the operators are correctly placed?",
				));
			}
			let next = working_tokens.remove(idx + 1);
			let notted = match next {
				Unparsed(t) => StatementElement::from_tokens(vec![t])?,
				Parsed(t) => t,
			};
			working_tokens[idx] = Parsed(StatementElement::Not {
				lhs: Box::new(notted),
			});
		}
		for (from, to) in operations.iter() {
			do_operation(&mut working_tokens, from, *to)?;
		}
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

fn do_operation<'a>(
	tokens: &mut Vec<MaybeParsed<'a>>,
	op_from: &MaybeParsed,
	op_to: fn(lhs: StatementElement<'a>, rhs: StatementElement<'a>) -> StatementElement<'a>,
) -> Result<(), ParseError> {
	while let Some(idx) = tokens.iter().position(|t| t == op_from) {
		if idx == 0 || idx + 1 == tokens.len() {
			return Err(ParseError(
			line!(),
			"Couldn't construct tree from statement. Are you sure the operators are correctly placed?",
		));
		}
		let right = tokens.remove(idx + 1);
		let left = tokens.remove(idx - 1);
		if let (Parsed(lhs), Parsed(rhs)) = (right, left) {
			//The removal of the left item offset the index by one
			tokens[idx - 1] = Parsed(op_to(lhs, rhs));
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

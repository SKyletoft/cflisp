use crate::*;
use flisp_instructions::{Addressing, Instruction};
use types::{Block, Function, Type};

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
	AdrOf(&'a str),
}

type OpFnPtr<'a> = fn(lhs: StatementElement<'a>, rhs: StatementElement<'a>) -> StatementElement<'a>;

#[derive(Debug, Clone, PartialEq)]
enum MaybeParsed<'a> {
	Parsed(StatementElement<'a>),
	Unparsed(StatementToken<'a>),
}
use MaybeParsed::*;

impl<'a> StatementElement<'a> {
	pub(crate) fn as_flisp_instruction(&self, adr: Addressing) -> Instruction {
		match self {
			StatementElement::Add { lhs: _, rhs: _ } => Instruction::ADDA(adr),
			StatementElement::Sub { lhs: _, rhs: _ } => Instruction::SUBA(adr),
			StatementElement::Mul { lhs: _, rhs: _ } => unimplemented!(),
			StatementElement::Div { lhs: _, rhs: _ } => unimplemented!(),
			StatementElement::Mod { lhs: _, rhs: _ } => unimplemented!(),
			StatementElement::LShift { lhs: _, rhs: _ } => unimplemented!(),
			StatementElement::RShift { lhs: _, rhs: _ } => unimplemented!(),
			StatementElement::And { lhs: _, rhs: _ } => Instruction::ANDA(adr),
			StatementElement::Or { lhs: _, rhs: _ } => Instruction::ORA(adr),
			StatementElement::Xor { lhs: _, rhs: _ } => Instruction::EORA(adr),
			StatementElement::Not { lhs: _ } => Instruction::COMA,
			StatementElement::GT { lhs: _, rhs: _ } => unimplemented!(),
			StatementElement::LT { lhs: _, rhs: _ } => unimplemented!(),
			StatementElement::Cmp { lhs: _, rhs: _ } => Instruction::SUBA(adr),
			StatementElement::FunctionCall {
				name: _,
				parametres: _,
			} => unimplemented!(),
			StatementElement::Var(_) => unimplemented!(),
			StatementElement::Num(_) => unimplemented!(),
			StatementElement::Char(_) => unimplemented!(),
			StatementElement::Bool(_) => unimplemented!(),
			StatementElement::Array(_) => unimplemented!(),
			StatementElement::Deref(_) => unimplemented!(),
			StatementElement::AdrOf(_) => unimplemented!(),
		}
	}

	pub(crate) fn depth(&self) -> usize {
		match self {
			StatementElement::Add { lhs, rhs } => lhs.as_ref().depth().max(rhs.as_ref().depth()),
			StatementElement::Sub { lhs, rhs } => lhs.as_ref().depth().max(rhs.as_ref().depth()),
			StatementElement::Mul { lhs, rhs } => lhs.as_ref().depth().max(rhs.as_ref().depth()),
			StatementElement::Div { lhs, rhs } => lhs.as_ref().depth().max(rhs.as_ref().depth()),
			StatementElement::Mod { lhs, rhs } => lhs.as_ref().depth().max(rhs.as_ref().depth()),
			StatementElement::LShift { lhs, rhs } => lhs.as_ref().depth().max(rhs.as_ref().depth()),
			StatementElement::RShift { lhs, rhs } => lhs.as_ref().depth().max(rhs.as_ref().depth()),
			StatementElement::And { lhs, rhs } => lhs.as_ref().depth().max(rhs.as_ref().depth()),
			StatementElement::Or { lhs, rhs } => lhs.as_ref().depth().max(rhs.as_ref().depth()),
			StatementElement::Xor { lhs, rhs } => lhs.as_ref().depth().max(rhs.as_ref().depth()),
			StatementElement::GT { lhs, rhs } => lhs.as_ref().depth().max(rhs.as_ref().depth()),
			StatementElement::LT { lhs, rhs } => lhs.as_ref().depth().max(rhs.as_ref().depth()),
			StatementElement::Cmp { lhs, rhs } => lhs.as_ref().depth().max(rhs.as_ref().depth()),
			StatementElement::Not { lhs } => lhs.as_ref().depth(),
			StatementElement::FunctionCall {
				name: _,
				parametres,
			} => parametres.iter().map(StatementElement::depth).sum(),
			StatementElement::Var(_) => 1,
			StatementElement::Num(_) => 1,
			StatementElement::Char(_) => 1,
			StatementElement::Bool(_) => 1,
			StatementElement::Array(_) => 1,
			StatementElement::Deref(_) => 1,
			StatementElement::AdrOf(_) => 1,
		}
	}

	fn from_token(token: StatementToken<'a>) -> Result<MaybeParsed<'a>, ParseError> {
		let res = match token {
			StatementToken::Bool(b) => Parsed(StatementElement::Bool(b)),
			StatementToken::Char(c) => Parsed(StatementElement::Char(c)),
			StatementToken::Num(n) => Parsed(StatementElement::Num(n)),
			StatementToken::Var(v) => Parsed(StatementElement::Var(v)),
			StatementToken::AdrOf(n) => Parsed(StatementElement::AdrOf(n)),
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
			StatementToken::Deref(ptr) => Parsed(StatementElement::Deref(Box::new(
				StatementElement::from_tokens(ptr)?,
			))),
			t => Unparsed(t),
		};
		Ok(res)
	}

	pub(crate) fn from_tokens(
		tokens: Vec<StatementToken<'a>>,
	) -> Result<StatementElement<'a>, ParseError> {
		let mut working_tokens: Vec<MaybeParsed<'a>> = tokens
			.into_iter()
			.map(StatementElement::from_token)
			.collect::<Result<_, _>>()?;

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

	fn type_of(
		&self,
		functions: &'a [Function<'a>],
		variables: &'a [Variable<'a>],
	) -> Result<Type, ParseError> {
		let res = match self {
			StatementElement::Num(_) => Type::Int,
			StatementElement::Char(_) => Type::Char,
			StatementElement::Bool(_) => Type::Bool,
			StatementElement::Add { lhs: _, rhs: _ } => Type::Int,
			StatementElement::Sub { lhs: _, rhs: _ } => Type::Int,
			StatementElement::Mul { lhs: _, rhs: _ } => Type::Int,
			StatementElement::Div { lhs: _, rhs: _ } => Type::Int,
			StatementElement::Mod { lhs: _, rhs: _ } => Type::Int,
			StatementElement::LShift { lhs: _, rhs: _ } => Type::Int,
			StatementElement::RShift { lhs: _, rhs: _ } => Type::Int,
			StatementElement::And { lhs: _, rhs: _ } => Type::Bool,
			StatementElement::Or { lhs: _, rhs: _ } => Type::Bool,
			StatementElement::Xor { lhs: _, rhs: _ } => Type::Bool,
			StatementElement::Not { lhs: _ } => Type::Bool,
			StatementElement::GT { lhs: _, rhs: _ } => Type::Bool,
			StatementElement::LT { lhs: _, rhs: _ } => Type::Bool,
			StatementElement::Cmp { lhs: _, rhs: _ } => Type::Bool,
			StatementElement::FunctionCall {
				name,
				parametres: _,
			} => functions
				.iter()
				.find(|f| &f.name == name)
				.map(|f| f.return_type.clone())
				.ok_or(ParseError(line!(), "Cannot resolve function!"))?,
			StatementElement::Var(name) => variables
				.iter()
				.find(|f| &f.name == name)
				.map(|v| v.typ.clone())
				.ok_or(ParseError(line!(), "Cannot resolve variable!"))?,
			StatementElement::Array(arr) => Type::Ptr(Box::new(
				arr.get(0)
					.map(|s| s.type_of(functions, variables))
					.unwrap_or(Ok(Type::Void))?,
			)),
			StatementElement::Deref(t) => t.as_ref().type_of(functions, variables)?,
			StatementElement::AdrOf(name) => Type::Ptr(Box::new(
				variables
					.iter()
					.find(|f| &f.name == name)
					.map(|v| v.typ.clone())
					.ok_or(ParseError(line!(), "Cannot resolve variable!"))?,
			)),
		};
		Ok(res)
	}

	pub(crate) fn type_check(
		&self,
		variables: &[Variable],
		functions: &[Function],
	) -> Result<bool, ParseError> {
		let res = match self {
			StatementElement::FunctionCall { name, parametres } => {
				if let Some(f) = functions.iter().find(|f| &f.name == name) {
					let len_eq = f.parametres.len() == parametres.len();
					let types_are_eq = {
						for (l, r) in f
							.parametres
							.iter()
							.map(|v| &v.typ)
							.zip(parametres.iter().map(|p| p.type_of(functions, variables)))
						{
							if l != &r? {
								return Ok(false);
							}
						}
						true
					};
					len_eq && types_are_eq
				} else {
					false
				}
			}
			StatementElement::Add { lhs, rhs } => {
				lhs.as_ref().type_check(variables, functions)?
					&& rhs.as_ref().type_check(variables, functions)?
			}
			StatementElement::Sub { lhs, rhs } => {
				lhs.as_ref().type_check(variables, functions)?
					&& rhs.as_ref().type_check(variables, functions)?
			}
			StatementElement::Mul { lhs, rhs } => {
				lhs.as_ref().type_check(variables, functions)?
					&& rhs.as_ref().type_check(variables, functions)?
			}
			StatementElement::Div { lhs, rhs } => {
				lhs.as_ref().type_check(variables, functions)?
					&& rhs.as_ref().type_check(variables, functions)?
			}
			StatementElement::Mod { lhs, rhs } => {
				lhs.as_ref().type_check(variables, functions)?
					&& rhs.as_ref().type_check(variables, functions)?
			}
			StatementElement::LShift { lhs, rhs } => {
				lhs.as_ref().type_check(variables, functions)?
					&& rhs.as_ref().type_check(variables, functions)?
			}
			StatementElement::RShift { lhs, rhs } => {
				lhs.as_ref().type_check(variables, functions)?
					&& rhs.as_ref().type_check(variables, functions)?
			}
			StatementElement::And { lhs, rhs } => {
				lhs.as_ref().type_check(variables, functions)?
					&& rhs.as_ref().type_check(variables, functions)?
			}
			StatementElement::Or { lhs, rhs } => {
				lhs.as_ref().type_check(variables, functions)?
					&& rhs.as_ref().type_check(variables, functions)?
			}
			StatementElement::Xor { lhs, rhs } => {
				lhs.as_ref().type_check(variables, functions)?
					&& rhs.as_ref().type_check(variables, functions)?
			}
			StatementElement::GT { lhs, rhs } => {
				lhs.as_ref().type_check(variables, functions)?
					&& rhs.as_ref().type_check(variables, functions)?
			}
			StatementElement::LT { lhs, rhs } => {
				lhs.as_ref().type_check(variables, functions)?
					&& rhs.as_ref().type_check(variables, functions)?
			}
			StatementElement::Cmp { lhs, rhs } => {
				lhs.as_ref().type_check(variables, functions)?
					&& rhs.as_ref().type_check(variables, functions)?
			}
			StatementElement::Not { lhs } => lhs.as_ref().type_check(variables, functions)?,
			_ => true,
		};
		Ok(res)
	}
}

fn do_operation<'a>(
	tokens: &mut Vec<MaybeParsed<'a>>,
	op_from: &MaybeParsed,
	op_to: fn(lhs: StatementElement<'a>, rhs: StatementElement<'a>) -> StatementElement<'a>,
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

pub(crate) fn move_declarations_first(block: &mut Block) {
	let give_value = |element: &LanguageElement| -> usize {
		match element {
			LanguageElement::VariableDeclaration { typ: _, name: _ } => 0,
			LanguageElement::VariableDeclarationAssignment {
				typ: _,
				name: _,
				value: _,
			} => 0,
			LanguageElement::FunctionDeclaration {
				typ: _,
				name: _,
				args: _,
				block: _,
			} => 0,
			_ => 1,
		}
	};
	block.sort_by_key(give_value);
}

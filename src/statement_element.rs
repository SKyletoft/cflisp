use crate::*;
use std::borrow::Cow;

///Tree structure to represent a statement. Boolean and bitwise logic are combined
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
	FunctionCall {
		name: Cow<'a, str>,
		parametres: Vec<StatementElement<'a>>,
	},
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
	///Returns the corresponding instruction for the root element of the tree. Ignores branches.
	pub(crate) fn as_flisp_instruction(
		&self,
		adr: Addressing,
	) -> Result<Instruction, CompileError> {
		let res = match self {
			StatementElement::Add { lhs: _, rhs: _ } => Instruction::ADDA(adr),
			StatementElement::Sub { lhs: _, rhs: _ } => Instruction::SUBA(adr),
			StatementElement::Mul { lhs: _, rhs: _ }
			| StatementElement::Div { lhs: _, rhs: _ }
			| StatementElement::Mod { lhs: _, rhs: _ } => {
				return Err(CompileError(
					line!(),
					"Internal error: function call, not instruction?",
				));
			}
			StatementElement::LShift { lhs: _, rhs: _ } => Instruction::LSLA,
			StatementElement::RShift { lhs: _, rhs: _ } => Instruction::LSRA,
			StatementElement::And { lhs: _, rhs: _ } => Instruction::ANDA(adr),
			StatementElement::Or { lhs: _, rhs: _ } => Instruction::ORA(adr),
			StatementElement::Xor { lhs: _, rhs: _ } => Instruction::EORA(adr),
			StatementElement::Not { lhs: _ } => Instruction::COMA,
			StatementElement::GreaterThan { lhs: _, rhs: _ } => Instruction::SUBA(adr),
			StatementElement::LessThan { lhs: _, rhs: _ } => Instruction::SUBA(adr),
			StatementElement::GreaterThanEqual { lhs: _, rhs: _ } => Instruction::SUBA(adr),
			StatementElement::LessThanEqual { lhs: _, rhs: _ } => Instruction::SUBA(adr),
			StatementElement::Cmp { lhs: _, rhs: _ } => Instruction::SUBA(adr),
			StatementElement::NotCmp { lhs: _, rhs: _ } => Instruction::SUBA(adr),
			StatementElement::FieldPointerAccess(_, _)
			| StatementElement::Var(_)
			| StatementElement::Num(_)
			| StatementElement::Char(_)
			| StatementElement::Bool(_)
			| StatementElement::Array(_)
			| StatementElement::Deref(_)
			| StatementElement::AdrOf(_)
			| StatementElement::FunctionCall {
				name: _,
				parametres: _,
			} => {
				return Err(CompileError(
					line!(),
					"Internal error: special cases and literals, not instructions?",
				))
			}
		};

		Ok(res)
	}

	pub(crate) fn depth(&self) -> usize {
		let rest = match self {
			StatementElement::Add { lhs, rhs }
			| StatementElement::Sub { lhs, rhs }
			| StatementElement::Mul { lhs, rhs }
			| StatementElement::Div { lhs, rhs }
			| StatementElement::Mod { lhs, rhs }
			| StatementElement::LShift { lhs, rhs }
			| StatementElement::RShift { lhs, rhs }
			| StatementElement::And { lhs, rhs }
			| StatementElement::Or { lhs, rhs }
			| StatementElement::Xor { lhs, rhs }
			| StatementElement::GreaterThan { lhs, rhs }
			| StatementElement::LessThan { lhs, rhs }
			| StatementElement::GreaterThanEqual { lhs, rhs }
			| StatementElement::LessThanEqual { lhs, rhs }
			| StatementElement::Cmp { lhs, rhs }
			| StatementElement::NotCmp { lhs, rhs } => lhs.as_ref().depth().max(rhs.as_ref().depth()),
			StatementElement::Not { lhs } => lhs.as_ref().depth(),
			StatementElement::Array(n) => n.iter().map(|e| e.depth()).max().unwrap_or(0),
			StatementElement::Deref(n) => n.as_ref().depth(),
			StatementElement::Var(_)
			| StatementElement::Num(_)
			| StatementElement::Char(_)
			| StatementElement::Bool(_)
			| StatementElement::AdrOf(_)
			| StatementElement::FieldPointerAccess(_, _) => 0,
			StatementElement::FunctionCall {
				name: _,
				parametres: _,
			} => 1, //Each parametre is its own memory alloc but can still require 1 if the function call is on the rhs
		};
		rest + 1
	}

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

			StatementToken::ArrayAccess { ptr, idx } => {
				Parsed(StatementElement::Deref(Box::new(StatementElement::Add {
					lhs: Box::new(StatementElement::Var(Cow::Borrowed(ptr))),
					rhs: Box::new(StatementElement::from_statement_tokens(idx)?),
				})))
			}

			t => Unparsed(t),
		};
		Ok(res)
	}

	pub(crate) fn from_str(s: &'a str) -> Result<StatementElement<'a>, ParseError> {
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
					Ok(StatementElement::Var(Cow::Owned(
						lhs.to_string() + "::" + &rhs,
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

		let un_ops: [(MaybeParsed, UnOpFnPtr); 4] = [
			(Unparsed(StatementToken::BitNot), |l| {
				Ok(StatementElement::Not { lhs: Box::new(l) })
			}),
			(Unparsed(StatementToken::BoolNot), |l| {
				Ok(StatementElement::Not { lhs: Box::new(l) })
			}),
			(Unparsed(StatementToken::Mul), |l| {
				Ok(StatementElement::Deref(Box::new(l)))
			}),
			(Unparsed(StatementToken::BitAnd), |l| {
				if let StatementElement::Var(n) = l {
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
			(Unparsed(StatementToken::Mul), |l, r| {
				Ok(StatementElement::Mul {
					lhs: Box::new(l),
					rhs: Box::new(r),
				})
			}),
			(Unparsed(StatementToken::Div), |l, r| {
				Ok(StatementElement::Div {
					lhs: Box::new(l),
					rhs: Box::new(r),
				})
			}),
			(Unparsed(StatementToken::Mod), |l, r| {
				Ok(StatementElement::Mod {
					lhs: Box::new(l),
					rhs: Box::new(r),
				})
			}),
			(Unparsed(StatementToken::Add), |l, r| {
				Ok(StatementElement::Add {
					lhs: Box::new(l),
					rhs: Box::new(r),
				})
			}),
			(Unparsed(StatementToken::Sub), |l, r| {
				Ok(StatementElement::Sub {
					lhs: Box::new(l),
					rhs: Box::new(r),
				})
			}),
			(Unparsed(StatementToken::LShift), |l, r| {
				Ok(StatementElement::LShift {
					lhs: Box::new(l),
					rhs: Box::new(r),
				})
			}),
			(Unparsed(StatementToken::RShift), |l, r| {
				Ok(StatementElement::RShift {
					lhs: Box::new(l),
					rhs: Box::new(r),
				})
			}),
			(Unparsed(StatementToken::LessThan), |l, r| {
				Ok(StatementElement::LessThan {
					lhs: Box::new(l),
					rhs: Box::new(r),
				})
			}),
			(Unparsed(StatementToken::LessThanEqual), |l, r| {
				Ok(StatementElement::LessThanEqual {
					lhs: Box::new(l),
					rhs: Box::new(r),
				})
			}),
			(Unparsed(StatementToken::GreaterThan), |l, r| {
				Ok(StatementElement::GreaterThan {
					lhs: Box::new(l),
					rhs: Box::new(r),
				})
			}),
			(Unparsed(StatementToken::GreaterThanEqual), |l, r| {
				Ok(StatementElement::GreaterThanEqual {
					lhs: Box::new(l),
					rhs: Box::new(r),
				})
			}),
			(Unparsed(StatementToken::Cmp), |l, r| {
				Ok(StatementElement::Cmp {
					lhs: Box::new(l),
					rhs: Box::new(r),
				})
			}),
			(Unparsed(StatementToken::NotCmp), |l, r| {
				Ok(StatementElement::NotCmp {
					lhs: Box::new(l),
					rhs: Box::new(r),
				})
			}),
			(Unparsed(StatementToken::BitAnd), |l, r| {
				Ok(StatementElement::And {
					lhs: Box::new(l),
					rhs: Box::new(r),
				})
			}),
			(Unparsed(StatementToken::Xor), |l, r| {
				Ok(StatementElement::Xor {
					lhs: Box::new(l),
					rhs: Box::new(r),
				})
			}),
			(Unparsed(StatementToken::BitOr), |l, r| {
				Ok(StatementElement::Or {
					lhs: Box::new(l),
					rhs: Box::new(r),
				})
			}),
			(Unparsed(StatementToken::BoolAnd), |l, r| {
				Ok(StatementElement::And {
					lhs: Box::new(l),
					rhs: Box::new(r),
				})
			}),
			(Unparsed(StatementToken::BoolOr), |l, r| {
				Ok(StatementElement::Or {
					lhs: Box::new(l),
					rhs: Box::new(r),
				})
			}),
		];

		for (from, to) in ptr_ops.iter() {
			do_binary_operation(&mut working_tokens, from, *to)?;
		}
		for (from, to) in un_ops.iter() {
			do_unary_operation(&mut working_tokens, from, *to)?;
		}
		for (from, to) in bin_ops.iter() {
			do_binary_operation(&mut working_tokens, from, *to)?;
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

	pub(crate) fn type_of(
		&self,
		functions: &'a [Function<'a>],
		variables: &'a [Variable<'a>],
	) -> Result<NativeType, ParseError> {
		let res = match self {
			StatementElement::Num(_) => NativeType::Int,
			StatementElement::Char(_) => NativeType::Char,
			StatementElement::Bool(_) => NativeType::Bool,
			StatementElement::Add { lhs: _, rhs: _ } => NativeType::Int,
			StatementElement::Sub { lhs: _, rhs: _ } => NativeType::Int,
			StatementElement::Mul { lhs: _, rhs: _ } => NativeType::Int,
			StatementElement::Div { lhs: _, rhs: _ } => NativeType::Int,
			StatementElement::Mod { lhs: _, rhs: _ } => NativeType::Int,
			StatementElement::LShift { lhs: _, rhs: _ } => NativeType::Int,
			StatementElement::RShift { lhs: _, rhs: _ } => NativeType::Int,
			StatementElement::And { lhs: _, rhs: _ } => NativeType::Bool,
			StatementElement::Or { lhs: _, rhs: _ } => NativeType::Bool,
			StatementElement::Xor { lhs: _, rhs: _ } => NativeType::Bool,
			StatementElement::Not { lhs: _ } => NativeType::Bool,
			StatementElement::GreaterThan { lhs: _, rhs: _ } => NativeType::Bool,
			StatementElement::LessThan { lhs: _, rhs: _ } => NativeType::Bool,
			StatementElement::GreaterThanEqual { lhs: _, rhs: _ } => NativeType::Bool,
			StatementElement::LessThanEqual { lhs: _, rhs: _ } => NativeType::Bool,
			StatementElement::Cmp { lhs: _, rhs: _ } => NativeType::Bool,
			StatementElement::NotCmp { lhs: _, rhs: _ } => NativeType::Bool,

			StatementElement::FunctionCall {
				name,
				parametres: _,
			} => functions
				.iter()
				.find(|f| f.name == name)
				.map(|f| f.return_type.clone())
				.ok_or(ParseError(line!(), "Cannot resolve function!"))?,

			StatementElement::Var(name) => variables
				.iter()
				.find(|f| f.name == name)
				.map(|v| v.typ.clone())
				.ok_or(ParseError(line!(), "Cannot resolve variable!"))?,

			StatementElement::Array(arr) => NativeType::Ptr(Box::new(
				arr.get(0)
					.map(|s| s.type_of(functions, variables))
					.unwrap_or(Ok(NativeType::Void))?,
			)),

			StatementElement::Deref(t) => t.as_ref().type_of(functions, variables)?,

			StatementElement::AdrOf(name) => NativeType::Ptr(Box::new(
				variables
					.iter()
					.find(|f| f.name == name)
					.map(|v| v.typ.clone())
					.ok_or(ParseError(line!(), "Cannot resolve variable!"))?,
			)),

			_ => todo!(),
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
				if let Some(f) = functions.iter().find(|f| f.name == name) {
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

			StatementElement::Add { lhs, rhs }
			| StatementElement::Sub { lhs, rhs }
			| StatementElement::Mul { lhs, rhs }
			| StatementElement::Div { lhs, rhs }
			| StatementElement::Mod { lhs, rhs }
			| StatementElement::LShift { lhs, rhs }
			| StatementElement::RShift { lhs, rhs }
			| StatementElement::And { lhs, rhs }
			| StatementElement::Or { lhs, rhs }
			| StatementElement::Xor { lhs, rhs }
			| StatementElement::GreaterThan { lhs, rhs }
			| StatementElement::LessThan { lhs, rhs }
			| StatementElement::Cmp { lhs, rhs } => {
				lhs.as_ref().type_check(variables, functions)?
					&& rhs.as_ref().type_check(variables, functions)?
			}

			StatementElement::Not { lhs } => lhs.as_ref().type_check(variables, functions)?,

			_ => true,
		};
		Ok(res)
	}

	pub(crate) fn internal_ref(&self) -> Option<(&StatementElement, &StatementElement)> {
		match self {
			StatementElement::Add { lhs, rhs }
			| StatementElement::Sub { lhs, rhs }
			| StatementElement::Mul { lhs, rhs }
			| StatementElement::Div { lhs, rhs }
			| StatementElement::Mod { lhs, rhs }
			| StatementElement::LShift { lhs, rhs }
			| StatementElement::RShift { lhs, rhs }
			| StatementElement::And { lhs, rhs }
			| StatementElement::Or { lhs, rhs }
			| StatementElement::Xor { lhs, rhs }
			| StatementElement::GreaterThan { lhs, rhs }
			| StatementElement::LessThan { lhs, rhs }
			| StatementElement::GreaterThanEqual { lhs, rhs }
			| StatementElement::LessThanEqual { lhs, rhs }
			| StatementElement::NotCmp { lhs, rhs }
			| StatementElement::Cmp { lhs, rhs } => Some((lhs.as_ref(), rhs.as_ref())),
			StatementElement::Not { lhs: _ }
			| StatementElement::FunctionCall {
				name: _,
				parametres: _,
			}
			| StatementElement::Var(_)
			| StatementElement::Num(_)
			| StatementElement::Char(_)
			| StatementElement::Bool(_)
			| StatementElement::Array(_)
			| StatementElement::Deref(_)
			| StatementElement::AdrOf(_) => None,

			_ => todo!(),
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

fn do_unary_operation<'a>(
	tokens: &mut Vec<MaybeParsed<'a>>,
	op_from: &MaybeParsed,
	op_to: fn(lhs: StatementElement<'a>) -> Result<StatementElement<'a>, ParseError>,
) -> Result<(), ParseError> {
	let mut idx = tokens.len() - 1;
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

pub(crate) fn move_declarations_first(block: &mut Block) {
	let give_value = |element: &LanguageElement| -> usize {
		match element {
			LanguageElement::VariableDeclaration {
				typ: _,
				name: _,
				is_static: true,
			} => 0,

			LanguageElement::VariableDeclarationAssignment {
				typ: _,
				name: _,
				value: _,
				is_static: true,
			} => 0,

			LanguageElement::VariableDeclaration {
				typ: _,
				name: _,
				is_static: false,
			} => 1,

			LanguageElement::VariableDeclarationAssignment {
				typ: _,
				name: _,
				value: _,
				is_static: false,
			} => 1,

			LanguageElement::FunctionDeclaration {
				typ: _,
				name: _,
				args: _,
				block: _,
			} => 2,
			_ => 3,
		}
	};
	block.sort_by_key(give_value);
}

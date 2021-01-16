use crate::*;
use Token::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) struct Variable<'a> {
	typ: Type,
	name: &'a str,
}

///The types that are currently supported by the compiler and their pointer types
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum Type {
	Uint,
	Int,
	Char,
	Bool,
	UintPtr,
	IntPtr,
	CharPtr,
	BoolPtr,
	Void,
	VoidPtr,
	Unknown,
}

///All possible tokens in the source (after comments have been removed)
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Token<'a> {
	Decl(Type),
	If,
	Else,
	Assign,
	AssignId(usize),
	AssignName(&'a str),
	FunctionCall(&'a str, Vec<&'a str>),
	VariableName(&'a str),
	VariableId(usize),
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
	Block(Vec<Statement<'a>>),
	Parens(Vec<Token<'a>>),
	Array,
	StringLit,
	Char(char),
	Bool(bool),
	Apostrophe,
	AdrOf,
	Deref(&'a str),
	Name(&'a str),
	Star,
	Ampersand,
	And,
	Or,
	Return,
	Num(isize),
	Void,
	For,
	While,
	NoArgs,
	Args(Vec<Variable<'a>>),
	UnparsedBlock(&'a str),
	NewLine,
	Xor,
	Not,
}

impl<'a> Token<'a> {
	pub(crate) fn parse_str_to_vec(source: &'a str) -> Result<Vec<Token<'a>>, ParseError> {
		let split = helper::split(source)?;
		let mut tokens = Vec::with_capacity(split.len());
		for slice in split {
			tokens.push(Token::parse(slice)?);
		}
		Ok(tokens)
	}
	pub(crate) fn parse(name: &'a str) -> Result<Self, ParseError> {
		let token = match name {
			";" => NewLine,
			"int" => Decl(Type::Int),
			"bool" => Decl(Type::Int),
			"char" => Decl(Type::Int),
			"uint" => Decl(Type::Int),
			"int*" => Decl(Type::IntPtr),
			"bool*" => Decl(Type::IntPtr),
			"char*" => Decl(Type::IntPtr),
			"uint*" => Decl(Type::IntPtr),
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
			"()" => Args(vec![]),
			n if n.starts_with('&') => Deref(n),
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
				let short = helper::remove_parentheses(s).split(',').collect::<Vec<_>>();
				let mut token_parsed = Vec::with_capacity(short.len());
				for slice in short {
					let as_tokens = Token::parse_str_to_vec(slice)?;
					let as_statement = StatementToken::from_tokens(&as_tokens)?;
					token_parsed.push(as_statement);
				}
				Ok(token_parsed)
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

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum StatementToken<'a> {
	Num(isize),
	Bool(bool),
	Char(char),
	Var(&'a str),
	FunctionCall(&'a str, Vec<Token<'a>>),
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
	LT,
	GT,
	Cmp,
	Not,
	Parentheses(Vec<StatementToken<'a>>),
}

impl<'a> StatementToken<'a> {
	pub(crate) fn from_tokens(tokens: &[Token<'a>]) -> Result<Statement<'a>, ParseError> {
		let mut res = Vec::new();
		for token in tokens {
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
				Token::LT => StatementToken::LT,
				Token::GT => StatementToken::GT,
				Token::Cmp => StatementToken::Cmp,
				Token::UnparsedBlock(b) => {
					let tokenised = Token::parse_str_to_vec(helper::remove_parentheses(b))?;
					let as_statement = StatementToken::from_tokens(&tokenised)?;
					StatementToken::Parentheses(as_statement)
				}
				Token::Parens(p) => {
					//If a name is followed by parentheses, interpret is as
					// a function call.
					let last = res.len() - 1;
					if let StatementToken::Var(n) = res[last] {
						res[last] = StatementToken::FunctionCall(n, p.clone());
						continue;
					} else {
						StatementToken::Parentheses(StatementToken::from_tokens(p)?)
					}
				}
				_ => {
					return Err(ParseError(line!(), "Token is not valid in this context"));
				}
			};
			res.push(new);
		}
		Ok(res)
	}
}

///A statement is a list of tokens. It must evaluate to a value when processed
pub(crate) type Statement<'a> = Vec<StatementToken<'a>>;
///A block is a list of valid LanguageElements
pub(crate) type Block<'a> = Vec<LanguageElement<'a>>;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum LanguageElement<'a> {
	VariableDeclaration {
		typ: Type,
		name: &'a str,
	},
	VariableAssignment {
		name: &'a str,
		value: StatementElement<'a>,
	},
	VariableDecarationAssignment {
		typ: Type,
		name: &'a str,
		value: StatementElement<'a>,
	},
	FunctionDeclaration {
		typ: Type,
		name: &'a str,
		args: Vec<Variable<'a>>,
		block: Block<'a>,
	},
	IfStatement {
		condition: StatementElement<'a>,
		then: Box<Block<'a>>,
		else_then: Option<Box<Block<'a>>>,
	},
	///`init` must be a VariableDeclarationAssignment
	For {
		init: Box<LanguageElement<'a>>,
		condition: StatementElement<'a>,
		body: Box<Block<'a>>,
	},
	While {
		condition: StatementElement<'a>,
		body: Box<Block<'a>>,
	},
}

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
		let mut tokens = tokens
			.into_iter()
			.map(|t| match t {
				StatementToken::Bool(b) => Parsed(StatementElement::Bool(b)),
				StatementToken::Char(c) => Parsed(StatementElement::Char(c)),
				StatementToken::Num(n) => Parsed(StatementElement::Num(n)),
				StatementToken::Var(v) => Parsed(StatementElement::Var(v)),
				t => Unparsed(t),
			})
			.collect::<Vec<_>>();
		let operations: [(MaybeParsed, OpFnPtr); 4] = [
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
		];
		for token in tokens.iter_mut() {
			if let Unparsed(StatementToken::Parentheses(p)) = token {
				let next = StatementElement::from_tokens(p.clone())?;
				*token = Parsed(next);
			}
		}
		for (from, to) in operations.iter() {
			do_operation(&mut tokens, from, *to)?;
		}
		if tokens.len() != 1 {
			return Err(ParseError(line!(), "Internal tree construction error"));
		}
		if let Parsed(elem) = tokens.remove(0) {
			Ok(elem)
		} else {
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

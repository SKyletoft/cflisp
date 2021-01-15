#![allow(dead_code)]

pub type SourceTree<'a> = Vec<Token<'a>>;

type Function<'a> = (&'a str, Vec<Variable<'a>>);
type Variable<'a> = (&'a str, Type);

use crate::*;
use Token::*;
use Type::*;

const TYPE_NAMES: [&str; 4] = ["int", "uint", "bool", "char"];

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Type {
	Uint,
	Int,
	Char,
	Bool,
	UintPtr,
	IntPtr,
	CharPtr,
	BoolPtr,
	Void,
	Unknown,
}

#[derive(Debug, Clone, PartialEq)]
enum Token<'a> {
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
	GTE,
	LTE,
	Block(Vec<Statement<'a>>),
	Parens(Statement<'a>),
	Array,
	StringLit,
	Char,
	Apostrophe,
	AdrOf,
	Deref(&'a str),
	Name(&'a str),
	Star,
	Ampersand,
	And,
	Or,
	BitAnd,
	BitOr,
	Return,
	Num(isize),
	Void,
	For,
	While,
	NoArgs,
	Args(Vec<Variable<'a>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Statement<'a> {
	typ: Type,
	tokens: Vec<Token<'a>>,
}

pub fn parse(source: &str /* , flags: &Flags*/) -> Result<Vec<Statement>, ParseError> {
	dbg!(source);
	let tokens = split(source)?;
	let mut token_lines = Vec::new();
	for line in split_lines(&tokens).into_iter() {
		dbg!(line);
		token_lines.push(parse_tokens(line)?);
	}
	dbg!(&token_lines);
	move_declarations_to_start(&mut token_lines);
	let lines = token_lines;
	let mut functions = Vec::new();
	let mut variables = Vec::new();

	dbg!(&lines);

	let mut v = Vec::new();
	for words in lines.iter() {
		let mut simplified = simplify_lines(words, &mut functions, &mut variables)?;
		v.append(&mut simplified);
	}

	dbg!(&v);
	eprintln!("\nSo far so good!\n");
	Err(ParseError(line!()))
}

fn move_declarations_to_start<'a>(lines: &mut [Vec<Token<'a>>]) {
	let sort = |t: Option<&Token<'a>>| {
		if let Some(Decl(_)) = t {
			1
		} else {
			0
		}
	};
	lines.sort_by(|a, b| {
		let x = sort(a.get(0));
		let y = sort(b.get(0));
		x.cmp(&y)
	})
}

fn simplify_lines<'a>(
	tokens: &[Token<'a>],
	functions: &mut Vec<Function>,
	variables: &mut Vec<Variable>,
) -> Result<Vec<Vec<Token<'a>>>, ParseError> {
	dbg!(tokens);
	let ret: Vec<Vec<Token<'a>>> = match tokens {
		[Decl(t), Name(name), Assign, ..] => {
			let inner = tokens[4..].to_owned();
			simplify_lines(&inner, functions, variables)?;
			let line = vec![
				Decl(*t),
				Name(name),
				Assign,
				Block(vec![Statement {
					typ: *t,
					tokens: inner,
				}]),
			];
			vec![line]
		}
		//int x = {...}
		[Decl(t), Name(name), Assign, Block(b)] => {
			let new_var: Variable = (name, *t);
			if variables.contains(&new_var) {
				eprintln!(
					"Variable already exists. You probably meant to assign to it, \
					you can remove the type at the beginning of the line"
				);
				return Err(ParseError(line!()));
			}
			eprintln!("Blocks don't return, as of now");
			//variables.push(new_var);
			return Err(ParseError(line!()));
		}
		//int x = 5
		[Decl(t), Name(name), Assign, Num(b)] => {
			let new_var: Variable = (name, *t);
			if variables.contains(&new_var) {
				eprintln!(
					"Variable already exists. You probably meant to assign to it, \
					you can remove the type at the beginning of the line"
				);
				return Err(ParseError(line!()));
			}
			if *t != Int {
				eprintln!("Type error. You can only assign ints to ints");
				return Err(ParseError(line!()));
			}
			variables.push(new_var);
			vec![
				vec![Decl(*t), Name(name)],
				vec![Name(name), Assign, Num(*b)],
			]
		}
		//int x
		[Decl(t), Name(name)] => {
			let new_var: Variable = (name, *t);
			if variables.contains(&new_var) {
				eprintln!(
					"Variable already exists. You probably meant to assign to it, \
					you can remove the type at the beginning of the line"
				);
				return Err(ParseError(line!()));
			}
			if *t != Int {
				eprintln!("Type error. You can only assign ints to ints");
				return Err(ParseError(line!()));
			}
			variables.push(new_var);
			vec![vec![Decl(*t), Name(name)]]
		}
		[Decl(t), Name(name), Args(p), Block(b)] => {
			let new_func: Function = (name, *p);
			if functions.contains(&new_func) {
				eprintln!(
					"Function already exists with those parametres. You probably meant to assign to it, \
					you can remove the type at the beginning of the line"
				);
				return Err(ParseError(line!()));
			}
			functions.push(new_func);
			vec![]
		}
		[Name(name), Assign, Block(b)] => {}
		[Name(name), Assign, Name(b)] => {}
		[Name(name), Assign, Num(b)] => {}
		[Name(name), Parens(p)] => {}
		[If, Parens(p), Block(b)] => {}
		[If, Parens(p), Block(if_block), Else, Block(else_block)] => {}
		[Deref(name), Assign, Block(b)] => {}
		[] => {}
		_ => return Err(ParseError(line!())),
	};
	Ok(vec![])
}

fn parse_tokens<'a>(strs: &[&'a str]) -> Result<Vec<Token<'a>>, ParseError> {
	dbg!(strs);
	let mut tokens = Vec::new();
	for &name in strs.iter() {
		let token = match name {
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
			">=" => GTE,
			"<=" => LTE,
			"&&" => And,
			"&" => BitAnd,
			"|" => BitOr,
			"||" => Or,
			"return" => Return,
			"()" => Args(vec![]),
			n if n.starts_with('&') => Deref(n),
			n if n.starts_with('(') => {
				dbg!(n);
				let mut lines = parse(strip(n))?;
				Parens(lines.pop().ok_or(ParseError(line!()))?)
			}
			n if n.starts_with('{') => {
				dbg!(n);
				Block(parse(strip(n))?)
			}
			n if n.starts_with('[') => Array,
			n if n.starts_with('"') => StringLit,
			n if n.starts_with('\'') => Token::Char,
			n => {
				if n.starts_with(|n: char| n == '-' || n.is_ascii_digit())
					&& n.chars().skip(1).all(|r| r.is_ascii_digit())
				{
					Num(n
						.parse::<isize>()
						.expect("Number parse error: Is the number too large?"))
				} else {
					Name(n)
				}
			}
		};
		dbg!(&token);
		tokens.push(token);
	}
	Ok(tokens)
}

fn strip(s: &str) -> &str {
	dbg!(s);
	&s[1..s.len() - 1]
}

fn split_lines<'a>(lines: &'a [&'a str]) -> Vec<&'a [&'a str]> {
	let mut vec = Vec::new();
	let mut back = 0;
	for (index, slices) in lines.iter().enumerate() {
		if slices == &";" {
			vec.push(&lines[back..index]);
			back = index + 1;
		}
	}
	vec.push(&lines[back..]);
	vec
}

fn split(s: &str) -> Result<Vec<&str>, ParseError> {
	let keep_closure = |slice: &str| slice.chars().any(|c| !c.is_whitespace());
	let mut vec = Vec::new();
	let mut parentheses = 0;
	let mut brackets = 0;
	let mut curlies = 0;
	let mut start = 0;
	let mut quotes = 0;
	let mut escape = false;
	for (i, c) in s.char_indices() {
		match (curlies, brackets, parentheses, quotes, c) {
			(0, 0, 0, 0, '{') => {
				let slice = &s[start..i];
				if keep_closure(slice) {
					vec.push(slice);
				}
				start = i;
				curlies += 1;
			}
			(_, 0, 0, 0, '{') => {
				curlies += 1;
			}

			(1, 0, 0, 0, '}') => {
				let slice = &s[start..=i];
				if keep_closure(slice) {
					vec.push(slice);
				}
				start = i + 1;
				curlies -= 1;
			}
			(_, 0, 0, 0, '}') => {
				curlies -= 1;
			}

			(0, 0, 0, 0, '[') => {
				let slice = &s[start..i];
				if keep_closure(slice) {
					vec.push(slice);
				}
				start = i;
				curlies += 1;
			}
			(0, _, 0, 0, '[') => {
				brackets += 1;
			}
			(0, 1, 0, 0, ']') => {
				let slice = &s[start..=i];
				if keep_closure(slice) {
					vec.push(slice);
				}
				start = i + 1;
				brackets -= 1;
			}
			(0, _, 0, 0, ']') => {
				brackets -= 1;
			}

			(0, 0, 0, 0, '(') => {
				let slice = &s[start..i];
				if keep_closure(slice) {
					vec.push(slice);
				}
				start = i;
				parentheses += 1;
			}
			(0, 0, _, 0, '(') => {
				parentheses += 1;
			}
			(0, 0, 1, 0, ')') => {
				let slice = &s[start..=i];
				if keep_closure(slice) {
					vec.push(slice);
				}
				start = i + 1;
				parentheses -= 1;
			}
			(0, 0, _, 0, ')') => {
				parentheses -= 1;
			}

			(0, 0, 0, 0, '"') if !escape => {
				let slice = &s[start..i];
				if keep_closure(slice) {
					vec.push(slice);
				}
				start = i;
				quotes += 1;
			}
			(0, 0, 0, 1, '"') if !escape => {
				let slice = &s[start..=i];
				if keep_closure(slice) {
					vec.push(slice);
				}
				start = i + 1;
				quotes -= 1;
			}

			(0, 0, 0, 0, _) if c.is_whitespace() => {
				let slice = &s[start..i];
				if keep_closure(slice) {
					vec.push(slice);
				}
				start = i + 1;
			}
			(0, 0, 0, 0, ';') => {
				let slice = &s[start..i];
				if keep_closure(slice) {
					vec.push(slice);
				}
				vec.push(";");
				start = i + 1;
			}

			(0, 0, 0, 1, '\\') => {
				escape = true;
				continue;
			}
			_ => {}
		}
		escape = false;
	}
	let slice = &s[start..];
	if keep_closure(slice) {
		vec.push(slice);
	}
	if parentheses == 0 && brackets == 0 && quotes == 0 {
		Ok(vec)
	} else {
		Err(ParseError(line!()))
	}
}

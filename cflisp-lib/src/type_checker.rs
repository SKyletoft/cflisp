use crate::*;
use std::{borrow::Cow, collections::HashMap};

pub fn type_check(block: &[LanguageElement]) -> Result<bool, ParseError> {
	language_element(block, &HashMap::new(), &HashMap::new(), &HashMap::new())
}

///Mostly broken type check. While this is technically correct, it relies on a very broken type check for statements
pub fn language_element(
	block: &[LanguageElement],
	upper_variables: &HashMap<Cow<str>, Type>,
	outer_functions: &HashMap<Cow<str>, Function>,
	structs: &HashMap<&str, Vec<Variable>>,
) -> Result<bool, ParseError> {
	let mut variables = upper_variables.clone();
	let mut functions = outer_functions.clone();
	let mut structs = structs.clone();

	for line in block {
		match line {
			LanguageElement::VariableDeclaration { typ, name, .. } => {
				variables.insert(name.clone(), typ.clone());
			}

			LanguageElement::VariableAssignment { name, value, .. } => {
				let correct_type = variables
					.get(name)
					.ok_or(ParseError(line!(), "Undefined variable"))?;
				let actual_type = type_of(value, &variables, &functions, &structs)?;
				if !statement_element(value, &variables, &functions, &structs)?
					|| correct_type != &actual_type
				{
					return Ok(false);
				}
			}

			LanguageElement::VariableDeclarationAssignment {
				typ, name, value, ..
			} => {
				variables.insert(name.clone(), typ.clone());
				let actual_type = type_of(value, &variables, &functions, &structs)?;
				if !statement_element(value, &variables, &functions, &structs)?
					|| typ != &actual_type
				{
					return Ok(false);
				}
			}

			LanguageElement::PointerAssignment { ptr, value } => {
				let correct_type = type_of(ptr, &variables, &functions, &structs)?;
				let actual_type = type_of(value, &variables, &functions, &structs)?;
				if !statement_element(ptr, &variables, &functions, &structs)?
					|| !statement_element(value, &variables, &functions, &structs)?
					|| correct_type != actual_type
				{
					return Ok(false);
				}
			}

			LanguageElement::FunctionDeclaration {
				typ,
				name,
				args,
				block,
			} => {
				functions.insert(
					name.clone(),
					Function {
						return_type: typ.into(),
						name: name.as_ref(),
						parametres: args.to_vec(),
					},
				);

				if !language_element(block, &variables, &functions, &structs)?
					|| !verify_function_return_type(block, &variables, &functions, &structs, typ)?
				{
					return Ok(false);
				}
			}

			LanguageElement::IfStatement {
				condition,
				then,
				else_then,
			} => {
				if !statement_element(condition, &variables, &functions, &structs)?
					|| !language_element(then, &variables, &functions, &structs)?
					|| !else_then
						.as_deref()
						.map(|v| language_element(v, &variables, &functions, &structs))
						.unwrap_or(Ok(true))?
				{
					return Ok(false);
				}
			}

			LanguageElement::For {
				init,
				condition,
				post,
				body,
			} => {
				if !statement_element(condition, &variables, &functions, &structs)?
					|| !language_element(init, &variables, &functions, &structs)?
					|| !language_element(post, &variables, &functions, &structs)?
					|| !language_element(body, &variables, &functions, &structs)?
				{
					return Ok(false);
				}
			}

			LanguageElement::While { condition, body } => {
				if !statement_element(condition, &variables, &functions, &structs)?
					|| !language_element(body, &variables, &functions, &structs)?
				{
					return Ok(false);
				}
			}

			//Is handled by function def instead
			LanguageElement::Return(_) => {}

			LanguageElement::Statement(statement) => {
				if !statement_element(statement, &variables, &functions, &structs)? {
					return Ok(false);
				}
			}

			LanguageElement::StructAssignment { name, value } => {
				let name: &str = &name;
				let fields = structs
					.get(name)
					.ok_or(ParseError(line!(), "Undefined struct type"))?;
				if !fields.len() == value.len() {
					return Ok(false);
				}
				for (field, value) in fields.iter().zip(value.iter()) {
					if !statement_element(value, &variables, &functions, &structs)?
						|| type_of(value, &variables, &functions, &structs)? != field.typ
					{
						return Ok(false);
					}
				}
			}

			LanguageElement::StructDeclarationAssignment {
				typ, name, value, ..
			} => {
				variables.insert(name.clone(), typ.clone());
				let struct_type = if let Type::Struct(name) = typ {
					Ok(*name)
				} else {
					Err(ParseError(line!(), "Undefined struct type"))
				}?;
				let fields = structs
					.get(struct_type)
					.ok_or(ParseError(line!(), "Undefined struct type"))?;
				if !fields.len() == value.len() {
					return Ok(false);
				}
				for (field, value) in fields.iter().zip(value.iter()) {
					if !statement_element(value, &variables, &functions, &structs)?
						|| type_of(value, &variables, &functions, &structs)? != field.typ
					{
						return Ok(false);
					}
				}
			}

			LanguageElement::StructFieldPointerAssignment { name, field, value } => {
				let name: &str = &name;
				let fields = structs
					.get(name)
					.ok_or(ParseError(line!(), "Undefined struct type"))?;
				let field_type = fields
					.iter()
					.find(|Variable { name, typ: _ }| name == field)
					.ok_or(ParseError(line!(), "Undefined field"))?;
				if !statement_element(value, &variables, &functions, &structs)?
					|| type_of(value, &variables, &functions, &structs)? != field_type.typ
				{
					return Ok(false);
				}
			}

			LanguageElement::StructDefinition { name, members } => {
				let name: &str = &name;
				structs.insert(name, members.to_owned());
			}
		}
	}

	Ok(true)
}

pub(crate) fn verify_function_return_type<'a>(
	elems: &[LanguageElement<'a>],
	variables: &'a HashMap<Cow<'a, str>, Type>,
	functions: &'a HashMap<Cow<'a, str>, Function>,
	structs: &'a HashMap<&'a str, Vec<Variable<'a>>>,
	correct_return: &Type,
) -> Result<bool, ParseError> {
	macro_rules! chain_blocks {
        ($e:expr) => {
            !verify_function_return_type(
                $e,
                variables,
                functions,
                structs,
                correct_return,
            )?
        };
        ($e1:expr, $($e2:expr), +) => {
            !verify_function_return_type(
                $e1,
                variables,
                functions,
                structs,
                correct_return,
            )? || chain_blocks!($($e2), +)
        }
    }
	for elem in elems {
		match elem {
			LanguageElement::While { body, .. }
			| LanguageElement::IfStatement {
				then: body,
				else_then: None,
				..
			} => {
				if !chain_blocks!(body) {
					return Ok(false);
				}
			}
			LanguageElement::IfStatement {
				then,
				else_then: Some(else_then),
				..
			} => {
				if chain_blocks!(then, else_then) {
					return Ok(false);
				}
			}
			LanguageElement::For {
				init, post, body, ..
			} => {
				if chain_blocks!(init, post, body) {
					return Ok(false);
				}
			}
			LanguageElement::Return(v) => {
				let actual_return = if let Some(v) = v {
					type_of(v, variables, functions, structs)?
				} else {
					Type::Void
				};
				return Ok(&actual_return == correct_return);
			}
			_ => {}
		}
	}
	Ok(true)
}

pub(crate) fn type_of<'a>(
	elem: &StatementElement<'a>,
	variables: &'a HashMap<Cow<'a, str>, Type>,
	functions: &'a HashMap<Cow<'a, str>, Function>,
	structs: &'a HashMap<&'a str, Vec<Variable<'a>>>,
) -> Result<Type<'a>, ParseError> {
	let res = match elem {
		StatementElement::Char(_) => Type::Char,
		StatementElement::Num(_)
		| StatementElement::Add { .. }
		| StatementElement::Sub { .. }
		| StatementElement::Mul { .. }
		| StatementElement::Div { .. }
		| StatementElement::Mod { .. }
		| StatementElement::LShift { .. }
		| StatementElement::RShift { .. }
		| StatementElement::BitAnd { .. }
		| StatementElement::BitOr { .. }
		| StatementElement::BitNot(_) => Type::Int,
		StatementElement::Bool(_)
		| StatementElement::BoolAnd { .. }
		| StatementElement::BoolOr { .. }
		| StatementElement::Xor { .. }
		| StatementElement::BoolNot(_)
		| StatementElement::GreaterThan { .. }
		| StatementElement::LessThan { .. }
		| StatementElement::GreaterThanEqual { .. }
		| StatementElement::LessThanEqual { .. }
		| StatementElement::Cmp { .. }
		| StatementElement::NotCmp { .. } => Type::Bool,

		StatementElement::FunctionCall { name, .. } => functions
			.get(name)
			.ok_or(ParseError(line!(), "Cannot resolve function!"))?
			.return_type
			.clone()
			.into(),

		StatementElement::Var(name) => variables
			.get(name)
			.ok_or(ParseError(line!(), "Cannot resolve variable!"))?
			.clone(),

		StatementElement::Array(arr) => Type::ptr(
			arr.first()
				.map(|first| type_of(first, variables, functions, structs))
				.unwrap_or(Ok(Type::Void))?,
		),

		StatementElement::Deref(t) => type_of(t.as_ref(), variables, functions, structs)?,

		StatementElement::AdrOf(name) => Type::ptr(
			variables
				.get(name)
				.ok_or(ParseError(line!(), "Cannot resolve variable!"))?
				.clone(),
		),

		StatementElement::Ternary { lhs, rhs, .. } => {
			let l = type_of(lhs, variables, functions, structs)?;
			let r = type_of(rhs, variables, functions, structs)?;
			if l != r {
				return Err(ParseError(
					line!(),
					"Ternary operator doesn't return same type on both branches",
				));
			}
			l
		}
		StatementElement::FieldPointerAccess(name, field) => {
			let name: &str = &name;
			let struct_type = if let Type::Struct(struct_type) = variables
				.get(name)
				.ok_or(ParseError(line!(), "Cannot resolve variable"))?
			{
				Ok(struct_type)
			} else {
				Err(ParseError(line!(), "Variable isn't a struct"))
			}?;

			structs
				.get(struct_type)
				.ok_or(ParseError(line!(), "Cannot resolve struct type"))?
				.iter()
				.find(|f| f.name == field)
				.ok_or(ParseError(line!(), "Cannot resolve field name"))?
				.typ
				.clone()
		}
	};
	Ok(res)
}

pub fn statement_element(
	elem: &StatementElement,
	variables: &HashMap<Cow<str>, Type>,
	functions: &HashMap<Cow<str>, Function>,
	structs: &HashMap<&str, Vec<Variable>>,
) -> Result<bool, ParseError> {
	let res = match elem {
		StatementElement::FunctionCall { name, parametres } => {
			if let Some(f) = functions.get(name) {
				let len_eq = f.parametres.len() == parametres.len();
				let types_are_eq = {
					for (l, r) in f.parametres.iter().map(|v| &v.typ).zip(
						parametres
							.iter()
							.map(|p| type_of(p, variables, functions, structs)),
					) {
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
		| StatementElement::BoolAnd { lhs, rhs }
		| StatementElement::BoolOr { lhs, rhs }
		| StatementElement::Xor { lhs, rhs }
		| StatementElement::GreaterThan { lhs, rhs }
		| StatementElement::LessThan { lhs, rhs }
		| StatementElement::Cmp { lhs, rhs } => {
			statement_element(lhs.as_ref(), variables, functions, structs)?
				&& statement_element(rhs.as_ref(), variables, functions, structs)?
		}

		StatementElement::BoolNot(lhs) => {
			statement_element(lhs.as_ref(), variables, functions, structs)?
		}

		_ => true,
	};
	Ok(res)
}

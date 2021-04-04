use crate::*;

///Mostly broken type check. While this is technically correct, it relies on a very broken type check for statements
pub fn language_element(
	block: &[LanguageElement],
	upper_variables: &[Variable],
	outer_functions: &[Function],
) -> Result<bool, ParseError> {
	let mut variables = upper_variables.to_owned();
	let mut functions = outer_functions.to_owned();

	for line in block {
		match line {
			LanguageElement::VariableDeclaration {
				typ,
				name,
				is_static: _,
			} => variables.push(Variable {
				typ: typ.clone(),
				name: name.as_ref(),
			}),

			LanguageElement::VariableAssignment { name: _, value } => {
				if !statement_element(value, &variables, &functions)? {
					return Ok(false);
				}
			}

			LanguageElement::VariableDeclarationAssignment {
				typ,
				name,
				value,
				is_static: _,
			} => {
				variables.push(Variable {
					typ: typ.clone(),
					name: name.as_ref(),
				});
				if !statement_element(value, &variables, &functions)? {
					return Ok(false);
				}
			}

			LanguageElement::PointerAssignment { ptr, value } => {
				if !statement_element(ptr, &variables, &functions)?
					|| !statement_element(value, &variables, &functions)?
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
				functions.push(Function {
					return_type: typ.into(),
					name: name.as_ref(),
					parametres: args.to_vec(),
				});

				if !language_element(block, &variables, &functions)? {
					return Ok(false);
				}
			}

			LanguageElement::IfStatement {
				condition,
				then,
				else_then,
			} => {
				if !statement_element(condition, &variables, &functions)?
					|| !language_element(then, &variables, &functions)?
					|| !else_then
						.as_deref()
						.map(|v| language_element(v, &variables, &functions))
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
				if !statement_element(condition, &variables, &functions)?
					|| !language_element(init, &variables, &functions)?
					|| !language_element(post, &variables, &functions)?
					|| !language_element(body, &variables, &functions)?
				{
					return Ok(false);
				}
			}

			LanguageElement::While { condition, body } => {
				if !statement_element(condition, &variables, &functions)?
					|| !language_element(body, &variables, &functions)?
				{
					return Ok(false);
				}
			}

			//Is handled by function def instead
			LanguageElement::Return(_) => {}

			LanguageElement::Statement(statement) => {
				if !statement_element(statement, &variables, &functions)? {
					return Ok(false);
				}
			}

			LanguageElement::StructAssignment { name, value } => todo!(),
			LanguageElement::StructDeclarationAssignment {
				typ,
				name,
				value,
				is_static,
			} => todo!(),
			LanguageElement::StructFieldPointerAssignment { name, field, value } => todo!(),
			LanguageElement::StructDefinition { .. } => {}
		}
	}

	Ok(true)
}

pub fn type_of<'a>(
	elem: &StatementElement<'a>,
	functions: &'a [Function<'a>],
	variables: &'a [Variable<'a>],
) -> Result<NativeType, ParseError> {
	let res = match elem {
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
			.map(|v| (&v.typ).into())
			.ok_or(ParseError(line!(), "Cannot resolve variable!"))?,

		StatementElement::Array(arr) => NativeType::ptr(
			arr.get(0)
				.map(|s| type_of(s, functions, variables))
				.unwrap_or(Ok(NativeType::Void))?,
		),

		StatementElement::Deref(t) => type_of(t.as_ref(), functions, variables)?,

		StatementElement::AdrOf(name) => NativeType::ptr(
			variables
				.iter()
				.find(|f| f.name == name)
				.map(|v| (&v.typ).into())
				.ok_or(ParseError(line!(), "Cannot resolve variable!"))?,
		),

		_ => todo!(),
	};
	Ok(res)
}

pub fn statement_element(
	elem: &StatementElement,
	variables: &[Variable],
	functions: &[Function],
) -> Result<bool, ParseError> {
	let res = match elem {
		StatementElement::FunctionCall { name, parametres } => {
			if let Some(f) = functions.iter().find(|f| f.name == name) {
				let len_eq = f.parametres.len() == parametres.len();
				let types_are_eq = {
					for (l, r) in f
						.parametres
						.iter()
						.map(|v| &v.typ)
						.zip(parametres.iter().map(|p| type_of(p, functions, variables)))
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
			statement_element(lhs.as_ref(), variables, functions)?
				&& statement_element(rhs.as_ref(), variables, functions)?
		}

		StatementElement::Not { lhs } => statement_element(lhs.as_ref(), variables, functions)?,

		_ => true,
	};
	Ok(res)
}

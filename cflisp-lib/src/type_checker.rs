use crate::*;
use std::{
	borrow::Cow,
	collections::{HashMap, HashSet},
};

pub fn type_check(block: &[LanguageElement]) -> Result<(), TypeError> {
	language_element(
		block,
		&mut HashMap::new(),
		&mut HashMap::new(),
		&mut HashMap::new(),
		&mut HashSet::new(),
	)
}

///The maps are out parametres because of loops
pub(crate) fn language_element<'a>(
	block: &'a [LanguageElement<'a>],
	variables: &mut HashMap<Cow<'a, str>, Type<'a>>,
	functions: &mut HashMap<Cow<'a, str>, Function<'a>>,
	structs: &mut HashMap<&'a str, Vec<Variable<'a>>>,
	constants: &mut HashSet<&'a str>,
) -> Result<(), TypeError> {
	for line in block {
		match line {
			LanguageElement::VariableDeclaration {
				typ,
				name,
				is_const,
				..
			} => {
				variables.insert(name.clone(), typ.clone());
				if *is_const {
					constants.insert(&name);
				}
			}

			LanguageElement::VariableAssignment { name, value, .. } => {
				statement_element(value, &variables, &functions, &structs)?;
				let correct_type = variables.get(name).ok_or_else(|| {
					dbg!(line, name);
					TypeError(line!(), "Undefined variable")
				})?;
				let actual_type = type_of(value, &variables, &functions, &structs)?;
				let name_ref: &str = &name;
				if correct_type != &actual_type {
					dbg!(line, correct_type, actual_type);
					return Err(TypeError(line!(), "Type mismatch"));
				}
				if constants.contains(&name_ref) {
					dbg!(line, name);
					return Err(TypeError(line!(), "Assignment to constant"));
				}
			}

			LanguageElement::VariableDeclarationAssignment {
				typ,
				name,
				value,
				is_const,
				..
			} => {
				statement_element(value, &variables, &functions, &structs)?;
				variables.insert(name.clone(), typ.clone());
				if *is_const {
					constants.insert(&name);
				}
				let actual_type = type_of(value, &variables, &functions, &structs)?;
				if typ != &actual_type {
					dbg!(line, typ, actual_type);
					return Err(TypeError(line!(), "Type mismatch"));
				}
			}

			LanguageElement::PointerAssignment { ptr, value } => {
				statement_element(ptr, &variables, &functions, &structs)?;
				statement_element(value, &variables, &functions, &structs)?;
				let correct_type = type_of(ptr, &variables, &functions, &structs)?;
				let actual_type = type_of(value, &variables, &functions, &structs)?;
				if correct_type != actual_type {
					dbg!(line, correct_type, actual_type);
					return Err(TypeError(line!(), "Type mismatch"));
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
				if args
					.iter()
					.any(|Variable { typ, name: _ }| typ == &Type::Void)
				{
					return Err(TypeError(line!(), "Function has void argument"));
				}
				if name == "interrupt" {
					if typ != &Type::Void {
						return Err(TypeError(line!(), "Interrupt handler does not return void"));
					}
					if !args.is_empty() {
						return Err(TypeError(line!(), "Interrupt handler takes arugments"));
					}
				}

				let mut inner_variables = variables.clone();
				args.iter().cloned().for_each(|Variable { name, typ }| {
					inner_variables.insert(Cow::Borrowed(name), typ);
				});
				let mut inner_functions = functions.clone();
				let mut inner_structs = structs.clone();
				let mut inner_constants = constants.clone();
				language_element(
					block,
					&mut inner_variables,
					&mut inner_functions,
					&mut inner_structs,
					&mut inner_constants,
				)?;
				verify_function_return_type(
					block,
					&inner_variables,
					&inner_functions,
					&inner_structs,
					typ,
				)?;
			}

			LanguageElement::IfStatement {
				condition,
				then,
				else_then,
			} => {
				statement_element(condition, &variables, &functions, &structs)?;
				let mut inner_variables = variables.clone();
				let mut inner_functions = functions.clone();
				let mut inner_structs = structs.clone();
				let mut inner_constants = constants.clone();
				language_element(
					then,
					&mut inner_variables,
					&mut inner_functions,
					&mut inner_structs,
					&mut inner_constants,
				)?;
				if let Some(else_then) = else_then {
					let mut inner_variables = variables.clone();
					let mut inner_functions = functions.clone();
					let mut inner_structs = structs.clone();
					let mut inner_constants = constants.clone();
					language_element(
						else_then,
						&mut inner_variables,
						&mut inner_functions,
						&mut inner_structs,
						&mut inner_constants,
					)?;
				}
			}

			LanguageElement::For {
				init,
				condition,
				post,
				body,
			} => {
				let mut inner_variables = variables.clone();
				let mut inner_functions = functions.clone();
				let mut inner_structs = structs.clone();
				let mut inner_constants = constants.clone();
				language_element(
					init,
					&mut inner_variables,
					&mut inner_functions,
					&mut inner_structs,
					&mut inner_constants,
				)?;
				statement_element(
					condition,
					&inner_variables,
					&inner_functions,
					&inner_structs,
				)?;
				language_element(
					post,
					&mut inner_variables,
					&mut inner_functions,
					&mut inner_structs,
					&mut inner_constants,
				)?;
				language_element(
					body,
					&mut inner_variables,
					&mut inner_functions,
					&mut inner_structs,
					&mut inner_constants,
				)?;
			}

			LanguageElement::While { condition, body } => {
				statement_element(condition, &variables, &functions, &structs)?;
				let mut inner_variables = variables.clone();
				let mut inner_functions = functions.clone();
				let mut inner_structs = structs.clone();
				let mut inner_constants = constants.clone();
				language_element(
					body,
					&mut inner_variables,
					&mut inner_functions,
					&mut inner_structs,
					&mut inner_constants,
				)?;
			}

			//Is handled by function def instead
			LanguageElement::Return(_) => {}

			LanguageElement::Statement(statement) => {
				statement_element(statement, &variables, &functions, &structs)?;
			}

			LanguageElement::StructAssignment { name, value } => {
				let name: &str = &name;
				let fields = structs.get(name).ok_or_else(|| {
					dbg!(line, name);
					TypeError(line!(), "Undefined struct type")
				})?;
				if fields.len() != value.len() {
					dbg!(line, fields.len(), value.len());
					return Err(TypeError(
						line!(),
						"Not the correct amount of fields in struct initalisation",
					));
				}
				for (field, value) in fields.iter().zip(value.iter()) {
					statement_element(value, &variables, &functions, &structs)?;
					if type_of(value, &variables, &functions, &structs)? != field.typ {
						return Err(TypeError(
							line!(),
							"Struct field type mismatch in intialisation",
						));
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
					dbg!(line, typ);
					Err(TypeError(line!(), "Undefined struct type"))
				}?;
				let fields = structs.get(struct_type).ok_or_else(|| {
					dbg!(line, struct_type);
					TypeError(line!(), "Undefined struct type")
				})?;
				if fields.len() != value.len() {
					dbg!(line, fields.len(), value.len());
					return Err(TypeError(
						line!(),
						"Not the correct amount of fields in struct initalisation",
					));
				}
				for (field, value) in fields.iter().zip(value.iter()) {
					statement_element(value, &variables, &functions, &structs)?;
					let actual_type = type_of(value, &variables, &functions, &structs)?;
					let correct_type = &field.typ;
					if &actual_type != correct_type {
						dbg!(line, actual_type, correct_type);
						return Err(TypeError(
							line!(),
							"Struct field type mismatch in intialisation",
						));
					}
				}
			}

			LanguageElement::StructFieldPointerAssignment { name, field, value } => {
				let name: &str = &name;
				let fields = structs.get(name).ok_or_else(|| {
					dbg!(line, name);
					TypeError(line!(), "Undefined struct type")
				})?;
				let field_type = fields
					.iter()
					.find(|Variable { name, typ: _ }| name == field)
					.ok_or_else(|| {
						dbg!(line, field);
						TypeError(line!(), "Undefined field")
					})?;
				statement_element(value, &variables, &functions, &structs)?;
				let actual_type = type_of(value, &variables, &functions, &structs)?;
				let correct_type = &field_type.typ;
				if &actual_type != correct_type {
					dbg!(line, actual_type, correct_type);
					return Err(TypeError(
						line!(),
						"Struct field type mismatch in pointer assignment",
					));
				}
			}

			LanguageElement::StructDefinition { name, members } => {
				let name: &str = &name;
				structs.insert(name, members.to_owned());
			}
		}
	}

	Ok(())
}

pub(crate) fn verify_function_return_type<'a>(
	elems: &[LanguageElement<'a>],
	variables: &'a HashMap<Cow<'a, str>, Type>,
	functions: &'a HashMap<Cow<'a, str>, Function>,
	structs: &'a HashMap<&'a str, Vec<Variable<'a>>>,
	correct_return: &Type,
) -> Result<(), TypeError> {
	macro_rules! chain_blocks {
        ($e:expr) => {
            verify_function_return_type(
                $e,
                variables,
                functions,
                structs,
                correct_return,
            )?;
        };
        ($e1:expr, $($e2:expr), +) => {
            verify_function_return_type(
                $e1,
                variables,
                functions,
                structs,
                correct_return,
            )?;
			chain_blocks!($($e2), +)
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
				chain_blocks!(body);
			}
			LanguageElement::IfStatement {
				then,
				else_then: Some(else_then),
				..
			} => {
				chain_blocks!(then, else_then);
			}
			LanguageElement::For {
				init, post, body, ..
			} => {
				chain_blocks!(init, post, body);
			}
			LanguageElement::Return(v) => {
				let actual_return = if let Some(v) = v {
					type_of(v, variables, functions, structs)?
				} else {
					Type::Void
				};
				if &actual_return != correct_return {
					dbg!(elem, actual_return, correct_return);
					return Err(TypeError(line!(), "Function returns wrong type"));
				}
			}
			_ => {}
		}
	}
	Ok(())
}

pub(crate) fn type_of<'a>(
	elem: &StatementElement<'a>,
	variables: &'a HashMap<Cow<'a, str>, Type>,
	functions: &'a HashMap<Cow<'a, str>, Function>,
	structs: &'a HashMap<&'a str, Vec<Variable<'a>>>,
) -> Result<Type<'a>, TypeError> {
	let res = match elem {
		StatementElement::Xor { lhs, rhs } => {
			let lhs = type_of(lhs, variables, functions, structs)?;
			let rhs = type_of(rhs, variables, functions, structs)?;
			if lhs != rhs || !(lhs == Type::Int || lhs == Type::Bool) {
				return Err(TypeError(line!(), "Xor has arguments of different types"));
			}
			lhs
		}
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
		| StatementElement::BoolNot(_)
		| StatementElement::GreaterThan { .. }
		| StatementElement::LessThan { .. }
		| StatementElement::GreaterThanEqual { .. }
		| StatementElement::LessThanEqual { .. }
		| StatementElement::Cmp { .. }
		| StatementElement::NotCmp { .. } => Type::Bool,

		StatementElement::FunctionCall { name, .. } => functions
			.get(name)
			.ok_or_else(|| {
				dbg!(elem, name);
				TypeError(line!(), "Cannot resolve function!")
			})?
			.return_type
			.clone()
			.into(),

		StatementElement::Var(name) => variables
			.get(name)
			.ok_or_else(|| {
				dbg!(elem, name);
				TypeError(line!(), "Cannot resolve variable!")
			})?
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
				.ok_or_else(|| {
					dbg!(elem, name);
					TypeError(line!(), "Cannot resolve variable!")
				})?
				.clone(),
		),

		StatementElement::Ternary { lhs, rhs, .. } => {
			let l = type_of(lhs, variables, functions, structs)?;
			let r = type_of(rhs, variables, functions, structs)?;
			if l != r {
				dbg!(elem, l, r);
				return Err(TypeError(
					line!(),
					"Ternary operator doesn't return same type on both branches",
				));
			}
			l
		}
		StatementElement::FieldPointerAccess(name, field) => {
			let name: &str = &name;

			let typ = variables.get(name).ok_or_else(|| {
				dbg!(elem, name, variables);
				TypeError(line!(), "Cannot resolve variable")
			})?;
			let struct_type = if let Type::Ptr(pointing_at) = typ {
				if let Type::Struct(struct_type) = pointing_at.as_ref() {
					Ok(struct_type)
				} else {
					dbg!(elem, name, variables);
					Err(TypeError(line!(), "Variable isn't a pointer to a struct"))
				}
			} else {
				dbg!(elem, name, variables);
				Err(TypeError(line!(), "Variable isn't a pointer"))
			}?;

			structs
				.get(struct_type)
				.ok_or_else(|| {
					dbg!(elem, struct_type);
					TypeError(line!(), "Cannot resolve struct type")
				})?
				.iter()
				.find(|f| f.name == field)
				.ok_or_else(|| {
					dbg!(elem, field);
					TypeError(line!(), "Cannot resolve field name")
				})?
				.typ
				.clone()
		}
		StatementElement::Cast { typ, .. } => typ.into(),
	};
	Ok(res)
}

pub fn statement_element(
	elem: &StatementElement,
	variables: &HashMap<Cow<str>, Type>,
	functions: &HashMap<Cow<str>, Function>,
	structs: &HashMap<&str, Vec<Variable>>,
) -> Result<(), TypeError> {
	let res = match elem {
		StatementElement::FunctionCall { name, parametres } => {
			if let Some(f) = functions.get(name) {
				if f.parametres.len() != parametres.len() {
					dbg!(elem, f.parametres.len(), parametres.len());
					return Err(TypeError(
						line!(),
						"Wrong amount of arguments in function call",
					));
				}
				for (l, r) in f.parametres.iter().map(|v| &v.typ).zip(
					parametres
						.iter()
						.map(|p| type_of(p, variables, functions, structs)),
				) {
					let r = r?;
					if l != &r {
						dbg!(elem, l, r);
						return Err(TypeError(line!(), "Wrong type in function call"));
					}
				}
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
		| StatementElement::GreaterThan { lhs, rhs }
		| StatementElement::LessThan { lhs, rhs }
		| StatementElement::Cmp { lhs, rhs }
		| StatementElement::BitAnd { lhs, rhs }
		| StatementElement::BitOr { lhs, rhs }
		| StatementElement::Xor { lhs, rhs }
		| StatementElement::GreaterThanEqual { lhs, rhs }
		| StatementElement::LessThanEqual { lhs, rhs }
		| StatementElement::NotCmp { lhs, rhs } => {
			statement_element(lhs, variables, functions, structs)?;
			statement_element(rhs, variables, functions, structs)?;
		}

		StatementElement::Deref(lhs)
		| StatementElement::BitNot(lhs)
		| StatementElement::BoolNot(lhs) => {
			statement_element(lhs, variables, functions, structs)?;
		}

		StatementElement::Cast { value, .. } => {
			statement_element(value, variables, functions, structs)?;
		}

		StatementElement::Array(arr) => {
			arr.iter()
				.try_for_each(|elem| statement_element(elem, variables, functions, structs))?;
		}

		StatementElement::Var(name) | StatementElement::AdrOf(name) => {
			if !variables.contains_key(name) {
				dbg!(elem);
				return Err(TypeError(line!(), "Unknown variable"));
			}
		}
		StatementElement::FieldPointerAccess(name, field) => {
			if !variables.contains_key(name) {
				dbg!(elem);
				return Err(TypeError(line!(), "Unknown variable"));
			}
			if let Some(Type::Struct(struct_type)) = variables.get(name) {
				if let Some(fields) = structs.get(struct_type) {
					if !fields.iter().any(|Variable { name, .. }| name == field) {
						dbg!(elem);
						return Err(TypeError(line!(), "Unknown field name"));
					}
				} else {
					dbg!(elem);
					return Err(TypeError(line!(), "Unknown struct type"));
				}
			} else {
				dbg!(elem);
				return Err(TypeError(line!(), "Variable is not a struct"));
			}
		}

		StatementElement::Ternary { .. }
		| StatementElement::Num(_)
		| StatementElement::Char(_)
		| StatementElement::Bool(_) => {}
	};
	Ok(res)
}

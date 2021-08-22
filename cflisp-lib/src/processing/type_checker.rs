use std::{
	borrow::Cow,
	collections::{HashMap, HashSet},
};

use crate::*;

pub fn type_check(block: &[LanguageElement]) -> Result<()> {
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
	functions: &mut HashMap<&'a str, Function<'a>>,
	structs: &mut HashMap<&'a str, Vec<NativeVariable<'a>>>,
	constants: &mut HashSet<&'a str>,
) -> Result<()> {
	for line in block.iter() {
		match line {
			LanguageElement::VariableDeclaration {
				typ,
				name,
				is_const,
				..
			} => {
				let name: &str = name;
				variables.insert(name.into(), typ.clone());
				if *is_const {
					constants.insert(name);
				}
			}

			LanguageElement::VariableAssignment { name, value, .. } => {
				let name: &str = name;
				statement_element(value, variables, functions, structs, constants)?;
				let correct_type = variables
					.get(name)
					.ok_or_else(|| error!(UndefinedVariable, line))?;
				let actual_type = type_of(value, variables, functions, structs)?;
				let name_ref: &str = name;
				if correct_type != &actual_type {
					return Err(error!(TypeMismatch, line));
				}
				if constants.contains(&name_ref) {
					return Err(error!(AssignmentToConstant, line));
				}
			}

			LanguageElement::VariableDeclarationAssignment {
				typ,
				name,
				value,
				is_const,
				..
			} => {
				statement_element(value, variables, functions, structs, constants)?;
				variables.insert(name.clone(), typ.clone());
				if *is_const {
					constants.insert(name);
				}
				let actual_type = type_of(value, variables, functions, structs)?;
				if typ != &actual_type {
					return Err(error!(TypeMismatch, line));
				}
			}

			LanguageElement::PointerAssignment { ptr, value, .. } => {
				statement_element(ptr, variables, functions, structs, constants)?;
				statement_element(value, variables, functions, structs, constants)?;
				let correct_type = type_of(ptr, variables, functions, structs)?
					.get_ptr_inner()
					.ok_or_else(|| error!(InternalPointerAssignmentToNonPointer, line))?;
				let actual_type = type_of(value, variables, functions, structs)?;
				if correct_type != actual_type {
					return Err(error!(TypeMismatch, line));
				}
			}

			LanguageElement::FunctionDeclaration {
				typ,
				name,
				args,
				block,
			} => {
				functions.insert(
					name,
					Function {
						return_type: typ.clone(),
						name,
						parametres: args.to_vec(),
					},
				);
				if args
					.iter()
					.any(|Variable { typ, name: _ }| typ == &Type::Void)
				{
					return Err(error!(IllegalVoidArgument, line));
				}
				if name == "interrupt" {
					if typ != &Type::Void {
						return Err(error!(MalformedInterruptHandlerReturn, line));
					}
					if !args.is_empty() {
						return Err(error!(MalformedInterruptHandlerArguments, line));
					}
				}

				let mut inner_variables = variables.clone();
				for Variable { name, typ } in args.iter().cloned() {
					if let Some(struct_type) = typ.get_struct() {
						let fields = structs
							.get(struct_type)
							.ok_or_else(|| error!(UndefinedType, line))?;
						for NativeVariable {
							name: field_name,
							typ,
						} in fields.iter()
						{
							let field_name = helper::merge_name_and_field(name, field_name);
							let field_type: Type = typ.into();
							inner_variables.insert(field_name, field_type);
						}
					}
					inner_variables.insert(name.into(), typ);
				}
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

			LanguageElement::FunctionSignatureDeclaration { typ, name, args } => {
				let name: &str = name;
				functions.insert(
					name,
					Function {
						return_type: typ.clone(),
						name,
						parametres: args.to_vec(),
					},
				);
				if args
					.iter()
					.any(|Variable { typ, name: _ }| typ == &Type::Void)
				{
					return Err(error!(IllegalVoidArgument, line));
				}
				if name == "interrupt" {
					if typ != &Type::Void {
						return Err(error!(MalformedInterruptHandlerReturn, line));
					}
					if !args.is_empty() {
						return Err(error!(MalformedInterruptHandlerArguments, line));
					}
				}
			}

			LanguageElement::IfStatement {
				condition,
				then,
				else_then,
			} => {
				statement_element(condition, variables, functions, structs, constants)?;
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
					constants,
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
				statement_element(condition, variables, functions, structs, constants)?;
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

			//Function def handles correct return types, here we only need to check if the statement is valid
			LanguageElement::Return(None) => {}

			LanguageElement::Return(Some(statement)) | LanguageElement::Statement(statement) => {
				statement_element(statement, variables, functions, structs, constants)?;
			}

			LanguageElement::StructAssignment { name, value } => {
				let name: &str = name;
				let fields = structs
					.get(name)
					.ok_or_else(|| error!(UndefinedType, line))?;
				if fields.len() != value.len() {
					return Err(error!(MissingStructFields, line));
				}
				for (field, value) in fields.iter().zip(value.iter()) {
					statement_element(value, variables, functions, structs, constants)?;
					if type_of(value, variables, functions, structs)? != field.typ {
						return Err(error!(TypeMismatch, line));
					}
				}
			}

			LanguageElement::StructDeclarationAssignment {
				typ, name, value, ..
			} => {
				variables.insert(name.clone(), typ.clone());
				let struct_type = typ.get_struct().ok_or_else(|| error!(TypeMismatch, line))?;
				let fields = structs
					.get(struct_type)
					.ok_or_else(|| error!(UndefinedField, line))?;
				if fields.len() != value.len() {
					return Err(error!(MissingStructFields, line));
				}
				for (field, value) in fields.iter().zip(value.iter()) {
					statement_element(value, variables, functions, structs, constants)?;
					let actual_type = type_of(value, variables, functions, structs)?;
					let correct_type = &field.typ;
					if &actual_type != correct_type {
						return Err(error!(TypeMismatch, line));
					}
				}
			}

			LanguageElement::StructFieldPointerAssignment {
				name, field, value, ..
			} => {
				let name: &str = name;
				let field: &str = field;
				let fields = structs
					.get(name)
					.ok_or_else(|| error!(UndefinedType, line))?;
				let field_type = fields
					.iter()
					.find(|NativeVariable { name, .. }| name == field)
					.ok_or_else(|| error!(UndefinedField, line))?;
				statement_element(value, variables, functions, structs, constants)?;
				let actual_type = type_of(value, variables, functions, structs)?;
				let correct_type = &field_type.typ;
				if &actual_type != correct_type {
					return Err(error!(TypeMismatch, line));
				}
			}

			LanguageElement::StructDefinition { name, members } => {
				let name: &str = name;
				structs.insert(name, members.to_owned());
			}
		}
	}

	Ok(())
}

pub(crate) fn verify_function_return_type<'a>(
	elems: &[LanguageElement<'a>],
	variables: &'a HashMap<Cow<'a, str>, Type>,
	functions: &'a HashMap<&'a str, Function>,
	structs: &'a HashMap<&'a str, Vec<NativeVariable<'a>>>,
	correct_return: &Type,
) -> Result<()> {
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
				let actual_return = if let Some(ref v) = v {
					type_of(v, variables, functions, structs)?
				} else {
					Type::Void
				};
				if &actual_return != correct_return {
					return Err(error!(TypeMismatch, elem)); //Maybe `elem` instead?
				}
			}
			_ => {}
		}
	}
	Ok(())
}

fn ptr_type_promotion<'a>(lhs: Type<'a>, rhs: Type<'a>) -> Option<Type<'a>> {
	match (&lhs, &rhs) {
		(Type::Ptr(_) | Type::Arr(..), Type::Int | Type::Uint) => Some(lhs),
		(Type::Int | Type::Uint, Type::Ptr(_) | Type::Arr(..)) => Some(rhs),
		_ => None,
	}
}

pub(crate) fn type_of<'a>(
	elem: &StatementElement<'a>,
	variables: &'a HashMap<Cow<'a, str>, Type>,
	functions: &'a HashMap<&'a str, Function>,
	structs: &'a HashMap<&'a str, Vec<NativeVariable<'a>>>,
) -> Result<Type<'a>> {
	let res = match elem {
		StatementElement::Xor { lhs, rhs } => {
			let lhs = type_of(lhs, variables, functions, structs)?;
			let rhs = type_of(rhs, variables, functions, structs)?;
			if lhs != rhs || !(lhs == Type::Int || lhs == Type::Bool) {
				return Err(error!(TypeMismatch, elem));
			}
			lhs
		}
		StatementElement::Char(_) => Type::Char,
		StatementElement::Num(_) => Type::Int,
		StatementElement::Add { lhs, rhs }
		| StatementElement::Sub { lhs, rhs }
		| StatementElement::Mul { lhs, rhs }
		| StatementElement::Div { lhs, rhs }
		| StatementElement::Mod { lhs, rhs }
		| StatementElement::LShift { lhs, rhs }
		| StatementElement::RShift { lhs, rhs }
		| StatementElement::BitAnd { lhs, rhs }
		| StatementElement::BitOr { lhs, rhs } => ptr_type_promotion(
			type_of(lhs, variables, functions, structs)?,
			type_of(rhs, variables, functions, structs)?,
		)
		.unwrap_or(Type::Int),
		StatementElement::IncDec { statement: val, .. } | StatementElement::BitNot(val) => {
			type_of(val, variables, functions, structs)?
		}
		StatementElement::BoolAnd { lhs, rhs } | StatementElement::BoolOr { lhs, rhs } => {
			let lhs_t = type_of(lhs, variables, functions, structs)?;
			let rhs_t = type_of(rhs, variables, functions, structs)?;
			if lhs_t != Type::Bool || rhs_t != Type::Bool {
				return Err(error!(TypeMismatch, elem));
			}
			Type::Bool
		}
		StatementElement::BoolNot(val) => {
			let val_t = type_of(val, variables, functions, structs)?;
			if val_t != Type::Bool {
				return Err(error!(TypeMismatch, elem));
			}
			Type::Bool
		}
		StatementElement::Bool(_)
		| StatementElement::GreaterThan { .. }
		| StatementElement::LessThan { .. }
		| StatementElement::GreaterThanEqual { .. }
		| StatementElement::LessThanEqual { .. }
		| StatementElement::Cmp { .. }
		| StatementElement::NotCmp { .. } => Type::Bool,

		StatementElement::FunctionCall { name, .. } => {
			let name: &str = name;
			functions
				.get(name)
				.ok_or_else(|| error!(UndefinedFunction, elem))?
				.return_type
				.clone()
		}

		StatementElement::Var(name) => {
			let name: &str = name;
			variables
				.get(name)
				.ok_or_else(|| error!(UndefinedVariable, elem))?
				.clone()
		}

		StatementElement::Array(arr) => Type::ptr(
			arr.first()
				.map(|first| type_of(first, variables, functions, structs))
				.unwrap_or(Ok(Type::Void))?,
		),

		StatementElement::Deref(t) => match type_of(t.as_ref(), variables, functions, structs)? {
			Type::Ptr(t) | Type::Arr(t, _) => *t,
			_ => return Err(error!(DerefNonPointer, elem)),
		},

		StatementElement::AdrOf(name) => {
			let name: &str = name;
			Type::ptr(
				variables
					.get(name)
					.ok_or_else(|| error!(UndefinedVariable, elem))?
					.clone(),
			)
		}

		StatementElement::Ternary { lhs, rhs, .. } => {
			let l = type_of(lhs, variables, functions, structs)?;
			let r = type_of(rhs, variables, functions, structs)?;
			if l != r {
				return Err(error!(TypeMismatch, elem));
			}
			l
		}
		StatementElement::FieldPointerAccess(name, field) => {
			let name: &str = name;

			let struct_type = variables
				.get(name)
				.ok_or_else(|| error!(UndefinedVariable, elem))?
				.get_ptr_inner_ref()
				.ok_or_else(|| error!(TypeMismatch, elem))?
				.get_struct()
				.ok_or_else(|| error!(TypeMismatch, elem))?;

			(&structs
				.get(struct_type)
				.ok_or_else(|| error!(UndefinedType, elem))?
				.iter()
				.find(|f| &f.name == field)
				.ok_or_else(|| error!(UndefinedField, elem))?
				.typ)
				.into()
		}
		StatementElement::Cast { typ, .. } => typ.into(),
	};
	Ok(res)
}

pub fn statement_element(
	elem: &StatementElement,
	variables: &HashMap<Cow<str>, Type>,
	functions: &HashMap<&str, Function>,
	structs: &HashMap<&str, Vec<NativeVariable>>,
	constants: &HashSet<&str>,
) -> Result<()> {
	let res = match elem {
		StatementElement::FunctionCall { name, parametres } => {
			let name: &str = name;
			if let Some(f) = functions.get(name) {
				if f.parametres.len() != parametres.len() {
					return Err(error!(MissingArguments, elem));
				}
				for (l, r) in f.parametres.iter().map(|v| &v.typ).zip(
					parametres
						.iter()
						.map(|p| type_of(p, variables, functions, structs)),
				) {
					let r = r?;
					if l != &r {
						return Err(error!(TypeMismatch, elem));
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
		| StatementElement::GreaterThan { lhs, rhs }
		| StatementElement::LessThan { lhs, rhs }
		| StatementElement::Cmp { lhs, rhs }
		| StatementElement::BitAnd { lhs, rhs }
		| StatementElement::BitOr { lhs, rhs }
		| StatementElement::Xor { lhs, rhs }
		| StatementElement::GreaterThanEqual { lhs, rhs }
		| StatementElement::LessThanEqual { lhs, rhs }
		| StatementElement::NotCmp { lhs, rhs } => {
			statement_element(lhs, variables, functions, structs, constants)?;
			statement_element(rhs, variables, functions, structs, constants)?;
			let lhs_t = type_of(lhs, variables, functions, structs)?;
			let rhs_t = type_of(rhs, variables, functions, structs)?;
			if lhs_t != rhs_t
				|| !matches!(lhs_t, Type::Int | Type::Uint | Type::Char)
				|| !matches!(rhs_t, Type::Int | Type::Uint | Type::Char)
			{
				return Err(error!(TypeMismatch, elem));
			}
		}

		StatementElement::BitNot(lhs)
		| StatementElement::Deref(lhs)
		| StatementElement::Cast { value: lhs, .. } => {
			statement_element(lhs, variables, functions, structs, constants)?;
		}

		StatementElement::BoolNot(lhs) => {
			statement_element(lhs, variables, functions, structs, constants)?;
			if type_of(lhs, variables, functions, structs)? != Type::Bool {
				return Err(error!(TypeMismatch, elem));
			}
		}

		StatementElement::BoolAnd { lhs, rhs } | StatementElement::BoolOr { lhs, rhs } => {
			statement_element(lhs, variables, functions, structs, constants)?;
			statement_element(rhs, variables, functions, structs, constants)?;
			if type_of(lhs, variables, functions, structs)? != Type::Bool
				|| type_of(rhs, variables, functions, structs)? != Type::Bool
			{
				return Err(error!(TypeMismatch, elem));
			}
		}

		StatementElement::IncDec { statement, .. } => {
			statement_element(statement, variables, functions, structs, constants)?;
			let typ = type_of(statement, variables, functions, structs)?;
			if let StatementElement::Var(name) = statement.as_ref() {
				if constants.contains(name as &str) {
					return Err(error!(AssignmentToConstant, elem));
				}
			}
			if !matches!(typ, Type::Int | Type::Uint | Type::Char | Type::Ptr(_)) {
				return Err(error!(TypeMismatch, elem));
			}
		}

		StatementElement::Array(arr) => {
			arr.iter().try_for_each(|elem| {
				statement_element(elem, variables, functions, structs, constants)
			})?;
		}

		StatementElement::Var(name) | StatementElement::AdrOf(name) => {
			let name: &str = name;
			if !variables.contains_key(name) {
				return Err(error!(UndefinedVariable, elem));
			}
		}
		StatementElement::FieldPointerAccess(name, field) => {
			let name: &str = name;
			let field: &str = field;
			if !variables.contains_key(name) {
				return Err(error!(UndefinedVariable, elem));
			}
			if let Some(Type::Struct(struct_type)) = variables.get(name) {
				if let Some(fields) = structs.get(struct_type) {
					if !fields
						.iter()
						.any(|NativeVariable { name, .. }| name == field)
					{
						return Err(error!(UndefinedField, elem));
					}
				} else {
					return Err(error!(UndefinedType, elem));
				}
			} else {
				return Err(error!(TypeMismatch, elem));
			}
		}

		StatementElement::Ternary { .. }
		| StatementElement::Num(_)
		| StatementElement::Char(_)
		| StatementElement::Bool(_) => {}
	};
	Ok(res)
}

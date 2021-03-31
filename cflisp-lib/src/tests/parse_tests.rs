use super::super::*;
use std::borrow::Cow;

/*
	Yeah, this is terrible to write. Currently done by copying the current
	result and then used as a regression test rather than a target

	TEMPLATE:


#[test]
fn init_native_variables() {
	let source = include_str!("init_native_variables.c");
	let parsed = parser::parse(source, false).expect("Test failed");
	let expected = vec![];
	assert_eq!(expected, parsed);
}

*/
#[test]
fn create_main() {
	let source = include_str!("create_main.c");
	let expected = vec![LanguageElement::FunctionDeclaration {
		typ: Type::Int,
		name: Cow::Borrowed("main"),
		args: vec![],
		block: vec![],
	}];
	let parsed = parser::parse(source, false).expect("Test failed");
	assert_eq!(expected, parsed);
}

#[test]
fn init_native_variables() {
	let source = include_str!("init_native_variables.c");
	let parsed = parser::parse(source, false).expect("Test failed");
	let expected = vec![LanguageElement::FunctionDeclaration {
		typ: Type::Int,
		name: "main".into(),
		args: vec![],
		block: vec![
			LanguageElement::VariableDeclaration {
				typ: Type::Int,
				name: "x".into(),
				is_static: false,
			},
			LanguageElement::VariableDeclarationAssignment {
				typ: Type::Int,
				name: "y".into(),
				value: StatementElement::Num(5),
				is_static: false,
			},
			LanguageElement::VariableDeclarationAssignment {
				typ: Type::Int,
				name: "z".into(),
				value: StatementElement::Add {
					lhs: Box::new(StatementElement::Num(6)),
					rhs: Box::new(StatementElement::Num(2)),
				},
				is_static: false,
			},
			LanguageElement::VariableDeclaration {
				typ: Type::Char,
				name: "c".into(),
				is_static: false,
			},
			LanguageElement::VariableDeclarationAssignment {
				typ: Type::Char,
				name: "d".into(),
				value: StatementElement::Char('a'),
				is_static: false,
			},
		],
	}];
	assert_eq!(expected, parsed);
}

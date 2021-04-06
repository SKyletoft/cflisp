//Clippy is broken. These are not unused imports. 1/4-21
#![allow(unused_imports)]
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

//This test only makes sure that it doesn't crash, not that the output is correct
#[test]
fn legacy_dont_crash() {
	let run = |file| {
		let flags = Flags::default();
		let no_comments = parser::remove_comments(file);
		let parsed = parser::parse(&no_comments, false).unwrap();
		let no_structs = LanguageElementStructless::from_language_elements(parsed).unwrap();
		let asm = compile_flisp::compile(&no_structs, &flags).unwrap();
		let text = text::instructions_to_text(&asm, &flags).unwrap();
		assert!(!text.is_empty());
	};
	let files = [
		include_str!("../../../cflisp-cli/legacy_tests/test1.c"),
		include_str!("../../../cflisp-cli/legacy_tests/test2.c"),
		include_str!("../../../cflisp-cli/legacy_tests/test4.c"),
		include_str!("../../../cflisp-cli/legacy_tests/test5.c"),
		include_str!("../../../cflisp-cli/legacy_tests/test7.c"),
		include_str!("../../../cflisp-cli/legacy_tests/test8.c"),
		include_str!("../../../cflisp-cli/legacy_tests/test9.c"),
		include_str!("../../../cflisp-cli/legacy_tests/test10.c"),
		include_str!("../../../cflisp-cli/legacy_tests/test11.c"),
		include_str!("../../../cflisp-cli/legacy_tests/test12.c"),
		include_str!("../../../cflisp-cli/legacy_tests/test13.c"),
		include_str!("../../../cflisp-cli/legacy_tests/test15.c"),
		include_str!("../../../cflisp-cli/legacy_tests/test16.c"),
		include_str!("../../../cflisp-cli/legacy_tests/test17.c"),
		include_str!("../../../cflisp-cli/legacy_tests/test18.c"),
		include_str!("../../../cflisp-cli/legacy_tests/test19.c"),
		include_str!("../../../cflisp-cli/legacy_tests/test20.c"),
		include_str!("../../../cflisp-cli/legacy_tests/test21.c"),
		include_str!("../../../cflisp-cli/legacy_tests/test22.c"),
		include_str!("../../../cflisp-cli/legacy_tests/test24.c"),
		include_str!("../../../cflisp-cli/legacy_tests/test27.c"),
	];

	for file in files.iter() {
		eprintln!("{}", file);
		run(file);
	}
}

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

#[test]
fn parse_numbers() {
	for expected in (i8::MIN as isize)..=(u8::MAX as isize) {
		let as_string = format!("{}", expected);
		let (parsed, rest) = lexer::get_token(&as_string).unwrap();
		if let Token::Num(n) = parsed {
			assert_eq!(expected as i8, n as i8);
			assert!(rest.is_empty());
		} else {
			panic!();
		}
	}
	for expected in (i8::MIN as isize)..=(u8::MAX as isize) {
		let expected = expected as i8;
		let as_string = format!("0x{:x}", expected);
		let (parsed, rest) = lexer::get_token(&as_string).unwrap();
		if let Token::Num(n) = parsed {
			assert_eq!(expected, n as i8);
			assert!(rest.is_empty());
		} else {
			panic!();
		}

		let as_string = format!("0x{:X}", expected);
		let (parsed, rest) = lexer::get_token(&as_string).unwrap();
		if let Token::Num(n) = parsed {
			assert_eq!(expected, n as i8);
			assert!(rest.is_empty());
		} else {
			panic!();
		}
	}
}

//Clippy is broken. These are not unused imports. 1/4-21
#![allow(unused_imports)]
use std::{
	borrow::Cow,
	collections::{HashMap, HashSet},
};

use super::{super::*, *};
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
		let type_checked = type_checker::type_check(&parsed);
		assert!(type_checked.is_ok(), "Type check failed");
		let no_structs = StructlessLanguage::from_language_elements(parsed).unwrap();
		let asm = flisp::compile_flisp::compile(&no_structs, &flags).unwrap();
		let text = flisp::text::instructions_to_text(&asm, &flags).unwrap();
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

	for (index, file) in files.iter().enumerate() {
		eprintln!("-[{}]------------------------------\n{}", index, file);
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
				is_const: false,
				is_volatile: false,
			},
			LanguageElement::VariableDeclarationAssignment {
				typ: Type::Int,
				name: "y".into(),
				value: StatementElement::Num(5.into()),
				is_static: false,
				is_const: false,
				is_volatile: false,
			},
			LanguageElement::VariableDeclarationAssignment {
				typ: Type::Int,
				name: "z".into(),
				value: StatementElement::Add {
					lhs: Box::new(StatementElement::Num(6.into())),
					rhs: Box::new(StatementElement::Num(2.into())),
				},
				is_static: false,
				is_const: false,
				is_volatile: false,
			},
			LanguageElement::VariableDeclaration {
				typ: Type::Char,
				name: "c".into(),
				is_static: false,
				is_const: false,
				is_volatile: false,
			},
			LanguageElement::VariableDeclarationAssignment {
				typ: Type::Char,
				name: "d".into(),
				value: StatementElement::Char('a'),
				is_static: false,
				is_const: false,
				is_volatile: false,
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
			assert_eq!(expected as i8, n.val as i8);
			assert!(rest.is_empty());
		} else {
			panic!();
		}
	}
	for expected in (i8::MIN as isize)..=(u8::MAX as isize) {
		let expected = expected as i8;
		let as_string = format!("0x{:x}", expected);
		let (parsed_hex_lo, rest) = lexer::get_token(&as_string).unwrap();
		if let Token::Num(n) = parsed_hex_lo {
			assert_eq!(expected, n.val as i8);
			assert!(rest.is_empty());
		} else {
			panic!();
		}

		let as_string = format!("0X{:X}", expected);
		let (parsed_hex_hi, rest) = lexer::get_token(&as_string).unwrap();
		if let Token::Num(n) = parsed_hex_hi {
			assert_eq!(expected, n.val as i8);
			assert!(rest.is_empty());
		} else {
			panic!();
		}
	}
}

#[test]
fn comments() {
	let expected_1 = "keep this and this";
	let expected_2 = "a\nb\nc\n";

	let cases_1 = [
		"keep this and this// but not this",
		"keep this /*not this*/and this",
		"keep this /*not this\nor this*/and this",
		"ke/*hello!*/ep this/*\n\n\n\nhello//not me*/ and this",
		"keep /* /* /* /* NESTED COMMENTS! */ */ */ */this and this",
	];
	for &case in cases_1.iter() {
		let result = parser::remove_comments(case);
		assert_eq!(result, expected_1, "\n{:?}", case);
	}

	let cases_2 = [
		"a//hello\nb\nc\n",
		"a//hi//continued\nb//more comments \nc\n",
	];
	for &case in cases_2.iter() {
		let result = parser::remove_comments(case);
		assert_eq!(result, expected_2, "\n{:?}", case);
	}

	let case_3 = include_str!("commented.txt");
	let expected_3 = include_str!("commentless.txt");
	let result_3 = parser::remove_comments(case_3);
	assert_eq!(result_3, expected_3);
}

#[test]
fn ternary_op() {
	let case_1 = "5 < 6 ? 1 : 0";
	let expected_1 = StatementElement::Ternary {
		cond: Box::new(StatementElement::LessThan {
			lhs: Box::new(StatementElement::Num(5.into())),
			rhs: Box::new(StatementElement::Num(6.into())),
		}),
		lhs: Box::new(StatementElement::Num(1.into())),
		rhs: Box::new(StatementElement::Num(0.into())),
	};
	let res_1 = StatementElement::parse_with_no_tail(&Token::by_byte(case_1).unwrap()).unwrap();
	assert_eq!(res_1, expected_1);

	let case_2 = "a < b ? c : d ? f : g";
	let expected_2 = StatementElement::Ternary {
		cond: Box::new(StatementElement::LessThan {
			lhs: Box::new(StatementElement::Var(Cow::Borrowed("a"))),
			rhs: Box::new(StatementElement::Var(Cow::Borrowed("b"))),
		}),
		lhs: Box::new(StatementElement::Var(Cow::Borrowed("c"))),
		rhs: Box::new(StatementElement::Ternary {
			cond: Box::new(StatementElement::Var(Cow::Borrowed("d"))),
			lhs: Box::new(StatementElement::Var(Cow::Borrowed("f"))),
			rhs: Box::new(StatementElement::Var(Cow::Borrowed("g"))),
		}),
	};
	let res_2 = StatementElement::parse_with_no_tail(&Token::by_byte(case_2).unwrap()).unwrap();
	assert_eq!(res_2, expected_2);
}

#[test]
fn negate() {
	let case_1 = "-x";
	let expected_1 = StatementElement::Sub {
		lhs: Box::new(StatementElement::Num(0.into())),
		rhs: Box::new(StatementElement::Var(Cow::Borrowed("x"))),
	};
	let res_1 = StatementElement::parse_with_no_tail(&Token::by_byte(case_1).unwrap()).unwrap();
	assert_eq!(res_1, expected_1);

	let case_2 = "-(-x)";
	let expected_2 = StatementElement::Sub {
		lhs: Box::new(StatementElement::Num(0.into())),
		rhs: Box::new(StatementElement::Sub {
			lhs: Box::new(StatementElement::Num(0.into())),
			rhs: Box::new(StatementElement::Var(Cow::Borrowed("x"))),
		}),
	};
	let res_2 = StatementElement::parse_with_no_tail(&Token::by_byte(case_2).unwrap()).unwrap();
	assert_eq!(res_2, expected_2);
}

#[test]
fn merge_comments() {
	let case_1 = merge_comments!(&Some(Cow::Borrowed("a")));
	let expected_1 = Some(Cow::Borrowed("a"));
	assert_eq!(case_1, expected_1);

	let case_2 = merge_comments!(&None, &Some(Cow::Borrowed("a")));
	assert_eq!(case_2, expected_1);

	let case_3 = merge_comments!(&Some(Cow::Borrowed("a")), &Some(Cow::Borrowed("b")));
	let expected_3 = Some(Cow::Owned(String::from("a, b")));
	assert_eq!(case_3, expected_3);

	let case_4 = merge_comments!(
		&Some(Cow::Borrowed("a")),
		&Some(Cow::Borrowed("b")),
		&Some(Cow::Borrowed("c"))
	);
	let expected_4 = Some(Cow::Owned(String::from("a, b, c")));
	assert_eq!(case_4, expected_4);

	let case_5 = merge_comments!(&Some(Cow::Borrowed("a")), &None, &Some(Cow::Borrowed("c")));
	let expected_5 = Some(Cow::Owned(String::from("a, c")));
	assert_eq!(case_5, expected_5);
}

#[test]
fn type_check() {
	let case_1 = "int x = 5;";
	let res_1 = type_checker::type_check(&parser::parse(case_1, false).unwrap());
	assert!(res_1.is_ok());

	let case_2 = "int x = 'a';";
	let res_2 = type_checker::type_check(&parser::parse(case_2, false).unwrap());
	assert!(res_2.is_err());

	let case_3 = "char x = 5;";
	let res_3 = type_checker::type_check(&parser::parse(case_3, false).unwrap());
	assert!(res_3.is_err());

	let case_4 = include_str!("type_test_1.c");
	let res_4 = type_checker::type_check(&parser::parse(case_4, false).unwrap());
	assert!(res_4.is_ok());

	let case_5 = include_str!("type_test_2.c");
	let res_5 = type_checker::type_check(&parser::parse(case_5, false).unwrap());
	assert!(res_5.is_err());
}

#[test]
fn statement_parse() {
	use parser::Parsable;
	let tokens = [Token::Num(5.into())];
	let tokens_semicolon = [Token::Num(5.into()), Token::NewLine];
	let tokens_comma = [Token::Num(5.into()), Token::Comma];
	let tokens_semicolon_cont = [Token::Num(5.into()), Token::NewLine, Token::Num(2.into())];
	let tokens_comma_cont = [Token::Num(5.into()), Token::Comma, Token::Num(2.into())];
	let empty_slice: &[Token] = &[];
	let two_slice: &[Token] = &[Token::Num(2.into())];
	assert_eq!(
		StatementElement::parse(&tokens),
		Ok((StatementElement::Num(5.into()), empty_slice))
	);
	assert_eq!(
		StatementElement::parse(&tokens_semicolon),
		Ok((StatementElement::Num(5.into()), empty_slice))
	);
	assert_eq!(
		StatementElement::parse(&tokens_comma),
		Ok((StatementElement::Num(5.into()), empty_slice))
	);
	assert_eq!(
		StatementElement::parse(&tokens_semicolon_cont),
		Ok((StatementElement::Num(5.into()), two_slice))
	);
	assert_eq!(
		StatementElement::parse(&tokens_comma_cont),
		Ok((StatementElement::Num(5.into()), two_slice))
	);
}

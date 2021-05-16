#![allow(unused_imports)]
use std::borrow::Cow;

use super::super::*;

#[test]
fn fast_mul() {
	let x = Cow::Borrowed("x");

	let mut case_1 = StructlessStatement::Mul {
		lhs: Box::new(StructlessStatement::Num(5)),
		rhs: Box::new(StructlessStatement::Num(6)),
	};
	let expected_1 = StructlessStatement::Num(5 * 6);
	optimise_statement::fast_mul(&mut case_1);
	assert_eq!(case_1, expected_1);

	let mut case_2 = StructlessStatement::Mul {
		lhs: Box::new(StructlessStatement::Num(0b10101)),
		rhs: Box::new(StructlessStatement::Var(x.clone())),
	};
	let expected_2 = StructlessStatement::Add {
		lhs: Box::new(StructlessStatement::Add {
			lhs: Box::new(StructlessStatement::Var(x.clone())),
			rhs: Box::new(StructlessStatement::LShift {
				lhs: Box::new(StructlessStatement::Var(x.clone())),
				rhs: Box::new(StructlessStatement::Num(2)),
			}),
		}),
		rhs: Box::new(StructlessStatement::LShift {
			lhs: Box::new(StructlessStatement::Var(x.clone())),
			rhs: Box::new(StructlessStatement::Num(4)),
		}),
	};
	optimise_statement::fast_mul(&mut case_2);
	assert_eq!(case_2, expected_2);
}

#[test]
fn fast_div() {
	let mut case_1 = StructlessStatement::Div {
		lhs: Box::new(StructlessStatement::Var(Cow::Borrowed("x"))),
		rhs: Box::new(StructlessStatement::Num(8)),
	};
	let expected_1 = StructlessStatement::RShift {
		lhs: Box::new(StructlessStatement::Var(Cow::Borrowed("x"))),
		rhs: Box::new(StructlessStatement::Num(3)),
	};
	optimise_statement::fast_div(&mut case_1);
	assert_eq!(case_1, expected_1);
}

#[test]
fn fast_mod() {
	let mut case_1 = StructlessStatement::Mod {
		lhs: Box::new(StructlessStatement::Var(Cow::Borrowed("x"))),
		rhs: Box::new(StructlessStatement::Num(8)),
	};
	let expected_1 = StructlessStatement::And {
		lhs: Box::new(StructlessStatement::Var(Cow::Borrowed("x"))),
		rhs: Box::new(StructlessStatement::Num(0b0000_0111)),
	};
	optimise_statement::fast_mod(&mut case_1);
	assert_eq!(case_1, expected_1);
}

#[test]
fn const_eval() {
	let mut case_1 = StructlessStatement::Add {
		lhs: Box::new(StructlessStatement::Num(5)),
		rhs: Box::new(StructlessStatement::Num(3)),
	};
	let expected_1 = StructlessStatement::Num(5 + 3);
	optimise_statement::const_eval(&mut case_1);
	assert_eq!(case_1, expected_1);

	let mut case_2 = StructlessStatement::Div {
		lhs: Box::new(StructlessStatement::Mul {
			lhs: Box::new(StructlessStatement::Add {
				lhs: Box::new(StructlessStatement::Num(5)),
				rhs: Box::new(StructlessStatement::Num(3)),
			}),
			rhs: Box::new(StructlessStatement::Num(2)),
		}),
		rhs: Box::new(StructlessStatement::Add {
			lhs: Box::new(StructlessStatement::Sub {
				lhs: Box::new(StructlessStatement::Num(8)),
				rhs: Box::new(StructlessStatement::Num(12)),
			}),
			rhs: Box::new(StructlessStatement::Num(34)),
		}),
	};
	let expected_2 = StructlessStatement::Num(0);
	optimise_statement::const_eval(&mut case_2);
	assert_eq!(case_2, expected_2);

	let mut case_3 = StructlessStatement::Mul {
		lhs: Box::new(StructlessStatement::Add {
			lhs: Box::new(StructlessStatement::Num(5)),
			rhs: Box::new(StructlessStatement::Num(3)),
		}),
		rhs: Box::new(StructlessStatement::Num(2)),
	};
	let expected_3 = StructlessStatement::Num((5 + 3) * 2);
	optimise_statement::const_eval(&mut case_3);
	assert_eq!(case_3, expected_3);
}

#[test]
fn remove_unused_variables() {
	let mut case_1 = vec![
		StructlessLanguage::VariableDeclarationAssignment {
			name: Cow::Borrowed("x"),
			typ: NativeType::Int,
			value: StructlessStatement::Num(5),
			is_static: false,
			is_const: false,
			is_volatile: false,
		},
		StructlessLanguage::VariableDeclarationAssignment {
			name: Cow::Borrowed("y"),
			typ: NativeType::Int,
			value: StructlessStatement::Add {
				lhs: Box::new(StructlessStatement::Num(1)),
				rhs: Box::new(StructlessStatement::Var(Cow::Borrowed("x"))),
			},
			is_static: false,
			is_const: false,
			is_volatile: false,
		},
		StructlessLanguage::VariableDeclarationAssignment {
			name: Cow::Borrowed("z"),
			typ: NativeType::Int,
			value: StructlessStatement::Num(5),
			is_static: false,
			is_const: false,
			is_volatile: false,
		},
		StructlessLanguage::Statement(StructlessStatement::Var(Cow::Borrowed("y"))),
	];
	let expected_1 = vec![
		StructlessLanguage::VariableDeclarationAssignment {
			name: Cow::Borrowed("x"),
			typ: NativeType::Int,
			value: StructlessStatement::Num(5),
			is_static: false,
			is_const: false,
			is_volatile: false,
		},
		StructlessLanguage::VariableDeclarationAssignment {
			name: Cow::Borrowed("y"),
			typ: NativeType::Int,
			value: StructlessStatement::Add {
				lhs: Box::new(StructlessStatement::Num(1)),
				rhs: Box::new(StructlessStatement::Var(Cow::Borrowed("x"))),
			},
			is_static: false,
			is_const: false,
			is_volatile: false,
		},
		StructlessLanguage::Statement(StructlessStatement::Var(Cow::Borrowed("y"))),
	];
	optimise_language::remove_unused_variables(&mut case_1);
	assert_eq!(case_1, expected_1);
}

#[test]
fn const_prop() {
	let mut case_1 = vec![
		StructlessLanguage::VariableDeclarationAssignment {
			typ: NativeType::Int,
			name: Cow::Borrowed("x"),
			value: StructlessStatement::Num(5),
			is_const: true,
			is_static: false,
			is_volatile: false,
		},
		StructlessLanguage::Return(Some(StructlessStatement::Add {
			lhs: Box::new(StructlessStatement::Var(Cow::Borrowed("x"))),
			rhs: Box::new(StructlessStatement::Num(2)),
		})),
	];
	let expected_1_mid = vec![
		StructlessLanguage::VariableDeclarationAssignment {
			typ: NativeType::Int,
			name: Cow::Borrowed("x"),
			value: StructlessStatement::Num(5),
			is_const: true,
			is_static: false,
			is_volatile: false,
		},
		StructlessLanguage::Return(Some(StructlessStatement::Add {
			lhs: Box::new(StructlessStatement::Num(5)),
			rhs: Box::new(StructlessStatement::Num(2)),
		})),
	];
	let expected_1_end = vec![StructlessLanguage::Return(Some(StructlessStatement::Num(
		7,
	)))];
	optimise_language::const_prop(&mut case_1);
	assert_eq!(&case_1, &expected_1_mid);
	optimise_language::all_optimisations(&mut case_1).unwrap();
	assert_eq!(case_1, expected_1_end);
}

#[test]
fn extract_statics() {
	let case_1 = vec![LanguageElement::FunctionDeclaration {
		typ: Type::Int,
		name: Cow::Borrowed("main"),
		args: vec![],
		block: vec![
			LanguageElement::VariableDeclaration {
				typ: Type::Int,
				name: Cow::Borrowed("alpha_beta_gamma"),
				is_static: true,
				is_const: false,
				is_volatile: false,
			},
			LanguageElement::VariableAssignment {
				name: Cow::Borrowed("alpha_beta_gamma"),
				value: StatementElement::Num(3),
			},
		],
	}];
	let expected_1 = vec![
		StructlessLanguage::VariableDeclaration {
			typ: NativeType::Int,
			name: Cow::Borrowed("main::alpha_beta_gamma"),
			is_static: true,
			is_const: false,
			is_volatile: false,
		},
		StructlessLanguage::FunctionDeclaration {
			typ: NativeType::Int,
			name: Cow::Borrowed("main"),
			args: vec![],
			block: vec![StructlessLanguage::VariableAssignment {
				name: Cow::Borrowed("main::alpha_beta_gamma"),
				value: StructlessStatement::Num(3),
			}],
		},
	];
	let res_1 = StructlessLanguage::from_language_elements(case_1).unwrap();
	assert_eq!(expected_1, res_1);
}

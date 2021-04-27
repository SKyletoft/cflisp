#![allow(unused_imports)]
use super::super::*;
use std::borrow::Cow;

#[test]
fn fast_mul() {
	let x = Cow::Borrowed("x");

	let mut case_1 = StatementElementStructless::Mul {
		lhs: Box::new(StatementElementStructless::Num(5)),
		rhs: Box::new(StatementElementStructless::Num(6)),
	};
	let expected_1 = StatementElementStructless::Num(5 * 6);
	optimise_statement::fast_mul(&mut case_1);
	assert_eq!(case_1, expected_1);

	let mut case_2 = StatementElementStructless::Mul {
		lhs: Box::new(StatementElementStructless::Num(0b10101)),
		rhs: Box::new(StatementElementStructless::Var(x.clone())),
	};
	let expected_2 = StatementElementStructless::Add {
		lhs: Box::new(StatementElementStructless::Add {
			lhs: Box::new(StatementElementStructless::Var(x.clone())),
			rhs: Box::new(StatementElementStructless::LShift {
				lhs: Box::new(StatementElementStructless::Var(x.clone())),
				rhs: Box::new(StatementElementStructless::Num(2)),
			}),
		}),
		rhs: Box::new(StatementElementStructless::LShift {
			lhs: Box::new(StatementElementStructless::Var(x.clone())),
			rhs: Box::new(StatementElementStructless::Num(4)),
		}),
	};
	optimise_statement::fast_mul(&mut case_2);
	assert_eq!(case_2, expected_2);
}

#[test]
fn const_eval() {
	let mut case_1 = StatementElementStructless::Add {
		lhs: Box::new(StatementElementStructless::Num(5)),
		rhs: Box::new(StatementElementStructless::Num(3)),
	};
	let expected_1 = StatementElementStructless::Num(5 + 3);
	optimise_statement::const_eval(&mut case_1);
	assert_eq!(case_1, expected_1);

	let mut case_2 = StatementElementStructless::Div {
		lhs: Box::new(StatementElementStructless::Mul {
			lhs: Box::new(StatementElementStructless::Add {
				lhs: Box::new(StatementElementStructless::Num(5)),
				rhs: Box::new(StatementElementStructless::Num(3)),
			}),
			rhs: Box::new(StatementElementStructless::Num(2)),
		}),
		rhs: Box::new(StatementElementStructless::Add {
			lhs: Box::new(StatementElementStructless::Sub {
				lhs: Box::new(StatementElementStructless::Num(8)),
				rhs: Box::new(StatementElementStructless::Num(12)),
			}),
			rhs: Box::new(StatementElementStructless::Num(34)),
		}),
	};
	let expected_2 = StatementElementStructless::Num(0);
	optimise_statement::const_eval(&mut case_2);
	assert_eq!(case_2, expected_2);

	let mut case_3 = StatementElementStructless::Mul {
		lhs: Box::new(StatementElementStructless::Add {
			lhs: Box::new(StatementElementStructless::Num(5)),
			rhs: Box::new(StatementElementStructless::Num(3)),
		}),
		rhs: Box::new(StatementElementStructless::Num(2)),
	};
	let expected_3 = StatementElementStructless::Num((5 + 3) * 2);
	optimise_statement::const_eval(&mut case_3);
	assert_eq!(case_3, expected_3);
}

#[test]
fn remove_unused_variables() {
	let mut case_1 = vec![
		LanguageElementStructless::VariableDeclarationAssignment {
			name: Cow::Borrowed("x"),
			typ: NativeType::Int,
			value: StatementElementStructless::Num(5),
			is_static: false,
			is_const: false,
			is_volatile: false,
		},
		LanguageElementStructless::VariableDeclarationAssignment {
			name: Cow::Borrowed("y"),
			typ: NativeType::Int,
			value: StatementElementStructless::Add {
				lhs: Box::new(StatementElementStructless::Num(1)),
				rhs: Box::new(StatementElementStructless::Var(Cow::Borrowed("x"))),
			},
			is_static: false,
			is_const: false,
			is_volatile: false,
		},
		LanguageElementStructless::VariableDeclarationAssignment {
			name: Cow::Borrowed("z"),
			typ: NativeType::Int,
			value: StatementElementStructless::Num(5),
			is_static: false,
			is_const: false,
			is_volatile: false,
		},
		LanguageElementStructless::Statement(StatementElementStructless::Var(Cow::Borrowed("y"))),
	];
	let expected_1 = vec![
		LanguageElementStructless::VariableDeclarationAssignment {
			name: Cow::Borrowed("x"),
			typ: NativeType::Int,
			value: StatementElementStructless::Num(5),
			is_static: false,
			is_const: false,
			is_volatile: false,
		},
		LanguageElementStructless::VariableDeclarationAssignment {
			name: Cow::Borrowed("y"),
			typ: NativeType::Int,
			value: StatementElementStructless::Add {
				lhs: Box::new(StatementElementStructless::Num(1)),
				rhs: Box::new(StatementElementStructless::Var(Cow::Borrowed("x"))),
			},
			is_static: false,
			is_const: false,
			is_volatile: false,
		},
		LanguageElementStructless::Statement(StatementElementStructless::Var(Cow::Borrowed("y"))),
	];
	optimise_language::remove_unused_variables(&mut case_1);
	assert_eq!(case_1, expected_1);
}

#[test]
fn const_prop() {
	let mut case_1 = vec![
		LanguageElementStructless::VariableDeclarationAssignment {
			typ: NativeType::Int,
			name: Cow::Borrowed("x"),
			value: StatementElementStructless::Num(5),
			is_const: true,
			is_static: false,
			is_volatile: false,
		},
		LanguageElementStructless::Return(Some(StatementElementStructless::Add {
			lhs: Box::new(StatementElementStructless::Var(Cow::Borrowed("x"))),
			rhs: Box::new(StatementElementStructless::Num(2)),
		})),
	];
	let expected_1_mid = vec![
		LanguageElementStructless::VariableDeclarationAssignment {
			typ: NativeType::Int,
			name: Cow::Borrowed("x"),
			value: StatementElementStructless::Num(5),
			is_const: true,
			is_static: false,
			is_volatile: false,
		},
		LanguageElementStructless::Return(Some(StatementElementStructless::Add {
			lhs: Box::new(StatementElementStructless::Num(5)),
			rhs: Box::new(StatementElementStructless::Num(2)),
		})),
	];
	let expected_1_end = vec![LanguageElementStructless::Return(Some(
		StatementElementStructless::Num(7),
	))];
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
		LanguageElementStructless::VariableDeclaration {
			typ: NativeType::Int,
			name: Cow::Borrowed("main::alpha_beta_gamma"),
			is_static: true,
			is_const: false,
			is_volatile: false,
		},
		LanguageElementStructless::FunctionDeclaration {
			typ: NativeType::Int,
			name: Cow::Borrowed("main"),
			args: vec![],
			block: vec![LanguageElementStructless::VariableAssignment {
				name: Cow::Borrowed("main::alpha_beta_gamma"),
				value: StatementElementStructless::Num(3),
			}],
		},
	];
	let res_1 = LanguageElementStructless::from_language_elements(case_1).unwrap();
	assert_eq!(expected_1, res_1);
}

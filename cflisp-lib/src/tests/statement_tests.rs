#![allow(unused_imports)]
use std::borrow::Cow;

use super::super::*;

#[test]
fn fast_mul() {
	let x = Cow::Borrowed("x");

	let mut case_1 = StructlessStatement::BinOp {
		op: BinOp::Mul,
		lhs: Box::new(StructlessStatement::Num(5.into())),
		rhs: Box::new(StructlessStatement::Num(6.into())),
		signedness: NumberType::Unknown,
	};
	let expected_1 = StructlessStatement::Num((5 * 6).into());
	optimise_statement::fast_mul(&mut case_1);
	assert_eq!(case_1, expected_1);

	let mut case_2 = StructlessStatement::BinOp {
		op: BinOp::Mul,
		lhs: Box::new(StructlessStatement::Num(0b10101.into())),
		rhs: Box::new(StructlessStatement::Var(x.clone())),
		signedness: NumberType::Unknown,
	};
	let expected_2 = StructlessStatement::BinOp {
		op: BinOp::Add,
		lhs: Box::new(StructlessStatement::BinOp {
			op: BinOp::Add,
			lhs: Box::new(StructlessStatement::Var(x.clone())),
			rhs: Box::new(StructlessStatement::BinOp {
				op: BinOp::LShift,
				lhs: Box::new(StructlessStatement::Var(x.clone())),
				rhs: Box::new(StructlessStatement::Num(2.into())),
				signedness: NumberType::Unknown,
			}),
			signedness: NumberType::Unknown,
		}),
		rhs: Box::new(StructlessStatement::BinOp {
			op: BinOp::LShift,
			lhs: Box::new(StructlessStatement::Var(x.clone())),
			rhs: Box::new(StructlessStatement::Num(4.into())),
			signedness: NumberType::Unknown,
		}),
		signedness: NumberType::Unknown,
	};
	optimise_statement::fast_mul(&mut case_2);
	assert_eq!(case_2, expected_2);
}

#[test]
fn fast_div() {
	let mut case_1 = StructlessStatement::BinOp {
		op: BinOp::Div,
		lhs: Box::new(StructlessStatement::Var(Cow::Borrowed("x"))),
		rhs: Box::new(StructlessStatement::Num(8.into())),
		signedness: NumberType::Unknown,
	};
	let expected_1 = StructlessStatement::BinOp {
		op: BinOp::RShift,
		lhs: Box::new(StructlessStatement::Var(Cow::Borrowed("x"))),
		rhs: Box::new(StructlessStatement::Num(3.into())),
		signedness: NumberType::Unknown,
	};
	optimise_statement::fast_div(&mut case_1);
	assert_eq!(case_1, expected_1);
}

#[test]
fn fast_mod() {
	let mut case_1 = StructlessStatement::BinOp {
		op: BinOp::Mod,
		lhs: Box::new(StructlessStatement::Var(Cow::Borrowed("x"))),
		rhs: Box::new(StructlessStatement::Num(8.into())),
		signedness: NumberType::Unknown,
	};
	let expected_1 = StructlessStatement::BinOp {
		op: BinOp::And,
		lhs: Box::new(StructlessStatement::Var(Cow::Borrowed("x"))),
		rhs: Box::new(StructlessStatement::Num(0b0000_0111.into())),
		signedness: NumberType::Unknown,
	};
	optimise_statement::fast_mod(&mut case_1);
	assert_eq!(case_1, expected_1);
}

#[test]
fn const_eval() {
	let mut case_1 = StructlessStatement::BinOp {
		op: BinOp::Add,
		lhs: Box::new(StructlessStatement::Num(5.into())),
		rhs: Box::new(StructlessStatement::Num(3.into())),
		signedness: NumberType::Unknown,
	};
	let expected_1 = StructlessStatement::Num((5 + 3).into());
	optimise_statement::const_eval(&mut case_1);
	assert_eq!(case_1, expected_1);

	let mut case_2 = StructlessStatement::BinOp {
		op: BinOp::Div,
		lhs: Box::new(StructlessStatement::BinOp {
			op: BinOp::Mul,
			lhs: Box::new(StructlessStatement::BinOp {
				op: BinOp::Add,
				lhs: Box::new(StructlessStatement::Num(5.into())),
				rhs: Box::new(StructlessStatement::Num(3.into())),
				signedness: NumberType::Unknown,
			}),
			rhs: Box::new(StructlessStatement::Num(2.into())),
			signedness: NumberType::Unknown,
		}),
		rhs: Box::new(StructlessStatement::BinOp {
			op: BinOp::Add,
			lhs: Box::new(StructlessStatement::BinOp {
				op: BinOp::Sub,
				lhs: Box::new(StructlessStatement::Num(8.into())),
				rhs: Box::new(StructlessStatement::Num(12.into())),
				signedness: NumberType::Unknown,
			}),
			rhs: Box::new(StructlessStatement::Num(34.into())),
			signedness: NumberType::Unknown,
		}),
		signedness: NumberType::Unknown,
	};
	let expected_2 = StructlessStatement::Num(0.into());
	optimise_statement::const_eval(&mut case_2);
	assert_eq!(case_2, expected_2);

	let mut case_3 = StructlessStatement::BinOp {
		op: BinOp::Mul,
		lhs: Box::new(StructlessStatement::BinOp {
			op: BinOp::Add,
			lhs: Box::new(StructlessStatement::Num(5.into())),
			rhs: Box::new(StructlessStatement::Num(3.into())),
			signedness: NumberType::Unknown,
		}),
		rhs: Box::new(StructlessStatement::Num(2.into())),
		signedness: NumberType::Unknown,
	};
	let expected_3 = StructlessStatement::Num(((5 + 3) * 2).into());
	optimise_statement::const_eval(&mut case_3);
	assert_eq!(case_3, expected_3);
}

#[test]
fn remove_unused_variables() {
	let mut case_1 = vec![
		StructlessLanguage::VariableDeclarationAssignment {
			name: Cow::Borrowed("x"),
			typ: NativeType::Int,
			value: StructlessStatement::Num(5.into()),
			is_static: false,
			is_const: false,
			is_volatile: false,
		},
		StructlessLanguage::VariableDeclarationAssignment {
			name: Cow::Borrowed("y"),
			typ: NativeType::Int,
			value: StructlessStatement::BinOp {
				op: BinOp::Add,
				lhs: Box::new(StructlessStatement::Num(1.into())),
				rhs: Box::new(StructlessStatement::Var(Cow::Borrowed("x"))),
				signedness: NumberType::Unknown,
			},
			is_static: false,
			is_const: false,
			is_volatile: false,
		},
		StructlessLanguage::VariableDeclarationAssignment {
			name: Cow::Borrowed("z"),
			typ: NativeType::Int,
			value: StructlessStatement::Num(5.into()),
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
			value: StructlessStatement::Num(5.into()),
			is_static: false,
			is_const: false,
			is_volatile: false,
		},
		StructlessLanguage::VariableDeclarationAssignment {
			name: Cow::Borrowed("y"),
			typ: NativeType::Int,
			value: StructlessStatement::BinOp {
				op: BinOp::Add,
				lhs: Box::new(StructlessStatement::Num(1.into())),
				rhs: Box::new(StructlessStatement::Var(Cow::Borrowed("x"))),
				signedness: NumberType::Unknown,
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
			value: StructlessStatement::Num(5.into()),
			is_const: true,
			is_static: false,
			is_volatile: false,
		},
		StructlessLanguage::Return(Some(StructlessStatement::BinOp {
			op: BinOp::Add,
			lhs: Box::new(StructlessStatement::Var(Cow::Borrowed("x"))),
			rhs: Box::new(StructlessStatement::Num(2.into())),
			signedness: NumberType::Unknown,
		})),
	];
	let expected_1_mid = vec![
		StructlessLanguage::VariableDeclarationAssignment {
			typ: NativeType::Int,
			name: Cow::Borrowed("x"),
			value: StructlessStatement::Num(5.into()),
			is_const: true,
			is_static: false,
			is_volatile: false,
		},
		StructlessLanguage::Return(Some(StructlessStatement::BinOp {
			op: BinOp::Add,
			lhs: Box::new(StructlessStatement::Num(5.into())),
			rhs: Box::new(StructlessStatement::Num(2.into())),
			signedness: NumberType::Unknown,
		})),
	];
	let expected_1_end = vec![StructlessLanguage::Return(Some(StructlessStatement::Num(
		7.into(),
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
				value: StatementElement::Num(3.into()),
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
				value: StructlessStatement::Num(3.into()),
			}],
		},
	];
	let res_1 = StructlessLanguage::from_language_elements(case_1).unwrap();
	assert_eq!(expected_1, res_1);
}

#[test]
fn signedness_logic() {
	for ux in 0..=u8::MAX {
		let ix = ux as i8;
		for uy in 1..=u8::MAX {
			let iy = uy as i8;
			assert_eq!(
				ux.wrapping_add(uy),
				ix.wrapping_add(iy) as u8,
				"{} {}",
				ux,
				uy
			);
			assert_eq!(
				ux.wrapping_sub(uy),
				ix.wrapping_sub(iy) as u8,
				"{} {}",
				ux,
				uy
			);
			assert_eq!(
				ux.wrapping_mul(uy),
				ix.wrapping_mul(iy) as u8,
				"{} {}",
				ux,
				uy
			);
		}
		assert_eq!(ux.wrapping_add(0), ix.wrapping_add(0) as u8, "{} 0", ux,);
		assert_eq!(ux.wrapping_sub(0), ix.wrapping_sub(0) as u8, "{} 0", ux,);
		assert_eq!(ux.wrapping_mul(0), ix.wrapping_mul(0) as u8, "{} 0", ux,);
	}
}

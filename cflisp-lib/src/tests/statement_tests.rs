#![allow(unused_imports)]
use super::super::*;
use std::borrow::Cow;

#[test]
fn fast_mul() {
	let x = Cow::Borrowed("x");

	let mut case_1 = StatementElement::Mul {
		lhs: Box::new(StatementElement::Num(5)),
		rhs: Box::new(StatementElement::Num(6)),
	};
	let expected_1 = StatementElement::Num(5 * 6);
	optimise_statement::fast_mul(&mut case_1);
	assert_eq!(case_1, expected_1);

	let mut case_2 = StatementElement::Mul {
		lhs: Box::new(StatementElement::Num(0b10101)),
		rhs: Box::new(StatementElement::Var(x.clone())),
	};
	let expected_2 = StatementElement::Add {
		lhs: Box::new(StatementElement::Add {
			lhs: Box::new(StatementElement::Var(x.clone())),
			rhs: Box::new(StatementElement::LShift {
				lhs: Box::new(StatementElement::Var(x.clone())),
				rhs: Box::new(StatementElement::Num(2)),
			}),
		}),
		rhs: Box::new(StatementElement::LShift {
			lhs: Box::new(StatementElement::Var(x.clone())),
			rhs: Box::new(StatementElement::Num(4)),
		}),
	};
	optimise_statement::fast_mul(&mut case_2);
	assert_eq!(case_2, expected_2);
}

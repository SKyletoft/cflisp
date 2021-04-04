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
}

#![allow(unused_imports)]
use super::super::*;
use std::collections::{HashMap, HashSet};

#[test]
fn integers() {
	let case_1 = StatementElement::Num(1);
	let expected_1 = Ok(true);
	let res_1 =
		type_checker::statement_element(&case_1, &HashSet::new(), &HashSet::new(), &HashMap::new());
	assert_eq!(res_1, expected_1);
}

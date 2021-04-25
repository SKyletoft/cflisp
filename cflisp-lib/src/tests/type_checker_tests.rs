#![allow(unused_imports)]
use super::super::*;
use std::collections::{HashMap, HashSet};

#[test]
fn integers() {
	let case_1 = StatementElement::Num(1);
	type_checker::statement_element(&case_1, &HashMap::new(), &HashMap::new(), &HashMap::new())
		.unwrap();
}

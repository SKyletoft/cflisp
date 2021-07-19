#![allow(unused_imports)]
use std::collections::{HashMap, HashSet};

use super::super::*;

#[test]
fn integers() {
	let case_1 = StatementElement::Num(1.into());
	type_checker::statement_element(&case_1, &HashMap::new(), &HashMap::new(), &HashMap::new())
		.unwrap();
}

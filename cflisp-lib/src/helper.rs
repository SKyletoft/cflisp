use std::borrow::Cow;

pub(crate) fn merge_name_and_field<'a>(name: &str, field: &str) -> Cow<'a, str> {
	Cow::Owned(name.to_string() + "::" + field)
}

#[macro_export]
macro_rules! merge_comments {
	($only:expr) => {
		$only.clone()
	};
	($lhs:expr, $($rhs:expr), +) => {{
		let lhs: &Option<Cow<str>> = $lhs;
		let rest = merge_comments!($($rhs), +);
		if let (Some(l), Some(r)) = (lhs, &rest) {
			Some(Cow::Owned(l.to_string() + ", " + &r))
		} else {
			$lhs.clone().or_else(|| rest.clone())
		}
	}};
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

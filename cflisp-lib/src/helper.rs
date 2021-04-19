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

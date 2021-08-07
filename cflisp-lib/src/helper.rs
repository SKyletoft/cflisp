use std::{borrow::Cow, fmt};

pub(crate) fn merge_name_and_field<'a>(name: &str, field: &str) -> Cow<'a, str> {
	Cow::Owned(format!("{}::{}", name, field))
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
			Some(Cow::Owned(format!("{}, {}", l, &r)))
		} else {
			$lhs.clone().or_else(|| rest.clone())
		}
	}};
}

pub(crate) fn write_token_slice<T: fmt::Display>(
	tokens: &[T],
	f: &mut fmt::Formatter<'_>,
	separator: &str,
) -> fmt::Result {
	if let [start @ .., end] = tokens {
		for token in start.iter() {
			write!(f, "{}{}", token, separator)?;
		}
		write!(f, "{}", end)
	} else {
		Ok(())
	}
}

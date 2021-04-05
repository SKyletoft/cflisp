use std::borrow::Cow;

pub(crate) fn merge_name_and_field<'a>(name: &str, field: &str) -> Cow<'a, str> {
	Cow::Owned(name.to_string() + "::" + field)
}

use std::iter::FromIterator;

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Flags {
	pub(crate) hex: bool,
	pub(crate) comments: bool,
	pub(crate) tree: bool,
	pub(crate) type_check: bool,
	pub(crate) print_result: bool,
	pub(crate) assemble: bool,
	pub(crate) out: String,
}

impl<'a> FromIterator<&'a String> for Flags {
	fn from_iter<T: IntoIterator<Item = &'a String>>(iter: T) -> Self {
		let mut flags = Flags {
			hex: false,
			comments: true,
			tree: false,
			type_check: false,
			print_result: false,
			assemble: false,
			out: "out.sflisp".to_string(),
		};
		let mut is_name = false;
		for arg in iter {
			if is_name {
				assert!(!arg.starts_with('-'), "Invalid file name");
				flags.out = arg.to_owned();
				is_name = false;
				continue;
			}
			if arg.starts_with('-') {
				if arg.contains('h') {
					flags.hex = true;
				} else if arg.contains('d') {
					flags.hex = false;
				}
				if arg.contains('c') {
					flags.comments = false;
				}
				if arg.contains('p') {
					flags.tree = true;
				}
				if arg.contains('t') {
					flags.type_check = true;
				}
				if arg.contains('s') {
					flags.print_result = true;
				}
				if arg.contains('a') {
					flags.assemble = true;
				}
			}
			if arg == "-o" {
				is_name = true;
			}
		}
		flags
	}
}

use std::iter::FromIterator;

///Container for all the compiler flags passed in from Args
#[derive(Clone, Debug, PartialEq)]
pub struct Flags {
	pub hex: bool,
	pub comments: bool,
	pub tree: bool,
	pub tree_structless: bool,
	pub type_check: bool,
	pub print_result: bool,
	pub assemble: bool,
	pub out: String,
	pub debug: bool,
	pub optimise: u8,
	pub inline: bool,
	pub kill_interrupts: bool,
}

impl Default for Flags {
	fn default() -> Self {
		Flags {
			hex: false,
			comments: true,
			tree: false,
			tree_structless: false,
			type_check: true,
			print_result: false,
			assemble: false,
			debug: false,
			optimise: 0,
			inline: false,
			kill_interrupts: true,
			out: "out.sflisp".to_string(),
		}
	}
}

impl<'a> FromIterator<&'a String> for Flags {
	fn from_iter<T: IntoIterator<Item = &'a String>>(iter: T) -> Self {
		let mut flags = Flags::default();
		let mut is_name = false;
		for arg in iter {
			if is_name {
				assert!(!arg.starts_with('-'), "Invalid file name");
				flags.out = arg.to_owned();
				is_name = false;
				continue;
			}
			if arg.starts_with('-') {
				if arg.contains('x') {
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
				if arg.contains('P') {
					flags.tree_structless = true;
				}
				if arg.contains('t') {
					flags.type_check = false;
				}
				if arg.contains('s') {
					flags.print_result = true;
				}
				if arg.contains('a') {
					flags.assemble = true;
				}
				if arg.contains('g') {
					flags.debug = true;
				}
				if arg.contains('i') {
					flags.inline = true;
				}
				if arg.contains('k') {
					flags.kill_interrupts = false;
				}
				if arg.contains('O') {
					flags.optimise = 2;
				}
			}
			if arg == "-o" {
				is_name = true;
			}
			if arg == "-O2" {
				flags.optimise = 2;
			}
			if arg == "-O1" {
				flags.optimise = 1;
			}
		}
		flags
	}
}

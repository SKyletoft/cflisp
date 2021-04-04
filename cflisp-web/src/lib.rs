use wasm_bindgen::prelude::*;

use cflisp_lib::{
	compile_flisp, flags::Flags, optimise_flisp, optimise_language, parser,
	structless::LanguageElementStructless, text,
};

#[wasm_bindgen]
pub fn run(
	source: &str,
	optimise: u8,
	type_check: bool,
	debug: bool,
	hex: bool,
	comments: bool,
	show_imports: bool,
) -> String {
	match compile_to_text(
		source,
		optimise,
		type_check,
		debug,
		hex,
		comments,
		show_imports,
	) {
		Ok(res) => res,
		Err(res) => res,
	}
}

fn compile_to_text(
	source: &str,
	optimise: u8,
	type_check: bool,
	debug: bool,
	hex: bool,
	comments: bool,
	show_imports: bool,
) -> Result<String, String> {
	let flags = Flags {
		hex,
		comments,
		tree: false,
		tree_structless: false,
		type_check,
		print_result: false,
		assemble: false,
		out: String::new(),
		debug,
		optimise: optimise.min(2).max(0),
		inline: false,
	};
	let stripped_comments = parser::remove_comments(source);
	let tree =
		parser::parse(&stripped_comments, flags.debug).map_err(|err| format!("{:?}", err))?;
	let mut structless = LanguageElementStructless::from_language_elements(tree)
		.map_err(|err| format!("{:?}", err))?;
	if optimise >= 2 {
		optimise_language::all_optimisations(&mut structless)
			.map_err(|err| format!("{:?}", err))?;
	}
	let mut inst =
		compile_flisp::compile(&structless, &flags).map_err(|err| format!("{:?}", err))?;
	if optimise >= 1 {
		optimise_flisp::all_optimisations(&mut inst).map_err(|err| format!("{:?}", err))?;
	}
	let mut text = text::instructions_to_text(&inst, &flags).map_err(|err| format!("{:?}", err))?;
	if show_imports {
		text::automatic_imports(&mut text, debug);
	}
	Ok(text)
}

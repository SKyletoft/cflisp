#![allow(clippy::too_many_arguments)]

use cflisp_lib::{
	flags::Flags,
	flisp::{compile_flisp, optimise_flisp, text},
	optimise_language, parser,
	structless_language::StructlessLanguage,
	type_checker,
};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn add(a: i32, b: i32) -> i32 {
	a + b
}

#[wasm_bindgen]
pub fn run_cc(
	source: &str,
	optimise: u8,
	type_check: bool,
	debug: bool,
	hex: bool,
	comments: bool,
	show_imports: bool,
	continue_interrupts: bool,
	output_type: u8,
) -> String {
	match compile_to_text(
		source,
		optimise,
		type_check,
		debug,
		hex,
		comments,
		show_imports,
		continue_interrupts,
		output_type,
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
	continue_interrupts: bool,
	output_type: u8,
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
		kill_interrupts: !continue_interrupts,
	};
	let stripped_comments = parser::remove_comments(source);
	let tree = parser::parse(&stripped_comments, flags.debug).map_err(|err| format!("{}", err))?;
	if type_check {
		type_checker::type_check(&tree).map_err(|err| format!("{}", err))?;
	}
	if output_type == 1 {
		return Ok(format!("{:#?}", tree));
	}
	if output_type == 2 {
		return Ok(format!(
			"{}",
			cflisp_lib::language_element::LanguageBlock(&tree)
		));
	}
	let mut structless =
		StructlessLanguage::from_language_elements(tree).map_err(|err| format!("{}", err))?;
	if optimise >= 2 {
		optimise_language::all_optimisations(&mut structless).map_err(|err| format!("{}", err))?;
	}
	if output_type == 3 {
		return Ok(format!("{:#?}", structless));
	}
	if output_type == 4 {
		return Ok(format!(
			"{}",
			cflisp_lib::structless_language::StructlessLanguage::Block {
				block: structless,
				scope_name: "global".into(),
			}
		));
	}
	let mut inst = compile_flisp::compile(&structless, &flags).map_err(|err| format!("{}", err))?;
	if optimise >= 1 {
		optimise_flisp::all_optimisations(&mut inst).map_err(|err| format!("{}", err))?;
	}
	if !debug {
		optimise_flisp::remove_unused_labels(&mut inst);
		optimise_flisp::repeat_rts(&mut inst);
	}
	let mut text = text::instructions_to_text(&inst, &flags).map_err(|err| format!("{}", err))?;
	if show_imports {
		text::automatic_imports(&mut text, debug, flags.kill_interrupts);
	}
	if flags.debug {
		text.insert_str(0, "\tORG\t$20\n");
	}
	if output_type == 0 {
		return Ok(text);
	}
	Err("Front end error: Invalid index from dropdown menu".to_string())
}

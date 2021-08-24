use std::{
	borrow::Cow,
	collections::{HashMap, HashSet},
	fs,
};

use super::*;
use crate::*;

///Removes all comments from the source code.
/// Recursive multiline comments are treated properly.
pub fn remove_comments(s: &str) -> Cow<str> {
	let cow_str = Cow::Borrowed(s);
	let no_multilines = remove_multiline_comments(cow_str);
	remove_single_line_comments(no_multilines)
}

///Takes the entire source code and removes the rest of the line for each line with a `//`.
fn remove_single_line_comments(mut s: Cow<str>) -> Cow<str> {
	//Could be sped up if we didn't start the search over each time,
	// but apparently I can't write code that doesn't crash
	while let Some(idx) = s.find("//") {
		let end = s[idx..]
			.find('\n')
			.map(|v| v + idx)
			.unwrap_or_else(|| s.len());
		match &mut s {
			Cow::Owned(slice) => slice.replace_range(idx..end, ""),
			Cow::Borrowed(slice) => s = Cow::Owned(slice[..idx].to_string() + &slice[end..]),
		}
	}
	s
}

///Takes the entire source code and removes everything between `/*` and `*/`.
/// Works with recursive comments.
/// Should probably be run before the single line version?
fn remove_multiline_comments(mut s: Cow<str>) -> Cow<str> {
	const OPEN: &str = "/*";
	const CLOSE: &str = "*/";
	loop {
		//Find the first comment ending and find the last start before it,
		// remove everything between them, repeat till there are no comments.
		if s.is_empty() {
			return s;
		}
		let first_end = s.find(CLOSE);
		//No end: return early, even if it might be unbalanced
		if first_end.is_none() {
			return s;
		}
		let first_end = first_end.unwrap();
		let mut latest_start = 0;
		let mut maybe_latest_start = s[latest_start..first_end].find(OPEN);
		//No start: return as it is, even though we know it's unbalanced
		if maybe_latest_start.is_none() {
			return s;
		}
		while let Some(start) = maybe_latest_start {
			latest_start += start + 2;
			maybe_latest_start = s[latest_start..first_end].find(OPEN);
		}
		if let Cow::Owned(s) = &mut s {
			//Replace with `remove_range` is something like that ever gets stabilised?
			let from = latest_start - 2;
			let to = first_end + 2;
			s.replace_range(from..to, "");
		} else {
			s = Cow::Owned(s[..latest_start - 2].to_string() + &s[first_end + 2..]);
		}
	}
}

pub fn preprocess(s: &str) -> Result<String> {
	let mut defines = HashMap::new();
	defines.insert("flisp".into(), "".into());
	let mut included = HashSet::new();
	let mut out = String::new();
	preproc(s, &mut out, &mut defines, &mut included, "original source")?;
	Ok(out)
}

fn get_def_and_rest(mut line: &str) -> (&str, &str) {
	line = line.trim();
	line.split_once(char::is_whitespace).unwrap_or((line, ""))
}

fn preproc(
	s: &str,
	out: &mut String,
	defines: &mut HashMap<String, String>,
	included: &mut HashSet<String>,
	file_name: &str,
) -> Result<()> {
	let mut skipping = false;
	let source = remove_comments(s);
	included.insert(file_name.into());
	for mut line in source.lines() {
		line = line.trim();
		if line.starts_with("#endif") {
			skipping = false;
			continue;
		}
		if skipping {
			continue;
		}
		if let Some(line) = line.strip_prefix("#pragma") {
			let (def, rest) = get_def_and_rest(line);
			if !rest.is_empty() {
				return Err(error!(ExcessTokens, line));
			}
			if def == "noflisp" || (def == "once" && included.contains(file_name)) {
				return Ok(());
			}
		} else if let Some(line) = line.strip_prefix("#define") {
			let (def, rest) = get_def_and_rest(line);
			defines.insert(def.into(), rest.into());
		} else if let Some(mut line) = line.strip_prefix("#include") {
			line = line.trim();
			let file = line
				.get(1..line.len().wrapping_sub(1))
				.ok_or(error!(InvalidImport, line))?;
			let content: Cow<str> = if line.starts_with('<') && line.ends_with('>') {
				match file {
					"builtins.h" => include_str!("../flisp/std_headers/builtins.h"),
					"mirror.h" => include_str!("../flisp/std_headers/mirror.h"),
					"swap.h" => include_str!("../flisp/std_headers/swap.h"),
					"stdin.h" => include_str!("../flisp/std_headers/stdin.h"),
					"stdout.h" => include_str!("../flisp/std_headers/stdout.h"),
					"stdio.h" => include_str!("../flisp/std_headers/stdio.h"),
					//"long.h" => include_str!("../flisp/std_headers/long.h"),
					"flisp_imports.h" => include_str!("../flisp/std_headers/flisp_imports.h"),
					_ => return Err(error!(InvalidImport, line)),
				}
				.into()
			} else if line.starts_with('"') && line.ends_with('"') && line.len() >= 2 {
				fs::read_to_string(file)
					.map_err(|_| error!(InvalidImport, line))?
					.into()
			} else {
				return Err(error!(InvalidImport, line));
			};
			preproc(&content, out, defines, included, file)?;
		} else if let Some(line) = line.strip_prefix("#ifdef") {
			let (def, _) = get_def_and_rest(line);
			if !defines.contains_key(def) {
				skipping = true;
			}
		} else if let Some(line) = line.strip_prefix("#ifndef") {
			let (def, _) = get_def_and_rest(line);
			if defines.contains_key(def) {
				skipping = true;
			}
		} else {
			//Applies defines
			for word in line.split_inclusive(|c: char| {
				c.is_whitespace() || lexer::FORBIDDEN_CHARACTERS.contains(&c)
			}) {
				let shortened_word = word
					.strip_suffix(|c: char| {
						c.is_whitespace()
							|| (lexer::FORBIDDEN_CHARACTERS.contains(&c)
								&& !word
									.starts_with(|cc| lexer::FORBIDDEN_CHARACTERS.contains(&cc)))
					})
					.unwrap_or(word);
				let suffix = &word[shortened_word.len()..];
				let defined_as = defines
					.get(shortened_word)
					.map(String::as_str)
					.unwrap_or(shortened_word);
				let replacement = if defined_as.is_empty() {
					shortened_word
				} else {
					defined_as
				};
				out.push_str(replacement);
				out.push_str(suffix);
			}
			out.push('\n');
		}
	}
	Ok(())
}

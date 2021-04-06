import init, {add, run_cc} from "../pkg/cflisp_web.js";

const resultWindow = document.getElementById("result");

const editor = ace.edit("source");
editor.session.setMode("ace/mode/c_cpp");

let last_state;

async function run() {
	await init();
	if (add(1, 2) !== 3) { throw new Error("wasm doesn't work!"); }

	function compile_and_write() {
		const source     = editor.getValue();
		const opt        = document.getElementById("opt").value;
		const type_check = document.getElementById("type_check").checked;
		const debug      = document.getElementById("debug").checked;
		const hex        = document.getElementById("hex").checked;
		const comments   = document.getElementById("comments").checked;
		const imports    = document.getElementById("imports").checked;

		const state = {
			source : source,
			opt : opt,
			type_check : type_check,
			debug : debug,
			hex : hex,
			comments : comments,
			imports : imports
		};

		if (state != last_state) {
			resultWindow.value = run_cc(source, opt, type_check, debug, hex, comments, imports);
		}
		last_state = state;
	}

	setInterval(compile_and_write, 250);
}

run();

const default_c =
    "//Try changing the compiler settings!\nint main() {\n\tint x = (5 + 3) * 2;\n\tif (x == 2) {\n\t\treturn 0;\n\t} else {\n\t\treturn -1;\n\t}\n}";
editor.setValue(default_c);
editor.clearSelection();
import init, {add, run_cc} from "../pkg/cflisp_web.js";

let last_state;

async function run() {
	await init();
	if (add(1, 2) !== 3) throw new Error("wasm doesn't work!");

	function compile_and_write() {
		const source     = document.getElementById("source").value;
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
			document.getElementById("result").value = run_cc(source, opt, type_check, debug, hex, comments, imports);
		}
		last_state = state;
	}

	setInterval(compile_and_write, 250);
}

run();

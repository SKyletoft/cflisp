import init, {add, run_cc} from "../pkg/cflisp_web.js";

async function run() {
	await init();
	if (add(1, 2) !== 3) throw new Error("wasm doesn't work!");

	function compile_and_write() {
		const source                            = document.getElementById("source").value;
		const opt                               = document.getElementById("opt").value;
		const type_check                        = document.getElementById("type_check").checked;
		const debug                             = document.getElementById("debug").checked;
		const hex                               = document.getElementById("hex").checked;
		const comments                          = document.getElementById("comments").checked;
		const imports                           = document.getElementById("imports").checked;
		const result                            = run_cc(source, opt, type_check, debug, hex, comments, imports);
		document.getElementById("result").value = result;
	}

	document.getElementById("run_btn").addEventListener("click", compile_and_write);
}

run();
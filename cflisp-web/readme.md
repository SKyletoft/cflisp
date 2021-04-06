# CFLISP-WEB
Godbolt-like webwrapper for cflisp. Runs locally through wasm instead of sending all your code to the server.

The editor is ace.js by Ajax.org used under their BSD-3 license

## Build
Requires rust wasm pack. (Link in top level readme)

`wasm-pack build --target=web` or else the generated .wasm file won't be accepted by the web browser
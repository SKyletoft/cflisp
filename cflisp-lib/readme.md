# CFLISP-LIB
The actual backbone of the project.

## Current project structure
Text is tokenised in `lexer.rs`. This tokenisation is not complete however and parentheses and brackets still contain their contents unparsed as a string.
There are two types of tokens. The general `Token` type that can be any kind of token and `StatementToken`, a reduced set of tokens that are only valid in right hand side statements. This means control flow tokens and types are excluded. This distinction is made to prevent invalid states from being represented at all as soon as possible.

Tokens are matched against a list of valid language constructs in `parser.rs` (and the right hand side expressions in `statement_element.rs`) and transformed into a tree structure of `LanguageElement`s. This version of the tree can contain structs. Structs are later removed and converted into native types and `LanguageElement`s and `StatementElements` are converted into their structless variants (`structless_language.rs` and `structless_element.rs`).

If enabled, O2 optimisation is performed on these trees before they are compiled down into a FLISP specific representation in `compile_flisp.rs`. The definition of said representation is found in `flisp_instructions.rs`. Unfortunately the current design allows for illegal instructions as any instruction can be paired with any addressing method, something that the flisp machine does not support.

After it has been compiled down to flisp instructions O1 optimisation is performed (if enabled) which basically just fixes overly general code generation down to more specialised versions.

As of now flisp instructions can only be exported as text (`text.rs`) and then passed on to the official `qaflisp` assembler. There are no current plans to implement an assembler for internal instruction representation.

## Tests
`tests/` currently contains a few tests for the project. `clfisp-cli` contains legacy tests that are just example c files that must be checked manually.

## ABI

*Function call that returns a primitive*

* Return value lives in the `A` register

Stack | Starts at 0xFC and grows down
-|-
Function arguments | Cleared by caller
Return value | Cleared by `RTS` instruction
Locals | Cleared by callee

*Function call that returns void*
Stack | Starts at 0xFC and grows down
-|-
Function arguments | Cleared by caller
Return value | Cleared by `RTS` instruction
Locals | Cleared by callee

*Function call that returns a struct*
Stack | Starts at 0xFC and grows down
-|-
Space for struct | Local in the caller's scope
Function arguments | Cleared by caller
Return pointer | Cleared by `RTS` instruction
Locals | Cleared by callee


Functions that return a struct are transformed from `Foo fn()` to `void fn (Foo *__ret__)` and pointer writes are inserted before each return. Because of this all functions that return a struct must be stored immediately to a variable and cannot be a part of a greater expression.

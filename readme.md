# CFLISP


An experimental C compiler for Digiflisp written in Rust.

Parses and compiles a subset of C to flisp assembly. All supported types are one byte large. Non native operators are supported via function calls. The example below is what you should expect when compiling with optimisations. `float`s and `double`s are not supported at all, nor is the preprocessor.
`struct`s are supported, but cannot be nested or returned from functions.

Basic maths, function calls (even recursive function calls), if-else-statements (even if-else-if-else-statements), for and while (not do-while) loops, arbitrary levels of pointers, comments, hex and decimal printing are all supported and tested features. The ternary operator is missing. On Linux you can even call qaflisp to assemble it directly (untested on Windows/Mac).

Interrupts are supported. The `interrupt` function (`void interrupt()`) is called when triggered. The interrupt enable flag is set automatically if the interrupt function exists.

## How do I run it?
Install the rust toolchain from rust-lang.org, clone this repo and then just run `cargo run --release` from the cflisp-cli folder. Alternatively there is a WIP web interface like Godbolt's Compiler Explorer.

## Why rust?
Rust is known for its correctness and safety guarantees without sacrificing performance. Here, however, rust was just chosen because the type system is really nice and idiomatic rust allows for some very functional style code without being fully functional. And performance doesn't hurt.

```c
//  legacy_tests/test1.c

int main() {
    int *ptr = 0;
    int x    = 5;
    if (x == 5) {
        x = 3;
    }
    *ptr = x;
}
```

into this:

```asm
; legacy_tests/expected1.sflisp
        ORG     $20
main    LDA     #$00
        PSHA            ; ptr
        LDA     #$05
        PSHA            ; x
        CMPA    #$05
        BNE     if_end_main_2
if_then_main_2
        LDA     #$03
        STA     $00,SP  ; x
if_end_main_2
        LDX     $01,SP  ; ptr
        LDA     $00,SP  ; x
        STA     $00,X
        LEASP   $02,SP  ; Clearing variables
        RTS

__init_ LDA     #0
        LDX     #0
        LDY     #0
        LDSP    #$FB
        JSR     main
__end__ JMP     __end__


        ORG     $FF
        FCB     __init_
```

## Compiler flags
Flag | Name | Description
--|--|--
x | hex | Prints numbers as hex instead
c | comments | Disables comments (variable names mostly)
p|tree|Prints the parsed tree structure of the program
P|tree_structless|Prints the parsed tree after structs have been simplified down to native types and after -O2 IR optimisations have been performed
t|type_check| Disables type checking
s|print_result|Prints the compiled code to stdout instead of a file
a|assemble|Calls qaflisp directly. Requires qaflisp to be on $PATH. Probably doesn't work on Windows
o|out|Sets the name of the output file (following argument)
g|debug|Adds debug info. Disables removal of unused names and adds `ORG $20` to the top for easier digiflisp debugging
O (/O1/O2)|optimise| Enables optimisations. In practice it mostly just fixes bad code generation rather than actually optimising the code. Needs to be its own flag and cannot be grouped with the others (Capital o, not zero)
k|kill_interrupts|Disables interrupts after main has exited

## Dependencies
### Cflisp-lib
* [Build] Rust/Cargo: https://www.rust-lang.org/tools/install
### Cflisp-cli
* [Runtime] Digiflisp (qaflisp): http://www.gbgmv.se/studies.html#compeng
### Cflisp-web
* [Build] WASM-Pack: https://rustwasm.github.io/wasm-pack/
* [Runtime] Any recent browser with proper WASM support

## Todo
(additional features still under consideration)
* Line numbers for parse errors
* Inline assembly
* `-a` flag on Windows and Mac
* Figure out licensing
* Implement own flisp simulator (16 bit addressing?)
* New ABI. (Multibyte size variables, return structs from functions)
* casts

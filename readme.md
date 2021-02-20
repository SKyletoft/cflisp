## CFLISP

An experimental C compiler for Digiflisp written in Rust.

Parses and compiles a subset of C to flisp assembly. All supported types are one byte large. Non native operators are supported via function calls. The example below is what you should expect when compiling with optimisations. `struct`s, `float`s and `double`s are not supported at all, nor is the preprocessor.

Basic maths, function calls (even recursive function calls), if-else-statements (even if-else-if-else-statements), for and while (not do-while) loops, arbitrary levels of pointers, comments, hex and decimal printing are all supported and tested features. On Linux you can even call qaflisp to assemble it directly (untested on Windows/Mac)

```c
//  tests/test1.c

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

```flisp
; tests/expected1.sflisp
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
        FCB     __init_```

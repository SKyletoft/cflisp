## CFLISP

An experimental C compiler for Digiflisp written in Rust.

Can currently parse a subset of C. Full ISO-C compliance is not planned. `float`s and `doubles` will never be supported as they lack support on the target hardware. `struct`s are not supported either. The goal is to transform this:

```c
//  tests/test1.c

int main() {
    int* ptr = 0;
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
main    LDA     #000
        PSHA             ; ptr
        LDA     #005
        PSHA             ; x
        LDA     000,SP   ; x
        CMPA    #005
        BNE     if_end_main_2
if_then_main_2
        LDA     #003
        STA     000,SP   ; x
if_end_main_2
        LDX     001,SP   ; ptr
        LDA     000,SP   ; x
        STA     000,X
        LEASP   002,SP
        RTS

init    LDA     #0
        LDX     #0
        LDY     #0
        LDSP    #$FB
        JSR     main
end     JMP     end

        ORG     $FF
        FCB     init```

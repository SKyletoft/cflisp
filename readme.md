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
main    ORG     $20
        LDA     #0
        PSHA            ; ptr
        LDA     #5
        PSHA            ; x
        CMPA    #5
        BNE     main_1
        LDA     #3
        STA     SP, 0
main_1  LDA     SP, 0
        STA     SP, -1
        JMP     end

end     JMP     end

        ORG     $FF
        FCB     main
```

# Generated Assembly Output

Given the following source code:

```c
// Source Code

def main(x: integer) -> void {
   let num = 1299 + 1299;
   let num2 = 1299;
}
```

The compiler generates the following AArch64 assembly:

```assembly
.global _main
_main:
sub sp, sp, #32
str x0, [sp, #24]
MOV w8, 1299
MOV w9, 1299
ADD w10, w8, w9
str w10, [sp, #16]
MOV w11, 1299
str w11, [sp, #8]
add sp, sp, #32
ret
```

This output represents the compiled function with stack management, immediate values, and basic arithmetic operations.
# bichara

## Poor C-like language Compiler in Rust.

### Example Input
```c
global int c;
c = 5;
```

### Output
```asm
.data
c: .word 0 // int c;
.text
.global _main
_main:
mov x0, 5
ldr x3, =c
str x0, [x3]
mov x0, 0
mov x16, 1
svc 0x80
```
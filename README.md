# bichara

## Poor C-like language Compiler in Rust.

### Run
```shell
$ cargo run
```

### Test
```shell
$ cargo test
```

### Example Input
```c
global int a; 
a = 4 + 5; 
global int b;
b = 4;
```

### Output
```asm
.data
	.align 2
.L2:
	.word 0
	.word 0

.text
mov x4, 4
mov x3, 5
add x4, x4, x3
adrp x3, .L2+0@PAGE
add x3, x3, .L2+0@PAGEOFF
str x4, [x3]
mov x1, 4
adrp x8, .L2+4@PAGE
add x8, x8, .L2+4@PAGEOFF
str x1, [x8]
mov x0, 0
mov x16, 1
svc 0x80
```
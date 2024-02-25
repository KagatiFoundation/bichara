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
No bound check
```c
global integer nums[5];
global integer value;
value = nums[0] + 12;
```

### Output
```asm
.data
.global nums
nums:
.word 0
.word 0
.word 0
.word 0
.word 0
.data
.global value
value: .align 4
	.word 0

.text
mov x5, 0
mov x2, 0
adrp x3, nums@PAGE
add x3, x3, nums@PAGEOFF
ldr x8, [x3, x2, lsl 3]
mov x1, 12
add x8, x8, x1
adrp x1, value@PAGE
add x1, x1, value@PAGEOFF
str x8, [x1]
mov x0, 0
mov x16, 1
svc 0x80
```

### This compiler doesn't support array assignment yet
i.e., for e.g., 'array[1] = 12;' is not supported.

### Go to ./samples for more examples

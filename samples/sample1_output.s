_L0: .ascii "KagatiFoundation"
.data
.global name
name: .align 8
	.quad 0

.text
adrp x0, _L0@PAGE
add x0, x0, _L0@PAGEOFF
adrp x4, name@PAGE
add x4, x4, name@PAGEOFF
str x0, [x4]
mov x0, 0
mov x16, 1
svc 0x80

	.text
.globl _start
_start:
	push $10
	push $20
	pop %rax
	pop %rbx
	add %rbx, %rax
	push %rax
	mov $0x3c, %rax
	mov $0, %rdi
	syscall

	.text
.globl	_start
_start:
	push $57
	push $1
	pop %rax
	pop %rbx
	sub %rax, %rbx
	push %rbx
	pop %rax
	sub $3, %rsp
	movb $0, 2(%rsp)
	movb $10, 1(%rsp)
	movb %al, (%rsp)
	mov $1, %rax
	mov $1, %rdi
	lea (%rsp), %rsi
	mov $2, %rdx
	syscall
	mov $0x3c, %rax
	mov $0, %rdi
	syscall

	.text
.globl	_start
_start:
	push $4
	push $2
	pop %rcx
	pop %rax
	cqto
	idivq %rcx
	push %rcx
	push $48
	pop %rax
	pop %rbx
	add %rbx, %rax
	push %rax
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

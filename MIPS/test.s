.text
	.globl	main
main:
	move $fp, $sp
	addiu $sp, $sp, 0
	sw $fp, -8($sp)
	j funAntes1
Funt:
	lw $t0, ($sp)
	addiu $sp, $sp, 4
	sw $t0, t
	lw $t0, t
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	li $t0, 2
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	lw $t2, ($sp)
	addiu $sp, $sp, 4
	lw $t1, ($sp)
	addiu $sp, $sp, 4
	add $t0, $t1, $t2
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	jr $ra
funAntes1:
	lw $t0, ($sp)
	addiu $sp, $sp, 4
	sw $t0, z
	li $t0, 0
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	lw $t0, ($sp)
	addiu $sp, $sp, 4
	sw $t0, x
	li $t0, 3
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	lw $t0, ($sp)
	addiu $sp, $sp, 4
	lw $t0, ($sp)
	addiu $sp, $sp, 4
	sw $t0, a
	li $t0, 2
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	lw $t0, ($sp)
	addiu $sp, $sp, 4
	sw $t0, x
	lw $t0, x
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	lw $t3, ($sp)
	addiu $sp, $sp, 4
	li $t0, 0
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	lw $t1, ($sp)
	addiu $sp, $sp, 4
	blt $t3, $t1, fContinuar1
For1:
	lw $t0, x
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	lw $t0, x
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	jal Funt
	lw $t2, ($sp)
	addiu $sp, $sp, 4
	lw $t1, ($sp)
	addiu $sp, $sp, 4
	la $t3, array1
	li $t0, 4
	mul $t1, $t1, $t0
	add $t3, $t3, $t1
	sw $t2, 0($t3)
	lw $t0, x
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	li $t0, 0
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	li $t0, 1
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	lw $t2, ($sp)
	addiu $sp, $sp, 4
	lw $t1, ($sp)
	addiu $sp, $sp, 4
	sub $t0, $t1, $t2
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	lw $t2, ($sp)
	addiu $sp, $sp, 4
	lw $t1, ($sp)
	addiu $sp, $sp, 4
	add $t0, $t1, $t2
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	lw $t0, ($sp)
	addiu $sp, $sp, 4
	sw $t0, x
	lw $t0, ($sp)
	addiu $sp, $sp, 4
	lw $t0, x
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	lw $t2, ($sp)
	addiu $sp, $sp, 4
	li $t0, 0
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	lw $t1, ($sp)
	addiu $sp, $sp, 4
	blt $t2, $t1, fContinuar1
	j For1
fContinuar1:
	li $t0, 0
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	lw $t0, ($sp)
	addiu $sp, $sp, 4
	sw $t0, x
	lw $t0, x
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	lw $t3, ($sp)
	addiu $sp, $sp, 4
	li $t0, 2
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	lw $t1, ($sp)
	addiu $sp, $sp, 4
	bgt $t3, $t1, fContinuar2
For2:
	lw $t0, x
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	lw $t1, ($sp)
	addiu $sp, $sp, 4
	la $t3, array1
	li $t0, 4
	mul $t1, $t1, $t0
	add $t3, $t3, $t1
	lw $t0, 0($t3)
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	li $v0, 1
	lw $a0, ($sp)
	addiu $sp, $sp, 4
	syscall
	li $v0, 4
	la $a0, newline
	syscall
	lw $t0, x
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	li $t0, 1
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	lw $t2, ($sp)
	addiu $sp, $sp, 4
	lw $t1, ($sp)
	addiu $sp, $sp, 4
	add $t0, $t1, $t2
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	lw $t0, ($sp)
	addiu $sp, $sp, 4
	sw $t0, x
	lw $t0, ($sp)
	addiu $sp, $sp, 4
	lw $t0, x
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	lw $t2, ($sp)
	addiu $sp, $sp, 4
	li $t0, 2
	addiu $sp, $sp, -4
	sw $t0, ($sp)
	lw $t1, ($sp)
	addiu $sp, $sp, 4
	bgt $t2, $t1, fContinuar2
	j For2
fContinuar2:
	li $v0, 10
	syscall
.data
x:
	.word 1
t:
	.word 1
z:
	.word 1
a:
	.word 1
array1:
	.space 12
newline:
	.asciiz "\n"

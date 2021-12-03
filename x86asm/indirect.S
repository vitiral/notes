# PURPOSE: show how an indirect call works
# INPUT: none
# OUTPUT: status code. View with echo $?
# VARIABLES:
#   %eax holds the system call number
#   %ebx holds the return status
.section .data

indirectCall: .quad 0 # indirect calls have to be stored as a quadword

.section .text
.globl _start
_start:
	movl $1, %eax   # system call number for exiting a program
	movl $setEbx42, indirectCall
	call *(indirectCall)
	movl $setEbx43, indirectCall
	movl $indirectCall, %ebx
	call *(%ebx)
	int  $0x80      # kernel vectored interrupt

setEbx42:
	movl $42, %ebx
	ret

setEbx43:
	movl $43, %ebx
	ret

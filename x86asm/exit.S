# PURPOSE: simple program to exit and return status code to linux kernel.
# INPUT: none
# OUTPUT: status code. View with echo $?
# VARIABLES:
#   %eax holds the system call number
#   %ebx holds the return status
.section .data

.section .text
.globl _start
_start:
	movl $1, %eax   # system call number for exiting a program
	movl $1, %ebx   # return code
	int  $0x80      # kernel vectored interrupt

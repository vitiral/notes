# PURPOSE: find the largest number in a list of pre-programmed numbers, with 0
# terminating.
#
# INPUT: none
# OUTPUT: larest number as the rc
# VARIABLES:
# - `data_items`: address where numbers start. Last number is a zero (lol).
# - %edi is current position in list (address)
# - %ebx current highest value
# - %eax current value being examined
.section .data
data_items:
	.long 3,225,34,222,45,75,54,34,44,33,22,11,66,0


.section .text
.globl _start
_start:
	movl $0, %edi    # set index to 0
	movl data_items(,%edi,4), %eax  # load the first byte of data
	movl %eax, %ebx  # set the value to be the current largest

1: // loop start
	cmpl  $0, %eax  # check to see if we've hit the end
	je 3f
	inc %edi
	movl data_items(,%edi,4), %eax  # load the first byte of data
	cmpl %eax, %ebx
	jge 1b  # jump to beggining if second value (ebx) > first value (eax).
	movl %eax, %ebx
	jmp 1b

3: // end loop
	movl $1, %eax   # system call number for exiting a program
	// movl $1, %ebx# return code = largest. Already set correctly.
	int  $0x80      # kernel vectored interrupt

# PURPOSE: demonstrate how C-like functions work in asm.

.code32  # 32 bit mode. Things will go wonky if you don't use --32
.section .data # empty
.section .text

.globl _start
_start:
	pushl $3  # push second argument
	pushl $2  # push first argument
	call power
	addl $8, %esp  # move sp back

	pushl %eax # save first answer before calling second.
	pushl $-1 # 2nd arg
	pushl $5 # 1st arg
	call power
	addl $8, %esp # move sp back

	popl %ebx
	addl %eax, %ebx # add, the result is in ebx
	call exit

# PURPOSE: compute value of a number raised to a power.
# INPUT: first arg: input, second arg: power
# OUTPUT: result is in %eax
# VARIABLES:
#  %ebx - base number
#  %ecx - power
#  -4(%ebp) - current result
#  %eax - temp storage
	.type power, @function
power:
	cmp $0, 8(%esp)  # if power is 0 simply return 1
	je power_0
	cmp $0, 8(%esp)  # if the power is <0 return 0
	jl power_neg

	pushl %ebp  # save old base pointer
	movl %esp, %ebp # make stack pointer the base pointer
	subl $4, %esp # make room for local storage
# Note: stack from low->high memory: [tmp, ebp, ret-addr, 1st-arg, 2nd-arg]
# ebp points to "ebp" in the stack (since it was before we subtracted from
# stack), so 1st-arg is +8 from ebp
	movl 8(%ebp), %ebx  # first argument in ebx
	movl 12(%ebp), %ecx  # second argument in ecx
	movl %ebx, -4(%ebp) # store current result in our "tmp" memory.

power_loop_start:
	cmpl $1, %ecx       # if power is <= 1 we are done

	jle power_loop_end
	movl -4(%ebp), %eax # move current result into eax
	imull %ebx, %eax    # eax=eax*ebx
	movl %eax, -4(%ebp) # store current result
	decl %ecx # decrement the power
	jmp power_loop_start


power_loop_end:
	movl -4(%ebp), %eax # return value in eax
	movl %ebp, %esp  # restore stack pointer
	popl %ebp        # restore the base pointer
	ret

power_0:
	movl $1, %eax
	ret

power_neg:
	movl $0, %eax
	ret

exit:
	movl $1, %eax   # system call number for exiting a program
	# %ebx   # return code
	int  $0x80      # kernel vectored interrupt

#include <asm/unistd.h>

.text                           # code section declaration

			        # we must export the entry point to the ELF linker or
    .global _start              # loader. They conventionally recognize _start as their
			        # entry point. Use ld -e foo to override the default.

_start:
# write our string to stdout
	movl    $len,%edx           # third argument: message length
	movl    $msg,%ecx           # second argument: pointer to message to write
	movl    $1,%ebx             # first argument: file handle (stdout)
	movl    $4,%eax             # system call number (sys_write)
	int     $0x80               # call kernel interrupt

	mov     $buffer,%ebx
	movb    $65,(%ebx)          # 'A'
	movb    $66,1(%ebx)
	movb    $67,2(%ebx)
	movb    $10,3(%ebx)         # '\n'
	movl    $4,%edx             # third argument: message length
	movl    %edx,(buffer_index)
	// movl    buffer_index,%edx             # third argument: message length
	// movl    $buffer,%ecx        # second argument: pointer to message to write
	// movl    $1,%ebx             # first argument: file handle (stdout)
	// movl    $4,%eax             # system call number (sys_write)
	// int     $0x80               # call kernel interrupt

	movl    $0,%ebx             # first argument: exit code
	movl    $1,%eax             # system call number (sys_exit)
	int     $0x80               # call kernel

	.set RETURN_STACK_SIZE,8192
	.set BUFFER_SIZE,4096



1:	// read(2) to fetch input from stdin
	xor %ebx,%ebx           # 1st param: stdin=0
	mov $buffer,%ecx	# 2nd param: buffer
	mov $BUFFER_SIZE,%edx	# 3rd param: max length
	mov $__NR_read,%eax     # syscall: read
	int $0x80               # system interrupt vector, sets eax=rc eax=numRead
	test %eax,%eax		# bitwise-and, sets flags
	jbe 2f                  # below or equal zero exit
	addl %eax,%ecx

2:	
	xor %ebx,%ebx		# 1st param: return code
	mov $__NR_exit,%eax	# syscall: exit
	int $0x80

buffer_index:
	.long buffer             # current place in input buffer

.data                           # section declaration
msg:
	.ascii    "Hello, world!\n"   # our dear string
	len = . - msg                 # length of our dear string

	.bss
/* FORTH return stack. */
	.align 4096
return_stack:
	.space RETURN_STACK_SIZE
return_stack_top:		// Initial top of return stack.

/* This is used as a temporary input buffer when reading from files or the terminal. */
	.align 4096
buffer:
	.space BUFFER_SIZE
buffer_top: 			// top of buffer

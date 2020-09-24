.section .data
inputFileName:
	.ascii "lower.txt"
inputFd:
	.int 0

outputFileName:
	.ascii "upper.txt"
outputFd:
	.int 0

inputBufferSize:
	.long 128

__SYS_EXIT:
	.long 1

__F_OPEN:
	.long 5

__F_READ:
	.long 3

__F_WRITE:
	.long 4

__F_CLOSE:
	.long 6

__SYS_STDIN:
	.long 1

__SYS_STDOUT:
	.long 2

.section .bss
	.lcomm inputBuffer, 512

.section .text
.globl _start
_start:
	call openInput
# call openOutput
1:
	movl (inputFd), %ebx
	call read
	cmpl $0, %eax  # bytesread=0 indicates end of input
	jle 2f
	movl $inputBuffer, %ecx
	movl (inputBufferSize), %edx
	call arr_toupper
	pushl $inputBuffer          # param3: start of buffer for writting
	pushl (__SYS_STDOUT)        # param2: fd
	pushl %eax                  # param1: #bytes to write
	call write_flush
	subl $12, %esp
	jmp 1b # try to read again

2:
	movl $0, %ebx
	call exit

# toupper: Convert character from lower->upper case.
# input: %bl character to convert
# return: %bl return value
#
# Notes: 'A'=0x41 or 65 'Z'=0x5A or 90
#        'a'=0x61 or 97 'z'=0x7A or 122
toupper:
	# If the byte is not within ['a', 'z'] then don't change
	cmpb $'a', %bl
	jl 1f
	cmpb $'z', %bl
	jg 1f
	# else subtract 0x20 to convert to upper case.
	subb $0x20, %bl
1:
	ret

# arr_toupper: Convert entire array to upper case in place.
# input:
# %ecx=address of buffer. Used as current addr
# %edx=size of buffer. Used as end addr
# %eax=not used, preserved
#
# return: n/a
# uses: %bl for converting characters with toupper
arr_toupper:
	addl %ecx, %edx  # make edx the end of the buffer
1:
	cmpl %ecx, %edx  # if addresses are equal end loop
	je 2f
	movb (%ecx), %bl # move byte into bl
	call toupper     # convert the byte to upper case
	movb %bl, (%ecx) # write the byte back
	inc %ecx         # increment addr we are looking at
	jmp 1b
2:
	ret

openInput:
	# opens inputFileName and sets inputFd
	movl (__F_OPEN), %eax  # open syscall
	movl $inputFileName, %ebx
	movl $0, %ecx # intention=read
	movl $0666, %edx # permissions
	int $0x80
	movl %eax, (inputFd)
	ret

openOutput:
	# opens outputFileName and sets outputFd
	movl (__F_OPEN), %eax  # open syscall
	movl $outputFileName, %ebx
	movl $03101, %ecx # intention=write
	movl $0666, %edx # permissions
	int $0x80
	movl %eax, (outputFd)
	ret


# read: try to fill the buffer from the fd
# input:
# %ebx: the fd to use
# return: %eax the number of bytes read
# error: if syscall is negative exits with syscall
# also uses: %eax, %ecx, %edx
read:
	movl (__F_READ), %eax  # read syscall
	# ebx=fd
	movl $inputBuffer, %ecx      # addr of input buffer
	movl (inputBufferSize), %edx # size of input buffer
	int $0x80
	cmp $0, %eax
	jl 1f
	ret
1:
	movl %eax, %ebx
	call exit

# write_flush: write and flush all output to fd
# input:
#  param3: 12(%esp) = start of buffer
#  param2:  8(%esp) = fd to use
#  param1   4(%esp) = number of bytes to write, will be mutated.
# return: no relevant return
# error: if syscall is negative exits with syscall
# also uses: %eax, %ecx, %edx
write_flush:
1:
	movl __F_WRITE, %eax  # read syscall
	movl 8(%esp), %ebx    # fd
	movl 12(%esp), %ecx   # start of write buffer
	movl 4(%esp), %edx   # max number of bytes to write
	int $0x80

	cmp $0, %eax
	jl write_flush_err
	subl %eax, 4(%esp)   # subtract number of bytes to write from number written
	cmp $0, 4(%esp)  # if number of bytes to write is 0 or less, return
	jle 2f
	addl %eax, 12(%esp)  # advance start of write buffer by bytes written

	jmp 1b
2:
	ret
write_flush_err:
	movl %eax, %ebx
	call exit

# exit: call exit syscall
# input: %ebx is the return code
exit:
	movl __SYS_EXIT, %eax   # system call number for exiting a program
	# %ebx   # return code
	int  $0x80      # kernel vectored interrupt


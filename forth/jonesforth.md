
# clone jonesforth from https://github.com/nornagon/jonesforth

https://en.wikipedia.org/wiki/X86_instruction_listings

# [x86 reference][guide to x86 assembly]
- `(%register)` is _dereferencing_. I.e. `movb $2, (%ebx)` moves the byte "2"
  into the _address_ of register %ebx.
- 4 main 32 bit registers EAX EBX ECX EDX. Subsets of each are the `*X` (i.e. AX)
  which is the high 16bits and `*H` `*L` (i.e. AH AL) which are the remaining
  two bytes. There are also two 32 bit only register ESI and EDI as well
  as the two special pointers ESP (stack pointer) and EBP (base pointer).
- `mov` moves values between registers/pointers/constants.

```
mov <reg>, <reg|mem>
mov <mem>, <reg>
mov <con>, <reg|mem>
```

- `push <reg32|mem|cons32:a> -> %esp-=4 (%esp)=a`: note that stack "grows down"
- `pop <reg32|mem:a> -> %esp+=4 a=(%esp)`
- `lea <mem:m>, <reg32:r> r=*m` place address of m into r. `lea (%ebx,%esi,8), %edi`
- `arithmatic <reg|cons> <reg|mem>  | add <mem> <reg>`: arithmatic operations include
  add, sub, and, or, xor


# Jonesforth


Register key:
- %eax - accumulator, used for storing codeword addr in NEXT
- %esi - "instruction pointer", contains next instruction to in NEXT
- %esp - our  _parameter/data stack_. Usually the "normal stack"
- %ebp - our _return stack_. Usually called the "frame pointer"

```
/* NEXT macro. */
	.macro NEXT
	lodsl        // %eax=%esi;%esi+=4
	jmp *(%eax)  // indirect jump: jmp to address contained in address at %eax
	.endm
```

- Forth dictionary is linked list composed of WORDS containing
  `LINK POINTER | LENGTH/FLAGS | NAME-STR | CODEWORD | DEFINITION`
- The "codeword" is just the code which needs to be executed to run
  the WORD.

[guide to x86 assembly]: http://flint.cs.yale.edu/cs421/papers/x86-asm/asm.html#:~:text=The%20lea%20instruction%20places%20the,and%20placed%20into%20the%20register.

Thoughts on better return:
- Locals specified by { u:value1 u:value2 }`
- local names are compiled within a buffer reserved inside a word (`{}buffer`
  or something). This is a LL dictionary like the normal one, but contains
  just the size of the local (in cells) and it's return address index.
- Each local value 


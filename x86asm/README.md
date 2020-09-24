Guide: [Programming from the ground up](http://download.savannah.gnu.org/releases/pgubook/)

## Important Instructions

"jump" instructions follow a "cmp[b|?|l]" instruction:
- je Jump if the values were equal
- jg Jump if the second value was greater than the first value
- jge Jump if the second value was greater than or equal to the first value
- jl Jump if the second value was less than the first value
- jle Jump if the second value was less than or equal to the first value
- jump unconditional jump

`instruction pointer` a special register pointing to the place in memory to
execute next.

Data Accessing Methods:
- `immediate mode`: the data to access is embedded in the instruction itself, i.e. `movl $0, %eax`
  Note that you can also access a label's memory location (a.k.a pointer) with
  `movl $label_name, %eax` or alternatively `lea label_name, %eax`
- `register addressing mode`: instruction contains a register to access rather
  than a place in memory, i.e. `movl %ebx, %eax`

For the remaining addressing modes, the general form is:
```
ADDRESS_OR_OFFSET(%BASE_OR_OFFSET,%INDEX,MULTIPLIER)
```
- `direct addressing mode`: instructions contains the specific memory address/label
  to access. i.e. `movl label_name, %eax` (or equivalently and confusingly
  `movl (label_name), %eax` -- the parens do NOT deref). You can also specify
  a constant directly `movl 0x400, %eax` -- notice there is NOT a `$`.
- `indexed addressing mode`: instruction contains the memory address to access
  and also an `index register` to offset the address. You can also specify a
  `multiplier` `movl label_name(,%edi,4), %eax`
- `indirect addressing mode`: instruction contains a register that contains a pointer
  to the data, `movl (%ebx), %eax`
- `base pointer addressing mode`: like indirect addressing but also includes a number
  called the _offset_ to add to the register value prior to lookup, `movl 4(%ebx), %eax`

## Ch3: First program

`section .data` is the data section, which contains non-instruction data.

`section .text` is the text section, which contains instructions.

`.globl _start` is needed by the linux linker to tell linux where to start your program.
`.globl` preserves the symbol so the linker can use it.

`_start:` defines the value of the `_start` label, i.e. it assigns an address.

`movl` and other instructions are typically structured by `instr source, dest` where
the source operand is not modified but the dest is.

`$` is for immediate mode. I.e. `$2` encodes the number 2 _directly into the operation_.

### Find the largest number in a list

Variables:
- `data_items`: address where numbers start. Last number is a zero (lol).
- %edi is current position in list (index)
- %ebx current highest value
- %eax current value being examined

See `largest.S` for my code. Important points:

- `cmpl` is the compare operation, which sets the flags register. `je` will
  jump if the comparisons were equal, whearas jge will jump if the SECOND is >=
  the FIRST (it's backwards... why???)
- `movl label_name(,%edi,4), %eax` uses base pointer addressing mode where
  `data_items` is the base pointer and `%edi,4` is adding `edi*4`


### Ch4 (p41): Functions
Stack instructions:
- `%esp` always contains the _pointer_ to the current top of stack.
- `pushl <value>` pushes a value onto the stack, which grows downward.
  Same as `movl <value>, (%esp); subl $4, %esp`
- `popl <memory>` sets the memory location and increments %esp by 4. Same
  as `movl (%esp), (<memory>); addl $4, %esp`


Function instructions:
- `call <mem>` calls a "function". First it pushes the next (local) instruction address
  onto the stack (%esp). Then it modifies the instruction pointer (%eip) to point to the
  start of the function. Same as `pushl 4+%eip; jmp <addr>`
- Conventionally, the _assembly_/C-code then stores the current %esp in the %ebp
  (the base pointer) so that the C-compiler is easy to write. The reason for this
  is that during C functions the stack may grow/shrink.
- Next, the function reserves the space it needs. By using i.e. `subl $8,
  %esp`. I now realize why it needs the base pointer... it needs it to find the
  return address at the end!
- "You can use other registers in base pointer addressing mode, but the x86
  architecture makes using the %ebp register a lot faster." -- I highly doubt
  this.

Side note: I think TypeForth can use the "normal" calling convention of `call`
but still use `%ebp` for the data-stack.
- _Before_ calling a function, we `subl $localsSize, %esp` then `call (xt)`
  This will push the return-index onto the stack for us. WE then `addl
  $localsSize` again, cleaning up after ourselves function. No need for the
  base pointer!
- If we ever want to interface w/ C-like code, we can store the %ebp on the
  return stack, but I don't think this is necessary.






### Ch5 (p??): Files

### Skip the rest


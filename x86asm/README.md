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
- `immediate mode`: the data to access is embedded in the instruction itself.
- `register addressing mode`: instruction contains a register to access rather
  than a place in memory.
- `direct addressing mode`: instructions contains the specific memory address
  to access.
- `indexed addressing mode`: instruction contains the memory address to access and
  also an `index register` to offset the address. You can also specify a `multiplier`
- `indirect addressing mode`: instruction contains a register that contains a pointer
  to the data.
- `base pointer addressing mode`: like indirect addressing but also includes a number
  called the _offset_ to add to the register value prior to lookup.

`section .data` is the data section, which contains non-instruction data.

`section .text` is the text section, which contains instructions.

`.globl _start` is needed by the linux linker to tell linux where to start your program.
`.globl` preserves the symbol so the linker can use it.

`_start:` defines the value of the `_start` label, i.e. it assigns an address.

`movl` and other instructions are typically structured by `instr source, dest` where
the source operand is not modified but the dest is.

`$` is for immediate mode. I.e. `$2` encodes the number 2 _directly into the operation_.


### Task 1: find the largest number in a list

Variables:
- `data_items`: address where numbers start. Last number is a zero (lol).
- %edi is current position in list (index)
- %ebx current highest value
- %eax current value being examined



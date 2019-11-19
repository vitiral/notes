# Necessary Registers

- ESP: Stack pointer, tracks where the stack is.
  - The top is actually at a lower virtual address than the bottom as the stack
    grows downwards in memory towards the heap
- EBP: "base pointer" for the current function so that all parameters and local
  variables would be at a fixed offset from the base pointer, even as ESP moved
  with push/pop. Is now commonly used as a general-purpose register.
- EAX: 32 bit volatile register which stores the return value of a function and does certain calculations.
  - `mov eax, 3 <==> return 3`
- E[BCD]X: general purpose registers used for minor things. B is non-volatile, C&D are.
- ESI: non-volatile register typically used for a pointer, most typically a "src"
- EDI: non-volatile register typically used for a pointer, most typically a "dst"
- 16 and 8 bit registers are _contained within_ the above 32 bit ones.
- 64 bit registers are created by concatenating 32 bit ones.
  - However x64 extends x86's 8 general-purpose registers to be 64-bit, and adds 8 new 64-bit registers
    [link ](https://docs.microsoft.com/en-us/windows-hardware/drivers/debugger/x64-architecture)


# Necessary commands
- MOV
  - mov eax, [ebx]    ; (`eax = ebs`) Move the 4 bytes in memory at the address contained in EBX into EAX
  - mov eax, [esi-4]  ; (`eax = esi[-4]`) Move 4 bytes at memory address ESI + (-4) into EAX


# Registers
The [IR] guide almost immediately had me wanting to learn more about registers,
which broght me to the wikipedia [register allocation] guide. Allocating
registers can happen over:
- **local register allocation** within a **basic block**: which is a single
  block of instructions with a single entrance and exit point (and which can
  never be started/ended in middle).
- **global register allocation** (_extremely_ poorly named) which happens over
  a "whole" function/procedure. (local to the function)
- **interprocedural register allocation** which happens accross function
  boundaries via the call-graph.

For interprocedural allocation, the **calling convention** dictates how
functions receive and return their values, which may require insertion
of save/restore (of registers) around call sites.

> Not all variables are in use (or "live") at the same time, so, over the
> lifetime of a program, a given register may be used to hold different
> variables. However, two variables in use at the same time cannot be assigned
> to the same register without corrupting one of the variables. If there are
> not enough registers to hold all the variables, some variables may be moved
> to and from RAM. **This process is called "spilling" the registers.**

There are four actions that can be done when assigning registers

- Assignment: put the variable in a register.
- Move insertion: move a variable around between registers (split live range approach)
- Spilling: move a variable to memory
- Coalescing: "This action consists of limiting the number of moves between
  registers, thus limiting the total number of instructions. For instance, by
  identifying a variable live across different methods, and storing it into one
  register during its whole lifetime."




[IR]: https://cranelift.readthedocs.io/en/latest/ir.html
[register allocation]: https://en.wikipedia.org/wiki/Register_allocation

# Cranelift Presentation
https://www.youtube.com/watch?v=9OIA7DTFQWU

Crates
- cranelift-codegen: the main one, takes (IR, target, settings) and generates
  machine code.
- cranelift-frontend: for helping to create cranelift IR
- cranelift-wasm: wasm -> IR
- cranelift-faerie: create native object files (.o) (why??)
- cranelift-simplejit: run JIT'd code in memory


Uses SSA (single static assignment) form _everywhere_.
- each variable is assigned exactly once
- each variable is defined before it is used
- does _not_ use phi functions (_merge_ two variables from past control flow).
  Instead the EBBs take arguments, which is a more explicit way to specify the
  same thing (and easier to reason about IMO).
- No pointer types (!). At a compiler level pointers are just integers (?)
- ECS-like design. Instead of "graphs of pointers", arrays of "arenas" and
  indicies into them.

ECS allows for:
- a SecondaryMap to associate indexes with secondary information (no hash
  maps!)
- typesafe indexes (?)

If an ebb is deleted, it is just "leaked". In fact, they don't do much
reorgonization of code.

Why don't we use wasm as the IR:
- Some things we want to do is unsafe because we've proven they are safe: we
  want to represent things w/out bounds check, etc
- Need a much more complete IR

The frontend does a lot of things that IR might do elsewhere (i.e. LLVM) like
memcpy and ensuring SSA

It then
- Legalizes the IR (attaches valid machine instructions to the IR)
- Runs optimizations on the IR
- Does register allocation, which _could_ be done in linear time because of SSA
  but they are doing some fancy thing (?) so it takes longer.
- Encodes to the machine and do some minor optimizations.

WIP optimizations
- Inlining
- Eliminate redundancies: including loads/stores and loop iterations.
- Cheaper instructions: constant folding, algebraic simplications, branch
  simplifications.


# Cranelift IR

Cranelift IR itself is composed of function definitions, which look like:

```
;         name    args      return [call_conv]
;         v       v            v   v
function %average(i32, i32) -> f32 system_v {
    ss0 = explicit_slot 8   ; function preable

ebb1(v0: i32, v1: i32):     ; ebb, functions are composed
    v2 = f64const 0x0.0     ; of these, which contain various
    stack_store v2, ss0     ; operations
    brz v1, ebb5
    v100 = f32const +NaN
    return v100
}
```

EBBs (extended basic blocks) are a _maximal_ set of instructions that can
**only be entered from the top** and always **end in a (non-conditional)
terminator instruction**.  In addition, they can have conditional branches
_anywhere_ in the middle.

Control flow must happen at the beginning or end of EBB using one of [return,
jump, br\*, jump_table, trap].  A jump table is declared to compose of EBB
names which take no arguments.

Wait...

> Conditional branches and instructions that trap conditionally are not terminator instructions.

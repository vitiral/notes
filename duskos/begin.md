## Critical
* `.S` prints the current state [doc/lib/diag.txt].
  Alternatively, `psdump` just prints PS.
* 


## Starting out
* `help` to open help menu in **ged**
  * `q` quits
  * hjkl to move, `[ ]` to scroll
  * `^e` follows a link, i.e. `[lib/str]`
    * you cycle buffers with `^]`

* `doc/usage.txt`
  * this is a good index to the rest of the docs
  * `doc/dict.txt` contains all forth words


Disadvantages to regular forth
* no simple disassembler (must be CPU specific)


## Forth
Argument docs are the normal `a b -- c d`.

Use of `""` indicates that the "argument" is
instead read from the input stream.

```
a  address
n  number
f  flag, 0 or 1
s  string
xt execution token
e  dictionary entry
u  count
w  "wide", a 16-bit value
c  character, a 8-bit value

*I* immediate word (macro)
*A* preserves A register

Symbols:
! - Store
@ - Fetch
$ - Initialize
( - Lower boundary
) - Upper boundary
[ - Beginning of a mode
] - End of a mode
' - Address of -- not '&'???
? - As a suffix, means "Is it ...?". As a prefix, "do ... if flag"
[...] - Indicates immediateness
```

Some variables
* NEXTWORD: this is truly brilliant.



\ line comment
( inline comment )
( Note: CASE is ignored for all words )
INCLUDE basic.fs
INCLUDE assert.fs



\ ### Interpreter Hints
\ see - shows the code for a command. Rarely useful.
\ .S  - prints the stack
\ page - clears the page

1 2 3 4 \ put some items on the stack

: drop4                 \ define a "word" i.e. function
  ( u3 u2 u1 u0 -- )    \ comment of the input/output of the stack
  drop drop drop drop ; \ word body and ';' is RETURN
drop4 \ run the word


\ ### Basic printing
." This prints stuff" \ as with all words, the ' ' is important
CR \ prints carriage return
65 EMIT \ emits ASCII character at 65 ('A')
: emitA [char] A EMIT ; \ [char] macro looks up character code of next item
emitA
42 . \ print a number follows by space
CR 42 6 U.R  4254 6 U.R \ U.R leaves space around number for tables
CR


\ ### Complex formatting
\ <# ... #> ( ud -- addr len ) 
\   Converts double-length unsigned value ud to output string addr len.
\ <# ... ROT SIGN #>  ( n |d| -- addr u )
\   Converts double-length signed value (where n is the high-order cell of d and
\   |d| is the absolute value of d).

: phone#7  ( d -- )  \ 1234567. -> 123-4567
  <#   \ begin number formatting
  #    \ ( d -- d ) consume smallest digit and insert into PAD as ASCII.
       \ Uses BASE=DECIMAL, so performs  d 10 /MOD, converting remainder to
       \ ASCII and leaving quotient on stack
  # # # \ ( 7 6 5 4 ) are in PAD
  [CHAR] - HOLD \ put the character '-' into the pad
  #S   \ Convert remaining numbers into decimal. Always produces at least 1
  #>   \ end number formatting
  TYPE \ ( addr len -- ) "types" the string at addr of len to the terminal
  SPACe ;
1234567. phone#7 CR


\ ### Arithmathic
true true = assertTrue    \ ( a b -- a==b )
true false <> assertTrue  \ ( a b -- a!=b )
false 0 assertEqual
true -1 assertEqual
true INVERT false assertEqual \ true = -1 = 0xFFFFFFFF
true 0= false assertEqual     \ better way to convert between true/false
false NOT true assertEqual    \ I prefer to bind NOT to 0=
2 3 + 5 assertEqual  \ addition
2 3 - -1 assertEqual \ subtraction
2 3 * 6 assertEqual  \ multiplication
2 3 / 0 assertEqual   ( u1 u0 -- u1 // u0 ) \ division
2 3 MOD 2 assertEqual ( u1 u0 -- u1 % u0 )  \ modulo
2 3 /MOD ( u1 u0 -- [u1%u0] [u1//u0] )      \ module+division
  0 assertEqual  \ top of stack: quotient
  2 assertEqual  \ next: remainder i.e. mod
1 2 < assertTrue
1 2 <= assertTrue
1 1 <= assertTrue
2 1 > assertTrue
2 2 >= assertTrue
2 1 < assertFalse
2 1 <= assertFalse
1 2 > assertFalse

\ !! Odd behavior for pforth!  operators fail if provided no values,
\ !! but with 1 value they simply consume it silently !!
\ 1 >   2 <   3 <>   4 =   5 <=   6 >=
\ 1 +   2 *   3 /    4 -   5 MOD  
\ assertEmpty
\ 6 /MOD 0 assertEqual \ does even worse, it emits some value

\ ### Complex arithmetic
423 32 100 */  135 assertEqual \ ( a b c -- (a*b)/c using d word intermediate )
423 32 100 */MOD  135 assertEqual 36 assertEqual \ ( a b c -- remainder (a*b)/c )


\ ### Double length arithmetic
\ [0-9]*.  for specifying double. ( lsb msb )
42. 0 assertEqual 42 assertEqual
HEX 0F000F000F000. DECIMAL 263886817259520. dAssertEqual
assertEmpty

\ um*     \ ( u1 u2 — ud )
\ um/mod  \ ( ud u1 — u:quotient u:remainder -- ud / u1 )
\ u<      \ ( u1 u2 - f ) compares as unsinged integers
\ HEX OCTAL DECIMAL \ used for setting BASE to 16 8 10 respectively
\ D. prints the double signed number
\ The double length stuff is super confusing


\ ### (Data) Stack modifiers
2 1 0  \ items on the stack. Refered to by index.
       \ 0=index0 1=index1 2=index2

dup    \ 2 1 0 0 -- duplicate top item
drop   \ 2 1 0 -- drop index 0
swap   \ 2 0 1 -- swap index 0 1
drop   \ 2 0
over   \ 2 0 2 -- duplicate index 2 to top
drop 1 \ 2 0 1 -- drop the extra 2 and replace with 1
rot    \ 0 1 2 -- rotate left ( a b c -- b c a )
-rot   \ 2 0 1- - rotate right ( a b c -- c a b )
1 assertEqual 0 assertEqual 2 assertEqual
assertEmpty

1 0 TUCK \ 0 1 0 -- "tucks" index 0 under index 1
0 assertEqual 1 assertEqual 0 assertEqual
assertEmpty

0 ?dup assertFalse \ not duped because is 0
42 ?dup assertTrue assertTrue \ was duped because is not 0

5 4 3 2 1 0
2swap   \ 5 4 1 0 3 2
2dup    \ 5 4 1 0 3 2 3 2
2 assertEqual 3 assertEqual
\ 5 4 1 0 3 2
2over   \ 5 4 1 0 3 2 1 0
2dup 0 assertEqual 1 assertEqual
2drop  \ 5 4 1 0 3 2
2 assertEqual 3 assertEqual 
0 assertEqual 1 assertEqual 
4 assertEqual 5 assertEqual
assertEmpty

: useReturnBuffer ( -- )
  \ !! return buffer must be put in previous state
  \ !! before returning or program crash!!
  -1
  >R  \ move cell into return buffer
  assertEmpty
  R@ assertTrue \ copy item from return buffer
  R> assertTrue ; \ move item off return buffer
useReturnBuffer


\ ### Markers and files
use blocks.forth  \ see blocks.forth for documentation
marker -work \ set a marker in the dictionary
include hello.fs  \ includes a file
hello \ from hello.fs
hello_result  800 assertEqual
: thisWillBeDeleted ." this will be deleted by -work " CR ;
thisWillBeDeleted

\ Note: Can use  u LIST  to view contents of a block
1 load   block_1_result 42 assertEqual
." Printing block 1 to console:" CR
\ load block 0 and type it to the console
1 block      \ load block 1 into buffer, return addr
  1024 type  \ type to console
\ block 1 is loaded into a buffer, but that buffer
\  is only valid until a "multitasking" word like
\  EMIT or TYPE is used.
\ The buffer contents can be _modified_ and written
\  to disk with UPDATE, which will update disk b4
\  reusing the buffer. FLUSH will force the update.

-work  \ removes anything since   MARKER -work
\ hello -- this wouldn't work anymore
assertEmpty


\ ### Conditionals
: if12SayHi ( a -- ) 12 =
  IF \ consumes input. If 0 skips to THEN
    ." Hi, it was 12 " CR 
  THEN ;  \ alternatively THEN can be used

10 if12SayHi \ doesn't say hi
12 if12SayHi \ says hi
assertEmpty

: getDecade ( age -- )
  dup 10 < IF 0
  ELSE dup 20 < IF 1
  ELSE dup 30 < IF 2
  ELSE dup 40 < IF 3
  ELSE dup 50 < IF 4
  ELSE -1
  THEN THEN THEN THEN THEN swap drop ;

05 getDecade 0 assertEqual
15 getDecade 1 assertEqual
25 getDecade 2 assertEqual
35 getDecade 3 assertEqual
45 getDecade 4 assertEqual
55 getDecade -1 assertEqual
assertEmpty

\ ### Loops
: doLoop ( -- )
  3 0 DO  \ ( limit index -- loop until I >= limit)
    I      \ the current index put on the stack
  LOOP ;

doLoop 2 assertEqual 1 assertEqual 0 assertEqual
assertEmpty

: doLoopReverse ( -- )
  0 2 DO \ ( limit index -- loop until I < index )
    I
  -1 +LOOP ; \ control how loop moves

doLoopReverse 0 assertEqual 1 assertEqual 2 assertEqual
assertEmpty

: doLoopNested ( -- )
  2 0 DO
    4 2 DO
      I  \ inner index
      J  \ outer index
    LOOP
  LOOP ;

doLoopNested
\ ( 2 0  3 0  2 1  3 1 )
  1 assertEqual 3 assertEqual
  1 assertEqual 2 assertEqual
  0 assertEqual 3 assertEqual
  0 assertEqual 2 assertEqual
assertEmpty

: beginUntil ( -- )
  0 BEGIN \ Note: guaranteed to always run once, unlike DO
    dup 1 +  \ stack growing: ( 0 0+1 0+1+1 ... 4)
  dup 3 > UNTIL ;  \ repeats UNTIL true

beginUntil
  4 assertEqual \ Note: 4 is on stack since until is AFTER block
  3 assertEqual 2 assertEqual 
  1 assertEqual 0 assertEqual
assertEmpty

: beginAgain ( -- )
  0 BEGIN
    dup 1 +
    dup 3 > IF  
      \ Exit the function. Note that if we were using
      \ a DO loop we need to use  UNLOOP EXIT  or  LEAVE
      EXIT 
    THEN
  AGAIN ;
beginAgain
  4 assertEqual \ Note: 4 is on stack since until is AFTER block
  3 assertEqual 2 assertEqual 
  1 assertEqual 0 assertEqual
assertEmpty

: beginWhile
  0 BEGIN
    dup 3 < WHILE \ repeats WHILE true at BEGIN
      dup 1 +
  REPEAT ;
beginWhile
  \ Note: 4 isn't here anymore since while condition is before  DUP 1 +
  3 assertEqual 2 assertEqual 
  1 assertEqual 0 assertEqual
assertEmpty


\ ### Variables, constants and arrays

\ Constants
42 constant theAnswer
theAnswer 42 assertEqual

42. 2constant theAnswer2
theAnswer2 42. dassertEqual

\ declares 3 global variables
VARIABLE day   VARIABLE month   VARIABLE year 

\ When the variable is used, the variables addr is put on the stack

\ function to set the date. ! prefix is standard when setting variables
\ ! is used to SET a value ( u addr -- )

: !DATE   ( year month day -- ) day !   month !   year ! ;
2020 09 14 !DATE

\ @ ( addr - u ) is used to retrieve values at an address
year @ 2020 assertEqual
month @ 09 assertEqual
day @ 14 assertEqual

\ ? is shorhand for  @ .
: isoDate   year ?  month ?  day ? ;
." Date: " isoDate CR

\ Length 2 variables
2VARIABLE  2DATE
20200914. 2DATE 2!
2DATE 2@  20200914. dassertEqual
: 2DATE.  <# # # [char] - hold # # [char] - hold # # # # #> TYPE SPACE ;
." 2DATE: " 20200914. 2DATE. CR
assertEmpty

variable myArray  \ declare space in dictionary called MYARRAY
  2 cells  \ simply 2 * cellsize
  allot    \ allot an _additional_ amount of bytes for myArray


42 myArray !           \ store 42 into myArray index 0
43 myArray cell+ !    \ store 43 into myArray index 1
44 myArray 2 cells+ ! \ store 44 into myArray index 1

myArray @ 42 assertEqual
myArray cell+ @ 43 assertEqual
myArray 2 cells+ @ 44 assertEqual

\ or using my words
myArray 0 @u 42 assertEqual
myArray 1 @u 43 assertEqual
myArray 2 @u 44 assertEqual
assertEmpty

442 myArray 1 !u
myArray 1 @u 442 assertEqual


\ TODO: hmm doesn't work
\ : fill ( c-addr u char -- )
\   \ If u is greater than zero, store char in each of u consecutive characters
\   \ of memory beginning at c-addr.

myArray 3 1 fill \ fill first 3 BYTES with 1
HEX
myArray 0 @u 010101 assertEqual
DECIMAL

." myArray contents:" CR
myArray 3 cells dump

myArray 3 cells erase \ erase all of myArray
myArray 0 @u 0 assertEqual
myArray 1 @u 0 assertEqual
myArray 2 @u 0 assertEqual
assertEmpty

\ Store bytes at addresses
42 myArray 0 + C!
1  myArray 1 + C!
2  myArray 2 + C!

myArray 0 + C@ 42 assertEqual
myArray 1 + C@ 1 assertEqual
myArray 2 + C@ 2 assertEqual
myArray 0 @u HEX 02012A assertEqual  DECIMAL
assertEmpty

\ Initialize an array with character values
CREATE initArray  \ put name into dict at compile time. Does NOT allocate data space
  42 ,            \ , reserves one cell of data space and stores x in the cell
  1 , 2 ,
initArray 0 @u 42 assertEqual
initArray 1 @u 1 assertEqual
initArray 2 @u 2 assertEqual

CREATE myBytes
  42 C,  \ C, reserves one BYTE of data space
  1 C, 2 C,
\ note: we are now not aligned. It would be BAD to use  ,
\ without first calling CREATE (which auto-aligns)
align \ alternatively, call  ALIGN  :D

\ non initialized bytes can be filled with JUNK
myBytes 3 + ( =caddr ) cell 3 - ( =n ) erase

myBytes @ HEX 02012A assertEqual  DECIMAL

HEX
." HERE=    0x" HERE U. CR
." PAD=     0x" PAD U. CR
DECIMAL ." PAD-HERE=" PAD HERE - cell / U. ." cells" CR
HEX
\ ." Dict H=  0x" H U. CR  \ not in gforth
." TIB=     0x" tib U. CR  \ terminal input buffer
." #TIB=    0x" #tib U. CR \ ???
." >IN=0x" >IN U. CR       \ current parsed position in input-stream
DECIMAL

\ ### Vectored execution

: goodAnswer 3 ;
: bestAnswer 42 ;

HEX
." xt of goodAnswer : " 
  ' goodAnswer   \ `  gets the address of next word on INPUT STREAM
  U. CR
DECIMAL
' goodAnswer EXECUTE 3 assertEqual \ EXECUTE runs it

\ Create a function which runs the vector
variable currentAnswer
' goodAnswer currentAnswer !  \ set the addr of goodAnswer to currentAnswer

: getCurrentAnswer  ( -- u ) currentAnswer @ EXECUTE ;

getCurrentAnswer 3 assertEqual
' bestAnswer currentAnswer !
getCurrentAnswer 42 assertEqual

bye

." ### Tutorial: https://www.forth.com/starting-forth/0-starting-forth/ " CR
: GREET ." Hello I speak forth." CR ;
GREET

: cleared DEPTH 0 > IF ." Stack not cleared: " .S true ABORT" " THEN ;
." DEPTH: " DEPTH . CR
1001
." DEPTH: " DEPTH . CR
. CR
cleared

( define a WORD )
: answer 42 ;

cleared
( Print to screen )
." The answer is " answer . CR

: done   ." <<" CR ;

." 1 space   :" space done ;
." 10 spaces :" 10 spaces done ;
." 42 emit   :" 42 emit done ;
." 10 + 3    :" 10 3 + . done ;

: YARDS  ( yard -- in ) 36 * ;
: FT   ( ft -- in ) 12 * ;

cleared
." 10 yards 2 ft 5 in > in=" 10 YARDS 2 FT 5 + + . ." inches" done ;

: eq_1a ( a b c -- [a+b]/c ) -rot + swap / ;
: eq_1b { a b c -- [a+b]/c } a b + c / ;
: eq_1c { a b c | d -- [a+b]/c }  a b + to d d c / ;
: eq_1_input ( -- a b c ) 10 20 2 ;

cleared
." equation 1 in 3 forms: " eq_1_input eq_1a . ." " eq_1_input eq_1b . ." " eq_1_input eq_1c . done ;

( other stack operations: SWAP DUP OVER ROT -ROT DROP )
( 2 conterparts: 2SWAP 2DUP .. etc )
( Note: ROT rotates left, -ROT rotates right )

: .CARTON_FULL   ( eggs -- ) 12 = IF ." It's full " ELSE ." Not full " THEN ;

cleared
." Carton full w/ 10? " 10 .carton_full done ;
." Carton full w/ 2? " 2 .carton_full done ;
." Carton full w/ 12? " 12 .carton_full done ;
." Carton full w/ 13? " 13 .carton_full done ;

." true=" TRUE . ." false=" false . done ;

cleared
." -1  0= 0= -> -1 : " -1 0= 0= . done ;
." 0   0= 0= -> 0 : " 0 0= 0= . done ;
." 200 0= 0= -> -1 : " 200 0= 0= . done ;

cleared
: .SIGN
  dup 0 > IF drop ." POSITIVE "
  ELSE 0 < IF ." NEGATIVE "
  ELSE ." ZERO "
  THEN THEN ;

." sign 100: " 100 .SIGN done ;
." sign -10: " -10 .SIGN done ;
." sign 0: " 0 .SIGN done ;

cleared
: WITHIN ( n low high -- low <= n < high )
  rot dup ( low high n n )
  rot ( low n n high )
  >= IF drop drop false
  ELSE ( low n ) swap < IF false
  ELSE true
  THEN THEN ;

: .BOOL dup 0= IF drop ." false"
  ELSE -1 = IF ." true"
  ELSE ." bad-true"
  THEN THEN ;

: range 5 10 ;

cleared
." n=1 5 <= n < 10? " 1 range WITHIN .BOOL SPACE done ;
." n=5 5 <= n < 10? " 5 range WITHIN .BOOL SPACE done ;
." n=6 5 <= n < 10? " 6 range WITHIN .BOOL SPACE done ;
." n=10 5 <= n < 10? " 10 range WITHIN .BOOL SPACE done ;
." n=100 5 <= n < 10? " 100 range WITHIN .BOOL SPACE done ;

( using variables is SO much easier! )
: within_b { n low high -- low <= n < high }
  n low < IF false
  ELSE n high >= IF false
  ELSE true
  THEN THEN ;

cleared
." WITHIN_B: " CR ;
." n=1 5 <= n < 10? " 1 range within_b .BOOL SPACE done ;
." n=5 5 <= n < 10? " 5 range within_b .BOOL SPACE done ;
." n=6 5 <= n < 10? " 6 range within_b .BOOL SPACE done ;
." n=10 5 <= n < 10? " 10 range within_b .BOOL SPACE done ;
." n=100 5 <= n < 10? " 100 range within_b .BOOL SPACE done ;

cleared
CR ." ### Ch5" CR ;
." 1+-: " 0 1+ 1- . done ;
." 2+-: " 0 2+ 2- . done ;
." 2*/: " 1 2* 2/ . done ;
." ABS 1 -1: " 1 ABS . -1 ABS . done ;
." NEGATE 42: " 42 NEGATE . done ;
." MIN 4 400: " 4 400 MIN . done ;
." MAX 4 400: " 4 400 MAX . done ;

cleared
: DIFFERENCE   - ABS ;
." Difference 37 52: " 37 52 DIFFERENCE . 52 37 DIFFERENCE . done ;

: SWAP_BACK   ( a b c -- b a c ) >R SWAP R> ;
." SWAP BACK 1 2 3: " 1 2 3 SWAP_BACK .S 2drop drop done ;

cleared
: QUADRATIC ( a b c x -- ax^2 + bx + c )
  >R ROT ( b c a | x )
  R@ R@ * * ( b c [x*x*a] )
  ROT ( c [x*x*a] b )
  R> * ( c [a*a*x] [b*x] )
  + + ;

: Q_ABC 2 3 4 ;
." QUADRATIC a=2 b=3 c=4" done ;
." x=-2: " Q_ABC -2 QUADRATIC . done ;
." x=0: " Q_ABC 0 QUADRATIC . done ;
." x=1: " Q_ABC 1 QUADRATIC . done ;
." x=2: " Q_ABC 2 QUADRATIC . done ;
." x=5: " Q_ABC 5 QUADRATIC . done ;

cleared
: %   ( value perc -- result ) 100 */ ;
." 32% of 225: " 225 32 % . done ;
: 4MAX ( a b c d -- maximum )
  MAX MAX MAX ;
." max of 1 2 3 4: " 1 2 3 4 4MAX . done ;

cleared
CR ." #### Ch6: loops" CR
: 10hello ( -- )
  10 0 DO
    ." Hello " I .
  LOOP ;

10hello done

: MULTIPLICATIONS ( n -- )
  ." Multiplications " DUP . ." : "
  10 1 DO
    DUP I * .
  LOOP DROP ;
2 MULTIPLICATIONS done
3 MULTIPLICATIONS done


cleared
: COMPOUND ( amount interest -- )
  ( "Compund <amount> %<interest>" )
  2dup swap ." Compound " . . ." % : " CR

  100 + ( amount [interest+100] )
  10 1 DO
    ." Amount after " I . ." years: "
    dup rot swap ( [interest+100] amount [interest+100] )
    % dup . CR swap ( newAmount [interest+100] )
  LOOP 
  2drop ;
1000 6 COMPOUND done

cleared
: RECTANGLE ( width -- )
  256 0 DO 
    DUP I SWAP MOD ( I % width ) 0= IF
      CR
    THEN ." *"
  LOOP DROP ;
32 RECTANGLE done

cleared
: MUL_TABLE ( -- )
  10 1 DO
    10 1 DO
      I J * 5 U.R
    LOOP
    CR
  LOOP ;
MUL_TABLE done

cleared
: PENTALOOP  50 0 DO I . 5 +LOOP ;
PENTALOOP done

cleared
: FALLING 0 9 DO I . -1 +LOOP ;
FALLING done

cleared
: STAR [CHAR] * EMIT ;
: STARS ( n -- . # of stars )
  0 DO STAR LOOP ;
." 5 stars: " 5 STARS done
." 10 stars: " 10 STARS done

cleared
CR ." Ch7: more numbers " CR
: ASCII_TABLE 
  126 32 DO
    5 I + I DO
      I 126 <= IF
        I 3 U.R ." : " [CHAR] ' EMIT I EMIT [CHAR] ' EMIT ."   "
      THEN
    LOOP
    CR
  5 +LOOP ;
." ASCII TABLE" CR ASCII_TABLE done

." I skipped most of this as I already know it." CR

cleared
: ?   @ . ; ( wow, this wasn't actually defined )
CR ." Ch8: Variables, constants, arrays" CR

VARIABLE DATE
cleared
12 DATE ! ( store the date as 12 )
." Today's date: " DATE @ . done
." Or just... " DATE ? done
13 DATE !
." Now the date is " DATE ? done
FORGET DATE

cleared
variable day   variable month    variable year
: !DATE   ( year month day -- ) DAY ! MONTH ! YEAR ! ;
: ?DATE   ( -- ) year ? month ? day ? ;
2020 09 12 !DATE
." Today's date: " ?DATE done

." oven limits " CR
cleared
220 constant ovenLimit
: ?tooHot  ovenLimit > IF ." Danger -- reduce heat " THEN ;

( 355 113 2CONSTANT PI ) ( 355 / 133 ~= 3.14159... )

." Burner limits " CR
cleared
VARIABLE burnerLimits[5] 4 CELLS ALLOT

( Get the address for a burner limit. Note that CELLS is basically multiplying )
( the pointer by the byte-width of a cell )
: &burnerLimit   ( index -- &burnerLimit ) CELLS burnerLimits[5] + ;

." &burnerLimits[5] " burnerLimits[5] . done
220 burnerLimits[5] ! 
." burnerLimit 0: " burnerLimits[5] ? done

220 0 &burnerLimit ! ( store burner 0 limit )
340 1 &burnerLimit ! ( store burner 1 limit )
325 2 &burnerLimit !
200 3 &burnerLimit !
150 4 &burnerLimit !

." burnerLimit 1: " 1 &burnerLimit @ . done
." burnerLimit 2: " 2 &burnerLimit @ . done

cleared
: dumpArray   ( addr cell-count : )
  CELLS OVER + SWAP ( addr [addr+cells] )
  DO
    I ?
  CELL +LOOP ;

." dumpArray " burnerLimits[5] 5 dumpArray done
." DUMP " burnerLimits[5] 5 cells dump done

cleared
CR ." ### Ch9: Under the hood" CR
." Greet execution token: " ' GREET U. done
." Execute GREET: " ' GREET EXECUTE done
( ." Doesn't work: " ' bazelkjsd EXECUTE done )
( ." What is an unfound value? : " ' NOT_FOUND . done -- throws error )
( ." Greet contents: " ' GREET 1 DUMP done -- segfaults ?? )

cleared
: hello ." Hello " ;
: goodbye ." Goodbye " ;
VARIABLE 'aloha   ' hello 'aloha ! ( store hello xt in 'aloha )
: aloha   'aloha @ EXECUTE ;

." hello goodbye: " hello goodbye done
." aloha: " aloha done
' goodbye 'aloha !
." (assigned goodbye) aloha: " aloha done

( tick always goes to the next word IN THE INPUT STREAM )
( In other words, it IGNORES the `'aloha !` and first )
( consumes a character from the input stream )
: alohaSays  ' 'aloha ! ;
alohaSays hello
." (alohaSays hello) aloha: " aloha done
alohaSays goodbye
." (alohaSays goodbye) aloha: " aloha done

: alohaComming   ['] hello 'aloha ! ;
: alohaGoing  ['] goodbye 'aloha ! ;
alohaComming
." (comming) aloha: " aloha done
alohaGoing
." (going) aloha: " aloha done

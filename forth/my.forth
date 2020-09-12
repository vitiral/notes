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
." ### Ch5" CR ;
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
: % 100 */ ;
." 32% of 225: " 225 32 % . done ;
: 4MAX ( a b c d -- maximum )
  MAX MAX MAX ;
." max of 1 2 3 4: " 1 2 3 4 4MAX . done ;

cleared
." #### Ch6" CR

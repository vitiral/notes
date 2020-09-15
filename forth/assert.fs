\ ### Assertions
\ ABORT" some text here" will abort if the value is TRUE
: assertTrue ( u1 u0 -- ) 0= ABORT" was false" ;
: assertFalse ( u1 u0 -- ) ABORT" was true" ;
: assertEqual ( u1 u0 -- ) 2dup <> IF
    ." Not equal: " U. U. true THROW
  THEN 2drop ;
: dAssertEqual ( d1 d0 -- ) 4dup D<> IF
    ." Not equal: " UD. UD. true THROW
  THEN 4drop ;
: assertEmpty ( -- ) DEPTH 0 > IF ." Stack not cleared!" .S true THROW THEN ;

3 2 1 0 sp@3
3 assertEqual
0 assertEqual 1 assertEqual 2 assertEqual 3 assertEqual

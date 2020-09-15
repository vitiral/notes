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
: assertC= ( addr1 addr2 count -- )
  3dup C= not IF
    rot over ( addr2 count addr1 count )
    CR ." if " .S CR
    ." Arrays not equal." CR ." addr1:" dump
    ." addr2:" dump
    true THROW
  THEN 3drop ;

3 2 1 0 sp@3
3 assertEqual
0 assertEqual 1 assertEqual 2 assertEqual 3 assertEqual

\ Helper methods that should be in the std

: sp@0 ( 0 -- 0 0 ) dup ;    \ fetch index 0 from stack
: sp@1 ( 1 0 -- 1 0 1 ) over ;   \ fetch index 1 from stack
: sp@2 ( 2 1 0 -- 2 1 0 2 ) SP@ 2 cells + @ ;
: sp@3 ( 3 2 1 0 -- 3 2 1 0 3 ) SP@ 3 cells + @ ;
: 3dup sp@2 sp@2 sp@2 ;
: 4dup sp@3 sp@3 sp@3 sp@3 ;
: 3drop 2drop drop ;
: 4drop 2drop 2drop ;
: NOT 0= ;

: cells+   ( addr i:cells -- addr + cell*i ) cells + ;
: @u ( addr u:cells -- u )
  \ return the cell at index of address
  cells + @ ;
: !u ( u addr u:cells -- ) 
  \ store value at index of address
  cells + ! ; 
: @d ( addr u:cells -- d ) 
  \ retrieve the double starting at index TODO: which significant bit is it?
  cells + 2@ ; 
: 2@d ( addr u:2*cells -- d )
  \ retrieve the double indexing by double cells
  cells + 2 * 2@ ; 
: -2rot ( d2 d1 d0 -- d0 d2 d1 )
  2swap 2>R  ( d2 d0  R: d1 )
  2swap 2R> ;

: BINARY  2 BASE ! ;

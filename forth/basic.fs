\ Helper methods that should be in the std

\ 0@dup is same as dup
: 1@dup ( 1 0 -- 1 0 1 ) over ;
: 2@dup ( 2 1 0 -- 2 1 0 2 ) SP@ 2 cells + @ ;
: 3@dup ( 3 2 1 0 -- 3 2 1 0 3 ) SP@ 3 cells + @ ;
: 3dup 2@dup 2@dup 2@dup ;
: 4dup 3@dup 3@dup 3@dup 3@dup ;
: 4drop 2drop 2drop ; 
: NOT 0= ;


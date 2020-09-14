
\ Variable declarations -----------------------------------------

0 VALUE Look \ lookahead character 

\ Tools ---------------------------------------------------------

\ read new character from input stream 
: getchar ( -- ) KEY ( was EKEY? ) TO Look ;                      

\ report an error
: error ( c-addr u -- ) CR 7 ( bell ) EMIT ." Error: " TYPE ." ." ; 

\ report an error and stop
: aborts ( c-addr u -- ) error ABORT ;

\ report what was expected 
: expected ( c-addr u -- ) S"  Expected" ." $+" aborts ;

: tolower ( C -- c ) dup [char] A [char] Z 1+ within if 32 + then ;
: toupper ( c -- C ) dup [char] a [char] z 1+ within if 32 - then ;

\ recognize an alpha character 
: alpha? ( char -- tf )  toupper [CHAR] A [CHAR] Z 1+ WITHIN ;

\ recognize a decimal digit
: digit? ( char -- tf )  [CHAR] 0 [CHAR] 9 1+ WITHIN ;                 

\ output a string with tab 
: emits ( c-addr u -- ) Tab EMIT TYPE ;

\ output a string with tab and crlf 
: emitln ( c-addr u -- ) CR emits ;

\ initialize
: init ( -- ) getchar ;

\ Specifics -----------------------------------------------------

\ match a specific input character ------------------------------
: match ( char -- )
        DUP Look = IF  DROP getchar
                 ELSE  S" `" ROT CHAR-APPEND &' CHAR-APPEND expected 
                ENDIF ;

\ get an identifier ---------------------------------------------
: getname ( -- char )
        Look alpha? 0= IF  S" Name" expected  ENDIF
        Look >UPC 
        getchar ;

\ get a number --------------------------------------------------
: getnum ( -- char )
        Look digit? 0= IF  S" Integer" expected  ENDIF
        Look '0' - 
        getchar ;

\ Main Program --------------------------------------------------
: cradle ( -- ) init ;


CR .( LOADING TEST SUITE: DMATH) CR

VARIABLE EXE
VARIABLE OK
VARIABLE FAIL
VARIABLE STEP

: ISUITE
0 EXE ! 0 OK ! 0 FAIL ! ;
: CHECK0
  DEPTH 0= IF 1 STEP +! THEN ;
: CHECK1
  DEPTH 1 = IF 1 STEP +! THEN ;
: CHECK2
  DEPTH 2 = IF 1 STEP +! THEN ;
: CHECK3
  DEPTH 3 = IF 1 STEP +! THEN ;
: CHECK4
  DEPTH 4 = IF 1 STEP +! THEN ;
: ASSERT
  = IF 1 STEP +! THEN ;
: DASSERT
  D= IF 1 STEP +! THEN ;
: PROK
  ." ok" CR ;
: PRNOK
  ." fail" CR ;
: RESULT
  = IF 1 OK +! PROK ELSE 1 FAIL +! PRNOK THEN ;
: ICASE
  1 EXE +! 0 STEP ! ;
: STAT
  CR ." eXECUTED " OK @ . ." / " EXE @ .
  ." CASES SUCCESSFULLY" CR ;

: TCONVDEC
  ." TESTING CONVERSION (DECIMAL) - "
  ICASE
  -2147483648.
  SWAP OVER DABS <# #S ROT SIGN #>
  CHECK2
  11 ASSERT 262 ASSERT
  262 C@ 45 ASSERT ( - )
  263 C@ 50 ASSERT ( 2 )
  272 C@ 56 ASSERT ( 8 )
  STEP @ 6 RESULT
;

: TCONVHEX
  ." TESTING CONVERSION (HEX) - "
  ICASE
  HEX
  [ HEX ] 1234ABCD. [ DECIMAL ]
  SWAP OVER DABS <# #S ROT SIGN #>
  DECIMAL
  CHECK2
  8 ASSERT 265 ASSERT
  265 C@ 49 ASSERT ( - )
  272 C@ 68 ASSERT ( D )
  STEP @ 5 RESULT
;

: DMATH
  CR ISUITE
  TCONVDEC
  TCONVHEX
  STAT
;


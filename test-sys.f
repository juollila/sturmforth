CR .( LOADING TEST SUITE: SYS) CR

HEX

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

: T-SYS
  ." TESTING SYS - "
  ICASE
  60 C000 C! ( RTS )
  C000 SYS
  CHECK0
  STEP @ 1 RESULT
;

: T-REGS
  ." TESTING AC, XR, YR - "
  ICASE
  10 AC C!
  20 XR C!
  40 YR C!
  E8 C000 C! ( INX )
  C8 C001 C! ( INY )
  18 C002 C! ( CLC )
  69 C003 C! ( ADC # )
  01 C004 C! ( #01 )
  60 C005 C! ( RTS )
  C000 SYS
  AC C@ 11 ASSERT
  XR C@ 21 ASSERT
  YR C@ 41 ASSERT
  CHECK0
  STEP @ 4 RESULT
;

: T-SR
  ." TESTING SR - "
  ICASE
  30 SR C!
  38 C000 C! ( SEC )
  60 C001 C! ( RTS )
  C000 SYS
  SR C@ 31 ASSERT
  CHECK0
  STEP @ 2 RESULT
;

: SYS
  CR ISUITE
  T-SYS
  T-REGS
  T-SR
  STAT
;

DECIMAL

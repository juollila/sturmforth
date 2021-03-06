CR .( LOADING TEST SUITE: BRANCH) CR

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

: T-IF
  ." TESTING IF THEN ELSE - "
  ICASE
  0 1 CHECK2
  IF 2 ELSE 3 THEN
  2 ASSERT
  IF 2 ELSE 3 THEN
  3 ASSERT CHECK0
  STEP @ 4 RESULT
;

: T-LOOP
  ." TESTING LOOP - "
  ICASE
  10 0 CHECK2 DO 1 STEP +! LOOP
  CHECK0
  STEP @ 12 RESULT
;

: T-+LOOP
  ." TESTING +LOOP - "
  ICASE
  10 0 CHECK2 DO 1 STEP +! 2 +LOOP
  CHECK0
  STEP @ 7 RESULT
;

: T-I
  ." TESTING I - "
  ICASE
  10 0 CHECK2 DO I STEP +! LOOP
  CHECK0
  STEP @ 47 RESULT
;

: T-J
  ." TESTING J - "
  ICASE
  10 0 DO 10 0 DO I J * STEP ! LOOP LOOP
  CHECK0
  STEP @ 82 RESULT
;

: T-UNTIL
  ." TESTING BEGIN UNTIL - "
  ICASE
  BEGIN 1 STEP +! STEP @ 9 = UNTIL
  CHECK0
  STEP @ 10 RESULT
;

: T-WHILE
  ." TESTING BEGIN WHILE REPEAT - "
  ICASE
  BEGIN CHECK0 STEP @ 9 < WHILE CHECK0 REPEAT
  STEP @ 9 RESULT
;

: BRANCH
  CR ISUITE
  T-IF
  T-LOOP
  T-+LOOP
  T-I
  T-J
  T-UNTIL
  T-WHILE
  STAT
;



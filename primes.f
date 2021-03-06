( TIME: 1:45 )
CR .( TYPE: PRIMES ) CR
VARIABLE TESTNUM
VARIABLE PRIMEPTR
VARIABLE LASTTEST
VARIABLE LASTPRIME
1000 CONSTANT TOTALPRIMES
: ARRAY <BUILDS 2* 2+ ALLOT DOES> SWAP 2* + ;
TOTALPRIMES ARRAY PRIMENUMS
: BUMPPTR 1 PRIMEPTR +! ;
: BUMPNUM 2 TESTNUM +! 2 PRIMEPTR ! ;
: NOTPRIME
  TESTNUM @ PRIMEPTR @ PRIMENUMS @ /MOD DROP 0 =
;
: FOUNDPRIME PRIMEPTR @ LASTTEST @ > ;
: GETPRIME
  BEGIN NOTPRIME IF BUMPNUM ELSE
  BUMPPTR THEN FOUNDPRIME UNTIL
;
: STOREPRIME
  1 LASTPRIME +! TESTNUM @ LASTPRIME @
  PRIMENUMS !
;
: ALLDONE LASTPRIME @ TOTALPRIMES = ;
: BUMPLAST 1 LASTTEST +! ;
: PASTLAST
  TESTNUM @ LASTTEST @ PRIMENUMS @
  DUP * >
;
: BUMPTEST PASTLAST IF BUMPLAST THEN ;
: SHOWPRIME TESTNUM @ . SPACE SPACE ;
: BIGLOOP
  BEGIN BUMPTEST BUMPNUM GETPRIME
  STOREPRIME SHOWPRIME ALLDONE UNTIL
;
: SETUP
  3 TESTNUM ! 2 LASTTEST ! 2 LASTPRIME ! ;
2 1 PRIMENUMS !
3 2 PRIMENUMS !
: PRIMES SETUP BIGLOOP ;

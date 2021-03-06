Introduction
============

SturmFORTH is a subroutine threaded code FORTH interpreter for Commodore 64.
FORTH is a simple high-level programming language. However, it is very
extensible and powerful. SturmFORTH is highly, but not fully compatible with
FIG-FORTH implementations and FORTH-79 and FORTH-83 standards.

This manual is a reference manual. "Starting FORTH" by Leo Brodie is
recommended if a reader is not familiar with the FORTH programming language.

Memory Map
==========

```
The following memory areas are used by SturmFORTH:
$0003-$0072 Data stack
$008B-$008F Variables
$0100-$0110 Number string buffer
$0200-$0240 Text buffer
$02E6-$02FF Variables
$030C-$030F Used by SYS word
$0801-$2800 SturmFORTH code, see note 1
$2800-$9FFF Free space for dictionary entries, see note 2
$C000-$CFFF Used by editor.f

Note 1: SturmFORTH code length is less than 8kB.
Note 2: Free space for dictionary entries starts immediately after SturmFORTH
thus the actual starting address is below $2800. The actual starting address
can be checked before any new word has been defined: HEX HERE @ .
```

Reference
=========

```
addr = 16 bit address
byte = 8 bit byte
c    = 8 bit character
d    = 32 bit signed double integer
flag = 0 is equal to false, non-zero is equal to true
id   = file id
ior  = i/o operation result
n    = 16 bit signed integer
u    = 16 bit unsigned integer
ud   = 32 bit unsigned double integer

! ( n addr -- )
Store n at addr.

# ( ud1 -- ud2 )
Generate from an unsigned double number d1, the next ASCII character which is
placed in an output string. Result d2 is the quotient after division by BASE
is maintained for further processing. Used between <# and #> .

#> ( d -- addr n )
End pictured numeric output conversion. Drop d, leaving the text address, and
character count, suitable for TYPE.

#S ( ud -- 0 0 )
Convert all digits of an unsigned 32-bit number ud, adding each to the
pictured numeric output text, until remainder is zero. A single zero is added
to the output string if the number was initially zero. Use only between <#
and #>.

' ( -- addr )
Used in the form: ' <name>
Get the execution address of word <name>. Note: SturmFORTH returns execution
address instead of compilation address.

( ( -- )
Used in the form: ( ccc)
Accept and ignore comment characters from the input  stream, until the next 
right parenthesis. As a word, the left parenthesis must be followed by one
blank.

* ( n1 n2 -- n3 )
Leave the arithmetic product of n1 times n2.

*/ ( n1 n2 n3 -- n4 )
Multiply n1 by n2, divide the result by n3 and leave the quotient n4. n4 is
rounded toward zero. The product of n1 times n2 is maintained as an
intermediate 32-bit value for greater precision than the otherwise equivalent
sequence: n1 n2 * n3 /

*/MOD ( n1 n2 n3 -- n4 n5 )
Multiply n1 by n2, divide the result by n3 and  leave the remainder n4 and
quotient n5. A 32-bit intermediate product is used as for */ . The remainder
has the same sign as n1.

+ ( n1 n2 -- n3 
Leave the arithmetic sum of n1 plus n2.

+! ( n addr -- )
Add n to the 16-bit value at the address, by the convention given for + .

+LOOP ( n -- )
Add the signed increment n to the loop index using the convention for +, and
compare the total to the limit.

, ( n -- )
Allot two bytes in the dictionary, storing n there.

- ( n1 n2 -- n3 )
Subtract n2 from n1 and leave the difference n3.

. ( n -- )
Display n converted according to BASE in a free field format with one trailing
blank. Display only a negative sign.

."
Used in a colon definition in the form: ." ccc"
Accept the following text from the input stream, terminated by " (quote).

.(
Same as ." but used in the interpreting mode.

.S
Display the content of data stack without altering it. Used for debugging
purposes.

/ ( n1 n2 -- n3 )
Divide n1 by n2 and leave the quotient n3. n3 is rounded toward zero.

/MOD ( n1 n2 -- n3 n4 )
Divide n1 by n2 and leave the remainder n3 and quotient n4. n3 has the same
sign as n1.

/UMMOD32 ( ud1 n1 --- n2 ud2 )
32-bit unsigned DIVMOD.

0< ( n -- flag )
True if n is less than zero (negative)

0= ( n -- flag )
True if n is zero.

0> ( n -- flag )
True if n is greater than zero.

0BRANCH ( n -- )
Branch if n is equal to zero. This word is used internally by SturmFORTH.

1+ ( n -- n+1 )
Increment n by one, according to the operation of + .

1- ( n -- n-1 )
Decrement n by one, according to the operation of - .

2! ( d addr -- )
Double integer is stored at addr.

2* ( n -- n*2 )
Multiply n by two.

2+ ( n -- n+2 )
Increment n by two, according to the operation of + .

2- ( n -- n-1 )
Decrement n by two, according to the operation of - .

2/ ( n -- n/2 )
Divide n by two.

2@ ( addr -- d )
Fetch d from addr.

2CONSTANT ( d -- )
A defining word executed in the form: d 2CONSTANT <name>
Creates a dictionary entry for <name> so that when <name> is later executed,
d will be left on the stack.

2DROP ( d -- )
d is removed from the stack.

2DUP ( d -- d d )
Duplicate d.

2ROT ( d1 d2 d3 -- d2 d3 d1 )
The top three double integers on the stack are rotated, bringing the third
double integer to the top of the stack.

2SWAP ( d1 d2 -- d2 d1 )
The top two double integers are exchanged.

2VARIABLE
A defining word executed in the form: 2VARIABLE <name>
A dictionary entry for <name> is created and four bytes are ALLOTted in the
dictionary. The space is used for contents of the variable. The application is
responsible for initializing the contents of the variable which it creates.
When <name> is later executed, the address variable is placed on the stack.

:
Used in the form called a colon-definition:
: <name> ... ;
Creates a dictionary entry defining <name> as equivalent to the following
sequence of word definitions '...' until the next ';' 

;
Terminate a colon definition and stop compilation.

< ( n1 n2 -- flag )
True if n1 is less than n2.

<#
Initialize pictured numeric output. The words: # #> #S <# HOLD SIGN can be
used to specify the conversion of a double-precision number into an ASCII
character string stored in right-to-left order.

<BUILDS
Used within a colon-definition:
: <name> <BUILDS ... DOES> ... ;
Each time <name> is executed <BUILDS defines a new word with a high level
execution procedure. Executing <name> in the form:
<name> <word>
uses <BUILDS to create a dictionary entry for <word> and executes words
between <BUILDS and DOES>. When <word> is later executed, it has the address
of its parameter area on the stack and executes the words after DOES> in
<name>. <BUILDS and DOES> allow run-time procedures to be written in high
level rather than in assembler code.

<CMOVE ( addr1 addr2 n -- )
Copy n bytes beginning at addr1 to addr2. The move proceeds within the bytes
from high memory towards low memory.

= ( n1 n2 -- flag )
True if n1 is equal to n2.

> ( n1 n2 -- flag )
True if n1 is greater than n2.

>R ( n -- )
Transfer n to the return stack. Every >R must be balanced by a R> in the same
control structure nesting level of a colon- definition.

? ( addr -- )
Display the number at address, using the format of "." .

?DUP ( n -- n ( n ) )
Duplicate n if it is non-zero.

@ ( addr -- n )
Leave on the stack the number contained at addr.

[
Used in a colon-definition in the form: : <name> [ ... ] ;
Leaves the compile mode. The words after [ are executed, not compiled. This
allows calculation or compilation exceptions before resuming compilation
with ]. 

[COMPILE]
Used in a colon-definition in the form: [COMPILE] <name>
Enforces compilation of the following word. This  allows compilation of an
IMMEDIATE word when it would otherwise be executed.

]
Resume the compilation mode.


ABORT
Clear the data and return stacks, setting execution mode.  Return control to
the terminal.

ABS ( n1 -- n1 )
Leave the absolute value of a number.

AC ( -- addr )
Leave the address of accumulator variable. Used with SYS word.

ALLOT ( n -- )
Allocate n bytes from the dictionary.

AND ( n1 n2 -- n3 )
Leave the bitwise logical 'and' of n1 and n2.

BASE ( -- addr )
Leave the address of a variable containing the current input-output numeric
conversion base. Use HEX or DECIMAL to change BASE.

BEGIN
Used in a colon-definition in the form:
BEGIN ... flag UNTIL or
BEGIN ... flag WHILE ... REPEAT
BEGIN marks the start of a word sequence for repetitive execution.
A BEGIN-UNTIL loop will be repeated until flag is true. A BEGIN-WHILE-REPEAT
loop will be repeated until flag is false. The words after UNTIL or REPEAT
will be  executed when either loop is finished. Flag is always dropped after
being tested.

BRANCH
Branch always. This word is used internally by SturmFORTH.

C! ( n addr -- )
Store the least significant 8-bits of n at addr.

C" ( -- addr)
Used in colon definitions. Compile a counted string. At runtime, the address
of counted string will pushed on the stack.

C, ( n -- )
ALLOT one byte then store the least significant 8 bits of n to the dictionary.

C@ ( addr -- n )
Leave on the stack the contents of the byte at addr (with higher bits zero).

CLOSE-FILE ( id -- ior )
Closes the open file which file id is equal to id. Leaves I/O operation's
result to the stack.
 
CMOVE ( addr1 addr2 n -- )
Move n bytes beginning at address addr1 to addr2. The contents of addr1 is
moved first proceeding toward high memory. If n is zero nothing is moved.

COLD
Cold start of SturmFORTH.

CONSTANT ( n -- )
A defining word used in the form: n CONSTANT <name>
Create a dictionary entry for <name>. When <name> is later executed, n will be
left on the stack.

COUNT ( addr -- addr+1 n )
Leave the address addr+1 and the character count of text beginning at addr.
The first byte at addr must contain the character count n.

CR
Print a carriage return.

CREATE
A defining word used in the form: CREATE <name>
Create a dictionary entry for <name>, without allocating any additional
memory. When <name> is subsequently executed, the address of the first byte of
<name>'s parameter field is left on the stack.


D+ ( d1 d2 -- d3 )
Leave the arithmetic sum of d1 and d2.

D- ( d1 d2 -- d3 )
Subtract d2 from d1 and leave the difference d3.

D. ( d -- )
Display d converted according to BASE in a free field format, with one
trailing blank. Display the sign only if negative.

D0= ( d -- flag )
Leave true if d is zero.

D2* ( d -- d*2 )
Multiply double integer by two.

D2/ ( d -- d/2 )
Divide double integer by two.

D< ( d1 d2 -- flag )
True if d1 is less than d2.

D= ( d1 d2 -- flag )
True if d1 equals d2.

DABS ( d1 -- d2 )
Leave the absolute value of a double number d1.

DECIMAL
Set the input-output numeric conversion base to ten.

DEPTH ( -- n )
Leave the number of the quantity of 16-bit values contained in the data stack,
before n added.

DMAX ( d1 d2 -- d3 )
Leave the larger of two double numbers.

DMIN ( d1 d2 -- d3 )
Leave the smaller of two double numbers.

DNEGATE ( d -- -d )
Leave the two's complement of a double number.

DO ( n1 n2 -- )
Used in a colon-definition:
DO ... LOOP or
DO ... +LOOP
Begin a loop which will terminate based on control parameters. The loop index
begins at n2, and terminates based on the limit n1. At LOOP or +LOOP, the
index is modified by a positive or negative value. The range of a DO-LOOP is
determined by the terminating word. DO-LOOP may be nested.

DOES>
A word which defines the run-time action within a high level defining word.
Used in combination with <BUILDS.

DROP ( n -- )
Drop the top number from the stack.

DUMP ( addr u -- )
List the contents of u addresses starting at addr. Each line of values may be
preceded by the address of the first value.

DUP ( n -- n n )
Leave a copy of the top stack number.

ELSE
Used in a colon-definition in the form: IF ... ELSE ... THEN
ELSE executes after the true part following IF. ELSE forces execution to skip
till just after THEN. It has no effect on the stack. see IF.

EMIT ( c -- )
Prints character c.

EXECUTE ( addr -- )

Execute the dictionary entry whose execution address is on the stack.

FILL ( addr n byte -- )
Fill  memory beginning at address with a sequence of n copies of byte.
If the quantity n is less than or equal to zero, take no action.

FIND ( -- addr )
Leave the execution address of the next word name, which is accepted from the
input stream. If that word cannot be found leave zero. Note: SturmFORTH
returns execution address instead of compilation address.

FORGET
Execute in the form: FORGET <name>
Delete from the dictionary <name> and all words added to the dictionary after
<name>.

HERE ( -- addr )
Return the address of the variable, which contains the next available
dictionary location. Note: HERE @ in SturmFORTH is equal to HERE in some other
implementations.

HEX
Set the input-output numeric conversion base to sixteen.

HIDE
Hide the most recently made dictionary entry.

HOLD ( c -- )
Insert char into a pictured numeric output string. May only be used between <#
and #> .

I ( -- n )
Copy the loop index onto the data stack. May only be used in the form:
DO ... I ... LOOP or
DO ... I ... +LOOP

IF ( flag -- )
Used in a colon-definition in the form:
flag  IF ... ELSE ... THEN or
flag  IF ... THEN
If flag is true, the words following IF are executed and the words following
ELSE are skipped. The ELSE part is optional. If flag is false, words between
IF and ELSE, or between IF and THEN (when no ELSE is used), are skipped.
IF-ELSE-THEN conditionals may be nested.

IMMEDIATE
Marks the most recently made dictionary entry as a word which will be executed
when encountered during compilation rather than compiled.

INCLUDE
Reads and compiles FORTH file from device. File name is read from the input
stream. In SturmFORTH INCLUDE words cannot be nested.

INTERPRET
Used internally by SturmFORTH.

J ( -- n )
Return the index of the next outer loop. May only be used within a nested
DO-LOOP in the form: DO ... DO ... J ... LOOP ... LOOP

KEY ( -- c )
Leave the PETSCII value of the next available character from the input stream.

LAST ( -- addr )
Return the address of the variable, which contains pointer to the most
recentry made dictionary entry.

LEAVE
Force termination of a DO-LOOP at the next LOOP or +LOOP by setting the loop
limit equal to the current value of the index.

LIT ( -- n )
Used internally by SturmFORTH. Run-time behavior of LITERAL.

LITERAL ( n -- )
Compile the stack value n as a 16-bit literal, which when later executed, will
leave n on the stack.

LOOP
Increment the DO-LOOP index by one, terminating the loop if the new index is
equal to or greater than the limit.

M* ( n1 n2 -- d )
Multiply n1 by n2, leaving the double integer d.

M/ ( d n1 -- n2 )
Divide double integer d by integer n1 leaving the 16-bit quotient n2.

M/MOD ( d n1 -- n2 n3 )
Divide double integer d by integer n1 leaving 16-bit remainder n2 and 16-bit
quotient n3.

MAX ( n1 n2 -- n3 )
Leave the greater of two numbers.

MIN ( n1 n2 -- n3 )
Leave the lesser of two numbers.

MOD ( n1 n2 -- n3 )
Divide n1 by n2, leaving the remainder n3, with the same sign as n1.

NEGATE ( n -- -n )
Leave the two's complement of a number.

NOT ( flag1 -- flag2 )
Reverse the boolean value of flag1.  This is identical to 0=.

NUMBER ( addr -- n/d )
If interpreting then convert a string at addr to integer (n) or double
integer (d) else compile integer (n) or double integer (d).

OPEN-FILE ( addr n id -- ior )
Open a file. File name is defined at addr, n is equal to file's length, id
is file id. Returns I/O operation's result.

OR ( n1 n2 -- n3 )
Leave the bitwise inclusive-or of two numbers.

OVER ( n1 n2 -- n1 n2 n1 )
Leave a copy of the second number on the stack.

PAD ( -- addr )
The address of a scratch area for intermediate processing. In SturmFORTH PAD
is same equal to: HERE @ 1024 +

PICK ( n1 -- n2 )
Return the contents of the n1-th stack value, not counting itself. 0 PICK is
equal to DUP. 1 PICK is equal to OVER.

QUERY
Used internally by STURMFORTH.

QUIT
Clear the return stack, setting execution mode, and return control to the
terminal. No message is given.

R> ( -- n )
Transfer n from the return stack to the data stack.

R@ ( -- n )
Copy the number on top of the return stack to the data stack.

READ-FILE (addr n1 id -- n2 ior )
Read a file. Destination address is addr, n1 is a number of bytes to be
read, id is file id, n2 is equal to a number of bytes successfully read and
ior is equal to I/O operation's result.

READ-LINE (addr n1 id -- n2 ior )
Read a file until n1 bytes has been read or end of line is reached.
Destination address is addr, n1 is a number of bytes to be read, id is file
id, n2 is equal to a number of bytes successfully read and ior is equal to
I/O operation's result. A line-terminating character is read into memory at
the end of the line, but is not included in the count n2.

REPEAT
Used in a colon-definition in the form: BEGIN ... WHILE ... REPEAT
At  run-time, REPEAT returns to just after the corresponding BEGIN.

ROT ( n1 n2 n3 -- n2 n3 n1 )
Rotate the top three values, bringing the deepest to the top.

S" ( -- addr n )
Compile a string. Leave the address of string and its length on the stack.

SIGN ( n -- )
Insert the PETSCII "-" (minus sign) into the pictured numeric output string
if n is negative.

SPACE
Print a PETSCII " " character.

SPACES ( n -- )
Print n spaces.

SR ( -- addr )
Leave the address of statur register variable. Used with SYS word.

STATE ( -- addr )
Leave the address of the variable containing the compilation state.

SWAP ( n1 n2 -- n2 n1 )
Exchange the top two stack values.

SYS ( addr -- )
Call machine code routine at addr. Register values are copied from AC, XR,
YR and SR variables before a routine is called. During the return from
subroutine, same variables are updated with return values. 

THEN
Used in a colon-definition in the form:
IF ... ELSE ... THEN or
IF ... THEN
THEN is the point where execution resumes after ELSE or IF (when no ELSE
is present).

TYPE ( addr n -- )
Print n characters beginning at address.

U. ( u -- )
Display u converted to BASE as an unsigned number.

UM* ( u u -- ud )
Multiply u1 by u2. Leave 32-bit result ud.

UM/MOD ( ud u1 -- u2 u3 )
Divide unsigned double integer (ud) with unsigned integer (u1). Leave unsigned
remainder (u2) and unsigned quotient (u3).

UNHIDE
Unhide (make visible) the most recently made dictionary entry.

UNTIL ( flag -- )
Within a colon-definition, mark the end of a BEGIN-UNTIL loop, which will
terminate based on flag. If flag is true, the loop is terminated. If flag is
false, execution returns to the first word after BEGIN. BEGIN-UNTIL structures
may be nested.

VARIABLE
A defining word executed in the form: VARIABLE  <name>
Create a dictionary entry for <name> and allot two bytes for storage in the
parameter field. The application must initialize the stored value. When <name>
is later executed, it will place the storage address on the stack.

WHILE ( flag -- )
Used in the form: BEGIN ... flag WHILE ... REPEAT
Select conditional execution based on flag. On a true flag, continue execution
through to REPEAT, which then returns back to just after BEGIN. On a false
flag, skip execution to just after REPEAT, exiting the structure.

WORD ( c -- addr )
Receive characters from the input stream until the non-zero delimiting
character c is encountered or the input stream is exhausted, ignoring leading
delimiters. The characters are stored as a packed string with the character
count in the first character position. The actual delimiter encountered (char
or null) is stored at the end of the text but not included in the count. If
the input stream was exhausted as WORD is called, then a zero length will
result. The address of the beginning of this packed string is left on the
stack.

WORDS
List the word names in the dictionary.

WRITE-FILE ( addr n id --- ior )
Write n bytes to a file. Destination address is addr, n is a number of bytes
to write, id is file id, and ior is equal to I/O operation's result.   

WRITE-LINE ( addr n id --- ior )
Write n bytes to a file. A carriage return is added to the string if needed.
Destination address is addr, n is a number of bytes to write, id is file id,
and ior is equal to I/O operation's result.   

XOR ( n1 n2 -- n3 )
Leave the bitwise exclusive-or of two numbers.

XR ( -- addr )
Leave the address of X register variable. Used with SYS word.

YR ( -- addr )
Leave the address of Y register variable. Used with SYS word.
```

Further Reading
===============

Forth Programmer’s Handbook,  Edward K. Conklin, Elizabeth D. Rather

MOVING FORTH, Part 1: Design Decisions in the Forth Kernel
http://www.bradrodriguez.com/papers/moving1.htm

Starting FORTH, Leo Brodie:
https://www.forth.com/starting-forth/


; SturmForth - subroutine threaded code (STC) FORTH interpreter
; Coded by Juha Ollila
;

	.setcpu		"6502"
	.include	"define.asm"
	.feature	dollar_is_pc

; In Forth, NEXT routine jumps to the next word.
; In STC interpreter it is a return from subroutine.
.macro NEXT
	rts
.endmacro

; In Forth, DOCOL is the interpreter function for : definitions
; In STC interpreter it is no op.
;.macro DOCOL
;.endmacro

basic:
	; .org	$801-2
	.word	*+2	; start address

	; 2019 SYS 2062
	.word	@nextbasicline
	.word	2020
	.byte	$9e, " 2062", $00
@nextbasicline:
	.word	0

init:	jmp	cold


; Format of word definition:
;
; pointer to previous word (2 bytes)
; lenght of name + flags (1 byte)
; name (1-31 bytes)
; code (1-n bytes)


.macro	defcode name, flags
:	.word	:--
	.local	name1, name2
	.byte	flags + name2-name1
name1:	.byte	name
name2:
.endmacro

; word definition flags:
	FLAG_I	= $40	; immediate
	FLAG_H	= $80	; hidden
	FLAG_M	= $1f	; length mask

; push value to the data stack
.macro	push	value
	lda	#<value
	sta	DSTACK,x
	inx
	lda	#>value
	sta	DSTACK,x
	inx
.endmacro

;
; *** CHECK STACK USAGE ***
;

checkoflow:
	cpx	#MAXSTACK
	bcs	@stackoverflow
	rts
@stackoverflow:
	jsr	primm
	.byte   "stack overflow",eol,0
	jmp	abort
check6:
	cpx	#12
	bcc	stackunderflow
	rts			; return from subs can be omitted for size optimization
check4:
	cpx	#8
	bcc	stackunderflow
	rts
check3:
	cpx	#6
	bcc	stackunderflow
	rts
check2:
	cpx	#4
	bcc	stackunderflow
	rts
check1:
	cpx	#2
	bcc	stackunderflow
	rts
stackunderflow:
	jsr	primm
	.byte   "stack underflow",eol,0
	jmp	abort

;
; *** TAIL OF THE LINKED LIST ***
;

:	.word	0
	.byte	0


;
; *** STACK MANIPULATION ***
;

; DROP ( n --- )
; remove the top entry from the stack.
	defcode "drop", 0
drop:
	jsr	check1
	dex
	dex
	NEXT

; DUP ( n --- n n )
; duplicate the top entry on the stack.
	defcode "dup", 0
dup:
	jsr	check1
	lda	DSTACK-2,x
	sta	DSTACK,x
	lda	DSTACK-1,x
	sta	DSTACK+1,x
	inx
	inx
	jsr	checkoflow
	NEXT

; OVER ( n1 n2 --- n1 n2 n1 )
; duplicate the second item on the stack.
	defcode "over", 0
over:
	jsr	check2
	lda	DSTACK-4,x
	sta	DSTACK,x
	lda	DSTACK-3,x
	sta	DSTACK+1,x
	inx
	inx
	jsr	checkoflow
	NEXT

; ROT ( n1 n2 n3 --- n2 n3 n1 )
; rotate the third item to the top of the stack.
	defcode "rot", 0
rot:	jsr	check3
	; save n1
	lda	DSTACK-6,x
	pha
	lda	DSTACK-5,x
	pha
	; n1 = n2
	lda	DSTACK-4,x
	sta	DSTACK-6,x
	lda	DSTACK-3,x
	sta	DSTACK-5,x
	; n2 = n3
	lda	DSTACK-2,x
	sta	DSTACK-4,x
	lda	DSTACK-1,x
	sta	DSTACK-3,x
	; n3 = saved n1
	pla
	sta	DSTACK-1,x
	pla
	sta	DSTACK-2,x
	NEXT

; SWAP ( n1 n2 --- n2 n1 )
; swap the first and the second item on the stack.
	defcode "swap", 0
swap:	jsr	check2
	; save n1
	lda	DSTACK-4,x
	pha
	lda	DSTACK-3,x
	pha
	; n1 = n2
	lda	DSTACK-2,x
	sta	DSTACK-4,x
	lda	DSTACK-1,x
	sta	DSTACK-3,x
	; n2 = saved n1
	pla
	sta	DSTACK-1,x
	pla
	sta	DSTACK-2,x
	NEXT

; ?DUP ( n --- n (n) )
;  duplicate the top entry of the stack if it is non zero.
	defcode "?dup", 0
qdup:	jsr	check1
	lda	DSTACK-1,x
	ora	DSTACK-2,x
	beq	@zero
	jmp	dup
@zero:	NEXT

; DEPTH ( --- n )
; count a number of items on stack.
	defcode "depth", 0
depth:	txa
	lsr
	sta	DSTACK,x
	lda	#0
	sta	DSTACK+1,x
	inx
	inx
	jsr	checkoflow
	NEXT

; PICK ( n1 --- n2 )
; copy n'th item to the top of the stack.          
; 0 PICK is equivalent to DUP   
; 1 PICK is equivalent to OVER
	defcode "pick", 0
pick:	
	stx	XSAVE
	; get n1 and ignore high byte
	lda	DSTACK-2,x
	tay
@pick1: ; x = x-2 for each count
	dey
	bmi	@pick2
	dex
	dex
	bmi	@error
	jmp	@pick1
@pick2:
	jsr	check2
	; copy item
	lda	DSTACK-4,x
	sta	TMP1
	lda	DSTACK-3,x
	sta	TMP2
	ldx	XSAVE
	lda	TMP1
	sta	DSTACK-2,x
	lda	TMP2
	sta	DSTACK-1,x
	NEXT
@error:	jmp	stackunderflow

; >R ( n --- )
; move the top entry of the stack to the return stack.
	defcode ">r", 0
tor:
	jsr	check1
	; save top item
	lda	DSTACK-2,x
	sta	TMP1
	lda	DSTACK-1,x
	sta	TMP2
	; adjust and save stack pointer
	dex
	dex
	stx	XSAVE
	; move return address in the return stack
	tsx
	lda	$101,x
	.byte	$9d, $ff, $00 ;	sta $00ff,x
	lda	$102,x
	sta	$100,x
	; copy top item to the return stack
	lda	TMP1
	sta	$101,x
	lda	TMP2
	sta	$102,x
	; adjust return stack pointer
	dex
	dex
	txs
	; x was modified so we have to load the correct value
	ldx	XSAVE
	NEXT

; R> ( --- n )
; move the top entry of the return stack to the stack.
	defcode "r>", 0
rfrom:
	stx	XSAVE
	; save an item from the return stack.
	tsx
	lda	$103,x
	sta	TMP1
	lda	$104,x
	sta	TMP2
	; adjust return address and the return stack sp
	lda	$101,x
	sta	$103,x
	lda	$102,x
	sta	$104,x
	inx
	inx
	txs
	; copy an item to the stack
	ldx	XSAVE
	lda	TMP1
	sta	DSTACK,x
	lda	TMP2
	sta	DSTACK+1,x
	; adjust stack pointer
	inx
	inx
	jsr	checkoflow
	NEXT

; R@ ( --- n )
; copy the top entry of the return stack to the stack.
	defcode "r@", 0
rfetch:
	stx	XSAVE
	; save an item from the return stack.
	tsx
	lda	$103,x
	sta	TMP1
	lda	$104,x
	sta	TMP2
	; copy an item to the stack
	ldx	XSAVE
	lda	TMP1
	sta	DSTACK,x
	lda	TMP2
	sta	DSTACK+1,x
	; adjust stack pointer
	inx
	inx
	jsr	checkoflow
	NEXT

; 0 ( --- 0 )
; push zero to the stack.
	defcode "0", 0
zero:	lda	#0
	sta	DSTACK,x
	inx
	sta	DSTACK,x
	inx
	jsr	checkoflow
	NEXT

; 1 ( --- 1 )
; push one to the stack.
	defcode "1", 0
one:	ldy	#1
	sty	DSTACK,x
	inx
	dey
	sty	DSTACK,x
	inx
	jsr	checkoflow
	NEXT

; 2 ( --- 2 )
; push two to the stack.
	defcode "2", 0
two:	lda	#2
	sta	DSTACK,x
	inx
	lda	#0
	sta	DSTACK,x
	inx
	jsr	checkoflow
	NEXT

;
; DOUBLE NUMBER STACK MANIPULATION
;

; 2DROP ( d --- )
; remove double number (or two items) from the stack.
	defcode "2drop", 0
twodrop:
	jsr	check2
	dex
	dex
	dex
	dex
	NEXT

; 2DUP ( d --- d d )
;      ( n1 n2 --- n1 n2 n1 n2 )
; duplicate the top double number on the stack.
	defcode "2dup", 0
twodup:
	jsr	check2
	jsr	over
	jsr	over
	jsr	checkoflow
	NEXT

; 2OVER ( d1 d2 --- d1 d2 d1 )
;       ( n1 n2 n3 n4 --- n1 n2 n3 n4 n1 n2
; duplicate the second double number on the stack.
	defcode "2over", 0
twoover:
	jsr	check4
	jsr	tor		; d1
	jsr	tor
	jsr	twodup		; d1 d1
	jsr	rfrom		; d1 d1 d2
	jsr	rfrom
	jsr	twoswap		; d1 d2 d1
	jsr	checkoflow
	NEXT

; 2ROT ( d1 d2 d3 --- d2 d3 d1 )
;      ( n1 n2 n3 n4 n5 n6 --- n3 n4 n5 n6 n1 n2 )
; rotate the third double number to the top of the stack.
	defcode "2rot", 0
tworot:	jsr	check6
	jsr	tor		; d1 d2
	jsr	tor
	jsr	twoswap		; d2 d1
	jsr	rfrom		; d2 d1 d3
	jsr	rfrom
	jsr	twoswap		; d2 d3 d1
	NEXT

; 2SWAP ( d1 d2 --- d2 d1 )
;       ( n1 n2 n3 n4 --- n3 n4 n1 n2
; swap the first and the second double number on the stack.
	defcode "2swap", 0
twoswap:
	jsr	check4
	; save d1
	lda	DSTACK-8,x
	pha
	lda	DSTACK-7,x
	pha
	lda	DSTACK-6,x
	pha
	lda	DSTACK-5,x
	pha
	; d1 = d2
	lda	DSTACK-4,x
	sta	DSTACK-8,x
	lda	DSTACK-3,x
	sta	DSTACK-7,x
	lda	DSTACK-2,x
	sta	DSTACK-6,x
	lda	DSTACK-1,x
	sta	DSTACK-5,x
	; d2 = saved d1
	pla
	sta	DSTACK-1,x
	pla
	sta	DSTACK-2,x
	pla
	sta	DSTACK-3,x
	pla
	sta	DSTACK-4,x
	NEXT

; *** LITERAL ***
;

; LITERAL ( n --- ) 
; compile literal
	defcode "literal", FLAG_I
literal:
	jsr	check1
	push	$20		; store "jsr"
	jsr	ccomma
	push	lit		; store address of lit
	jsr	comma
	jsr	comma		; store number
	NEXT

; LIT ( --- n ) in runtime
; pushes (compiled) n to the stack
	defcode "lit", 0
lit:
	txa			; save data stack ptr
	tay
	tsx			; modify return address
	clc
	lda	$101,x
	sta	TMP1
	adc	#2
	sta	$101,x
	lda	$102,x
	sta	TMP2
	adc	#0
	sta	$102,x
	tya			; restore data stack ptr
	tax
	ldy	#1		; push literal to the stack
	lda	(TMP1),y
	sta	DSTACK,x
	iny
	inx
	lda	(TMP1),y
	sta	DSTACK,x
	inx
	jsr	checkoflow
	NEXT

;
; *** BUILT IN VARIABLES ***
;

.macro	variable location
	lda	#<location
	sta	DSTACK,x
	inx
	lda	#>location
	sta	DSTACK,x
	inx
	jsr	checkoflow
	NEXT
.endmacro

	defcode "state", 0
state:
	variable STATE

	defcode "here", 0
here:
	variable HEREPTR

	defcode "last", 0
last:
	variable LASTPTR

	defcode "base", 0
base:
	variable BASE

	defcode "device", 0
device:
	variable DEVICE

	defcode "ac", 0
	variable ACC

	defcode "xr", 0
	variable XR

	defcode "yr", 0
	variable YR

	defcode "sr", 0
	variable SR


; HEX ( --- )
; changes base to hex
	defcode "hex", 0
hex:
	lda	#16
	sta	BASE
	NEXT

; DECIMAL ( --- )
; changes base to decimal
	defcode "decimal", 0
decimal:
	lda	#10
	sta	BASE
	NEXT

;
; *** INPUT/OUTPUT ***
;

; . ( n --- )
; print value of n
	defcode ".", 0
dot:	jsr	check1
	lda	DSTACK-2,x
	ldy	DSTACK-1,x
	dex
	dex
	pha
	lda	#10
	cmp	BASE
	bne	@dot1
	pla
	jsr	printdec
	jmp	printspc
@dot1:	pla
	jsr	printint
	jsr	printspc
	NEXT

; ? ( addr --- )
; print value at addr
	defcode "?", 0
qmark:
	jsr	fetch
	jsr	dot
	NEXT

; U. ( n --- )
; print unsigned value of n
	defcode "u.", 0
udot:	jsr	check1
	lda	DSTACK-2,x
	ldy	DSTACK-1,x
	dex
	dex
	pha
	lda	#10
	cmp	BASE
	bne	@udot1
	pla
	jsr	printudec
	jmp	printspc
@udot1:	pla
	jsr	printuint
	jsr	printspc
	NEXT

; D. ( d --- )
; print double number
	defcode "d.", 0
ddot:
	jsr	check2
	jsr	swap		; n2 n1
	jsr	over		; n2 n1 n2
	jsr	dabs
	jsr	bracknum
	jsr	nums		; n2 0 0
	jsr	rot		; 0 0 n2
	jsr	sign
	jsr	numbrack
	jsr	type
	jsr	printspc
	NEXT

; KEY ( --- n )
; read a key from the keyboard
	defcode "key", 0
key:
	stx	XSAVE
	jsr	getin
	ldx	XSAVE
	cmp	#0
	beq	key	; branch if no key was pressed
	sta	DSTACK,x
	inx
	lda	#0
	sta	DSTACK,x
	inx
	jsr	checkoflow
	NEXT

; CR ( --- )
; print an end of line character
	defcode "cr", 0
printcr:
	lda	#eol
	jsr	chrout
	NEXT

; SPACE ( --- )
; print a space character
	defcode "space", 0
printspc:
	lda	#space
	jsr	chrout
	NEXT

; SPACES ( n --- )
; print n space characters
	defcode "spaces", 0
spaces:	jsr	check1
@spaces1:
	jsr	printspc
	dec	DSTACK-2,x
	bne	@spaces1
	dex
	dex
	NEXT

; EMIT ( n --- )
; print a character (petscii code n)
	defcode "emit", 0
emit:	jsr	check1
	lda	DSTACK-2,x
	jsr	chrout
	dex
	dex
	NEXT

; ." ( --- )
; compile print a string (terminated with ")
:	.word	:--		; defcode ".\"", FLAG_I
	.byte	2 + FLAG_I
	.byte	'.', '"'
dotquote:
	push	quote
	jsr	word
	lda	DSTACK-2,x
	sta	TMP3
	lda	DSTACK-1,x
	sta	TMP4
	dex			; remove address of string from the data stack
	dex
	ldy	#0		; check whether a string was found
	lda	(TMP3),y
	beq	@error
	push	$20		; compile jsr primm
	jsr	ccomma		; ccomma or comma should not tamper TMP3 and TMP4!!!
	push	primm
	jsr	comma
@dotquote1:
	ldy	#2
	sty	STRCNT
@dotquote2:
	ldy	STRCNT
	lda	(TMP3),y
	cmp	#eol		; branch to end if eol
	beq	@dotquote4
	cmp	#quote		; branch to end if quote char
	beq	@dotquote3
	sta	DSTACK,x
	inx
	lda	#0		
	sta	DSTACK,x
	inx
	jsr	ccomma		; compile the byte
	inc	STRCNT
	bne	@dotquote2
	beq	@dotquote4
@dotquote3:
	inc	CPTR		; HACK: ignore the following quote in the input stream
@dotquote4:
	push	0		; compile zero terminator for the primm
	jsr	ccomma	
	NEXT
@error:
	jsr	primm
	.byte	"no string",eol,0
	jmp	abort

; C" ( --- ) compile time
;    ( --- addr ) runtime
; compile a string
:	.word	:--		; defcode "C\"", FLAG_I
	.byte	2 + FLAG_I
	.byte	'c', '"'
cquote:
	push	quote
	jsr	word
	lda	DSTACK-2,x
	sta	TMP3
	lda	DSTACK-1,x
	sta	TMP4
	jsr	cfetch		; stack: len+1
	jsr	oneminus	; stack: len
	ldy	#0		; check whether a string was found
	lda	(TMP3),y
	beq	@error
	jsr	here
	jsr	fetch
	push	10		; jsr lit addr1 jsr branch addr2
	jsr	plus		; stack: len here+10
	jsr	literal
	push	$20		; jsr branch
	jsr	ccomma
	push	branch
	jsr	comma
	jsr	here		; copy location of branch operand to data stack
	jsr	fetch
	push	0		; dummy address
	jsr	comma
	jsr	swap		; stack: addr_of_operand len
	jsr	ccomma
@cquote1:
	ldy	#2
	sty	STRCNT
@cquote2:
	ldy	STRCNT
	lda	(TMP3),y
	cmp	#eol		; branch to end if eol
	beq	@cquote4
	cmp	#quote		; branch to end if quote char
	beq	@cquote3
	sta	DSTACK,x
	inx
	lda	#0		
	sta	DSTACK,x
	inx
	jsr	ccomma		; compile the byte
	inc	STRCNT
	bne	@cquote2
	beq	@cquote4
@cquote3:
	inc	CPTR		; HACK: ignore the following quote in the input stream
@cquote4:
	jsr	here		; update jsr branch addr2
	jsr	fetch
	jsr	swap
	jsr	store
	NEXT
@error:
	jsr	primm
	.byte	"no string",eol,0
	jmp	abort

; S" ( --- addr )
; compile a string
:	.word	:--		; defcode "S\"", FLAG_I
	.byte	2
	.byte	's', '"'
squote:
	push	quote
	jsr	word
	lda	DSTACK-2,x
	sta	TMP3
	lda	DSTACK-1,x
	sta	TMP4
	jsr	cfetch		; stack: len+1
	jsr	oneminus	; stack: len
	ldy	#0		; check whether a string was found
	lda	(TMP3),y
	beq	@error
	jsr	here
	jsr	fetch		; stack: len addr
	jsr	swap		; stack: addr len
@squote1:
	ldy	#2		; ignore len and space
	sty	STRCNT
@squote2:
	ldy	STRCNT
	lda	(TMP3),y
	cmp	#eol		; branch to end if eol
	beq	@squote4
	cmp	#quote		; branch to end if quote char
	beq	@squote3
	sta	DSTACK,x
	inx
	lda	#0		
	sta	DSTACK,x
	inx
	jsr	ccomma		; compile the byte
	inc	STRCNT
	bne	@squote2
	beq	@squote4
@squote3:
	inc	CPTR		; HACK: ignore the following quote in the input stream
@squote4:
	NEXT
@error:
	jsr	primm
	.byte	"no string",eol,0
	jmp	abort


; .( ( --- )
; print a string which is terminated with )
	defcode ".(", 0
dotparen:
	push	')'
	jsr	word
	lda	DSTACK-2,x
	sta	TMP3
	lda	DSTACK-1,x
	sta	TMP4
	dex			; remove address of string from the data stack
	dex
	ldy	#0		; check whether a string was found
	lda	(TMP3),y
	beq	@error
@dotquote1:
	ldy	#2		; ignore len and space
@dotquote2:
	lda	(TMP3),y
	cmp	#eol		; branch to end if eol
	beq	@dotquote4
	cmp	#')'		; branch to end if quote char
	beq	@dotquote3
	jsr	chrout
	iny
	bne	@dotquote2
	beq	@dotquote4
@dotquote3:
	inc	CPTR		; HACK: ignore the following char in the input stream
@dotquote4:
	NEXT
@error:
	jsr	primm
	.byte	"no string",eol,0
	jmp	abort

; count ( addr1 --- addr2 n )
; addr1 is a ptr to a counted string
; addr2 = addr1+1, n = count
	defcode "count", 0
count:
	jsr	check1
	ldy	#0		; get counted string addr
	lda	DSTACK-2,x
	sta	TMP1
	lda	DSTACK-1,x
	sta	TMP2
	lda	(TMP1),y	; save count
	sta	DSTACK,x
	sty	DSTACK+1,x	; high byte of n = 0
	inc	DSTACK-2,x	; addr2 = addr1+1
	bne	@count1
	inc	DSTACK-1,x
@count1:
	inx			; adjust data stack ptr
	inx
	NEXT

; type ( addr n --- )
; print a string, addr = starting address, n = len of string
	defcode "type", 0
type:
	jsr	check2
	ldy	#0		; get string addr
	lda	DSTACK-4,x
	sta	TMP1
	lda	DSTACK-3,x
	sta	TMP2
@type1:
	dec	DSTACK-2,x
	bmi	@type2
	lda	(TMP1),y
	iny
	jsr	chrout
	jmp	@type1
@type2:
	jsr	twodrop
	NEXT

;
; *** NUMBER CONVERSION ***
;

; <# ( ud --- ud )
; initialize number conversion
	defcode "<#", 0
bracknum:
	jsr	check2
	lda	#0
	sta	NUMLEN
	lda	#<nbuffer
	sta	NUMPTR
	lda	#>nbuffer
	sta	NUMPTR+1
	NEXT

; sign ( n --- )
; add sign to the number string.
	defcode "sign", 0
sign:
	jsr	check1
	lda	DSTACK-1,x
	bpl	@sign2
	lda	#'-'
	ldy	NUMPTR
	sty	TMP1
	ldy	NUMPTR+1
	sty	TMP2
	ldy	#0
	sta	(TMP1),y
	dec	NUMPTR		; numstring ptr -= 1
	bpl	@sign1
	dec	NUMPTR+1
@sign1:
	inc	NUMLEN		; numstring len += 1
@sign2:
	dex
	dex
	NEXT

; # ( ud1 --- ud2 )
; divide ud1 by BASE. add the remainder to the number string.
	defcode "#", 0
numsign:
	jsr	check2
	jsr	base
	jsr	fetch		; u1 base
	jsr	ummod32		; n1 ud1l ud1h
	jsr	rot		; ud1l ud1h n1
	clc			; convert digit to char
	lda	DSTACK-2,x
	adc	#'0'
	cmp	#':'
	bcc	numsign2
	clc
	adc	#7		; 'a'-':'
numsign2:
	ldy	NUMPTR		; store char to numstring
	sty	TMP1
	ldy	NUMPTR+1
	sty	TMP2
	ldy	#0
	sta	(TMP1),y
	dec	NUMPTR		; numstring ptr -= 1
	bpl	@numsign3
	dec	NUMPTR+1
@numsign3:
	inc	NUMLEN		; numstring len += 1
	dex			; remove quotient+'0' from stack
	dex
	NEXT

; #S ( ud1 --- ud2 )
; convert number ud1 to the number string.
	defcode "#s", 0
nums:	jsr	check2
@nums1:
	jsr	numsign
	lda	DSTACK-4,x
	ora	DSTACK-3,x
	ora	DSTACK-2,x
	ora	DSTACK-1,x
	bne	@nums1
	NEXT	

; HOLD ( char --- )
; char is inserted into the number string.
	defcode "hold", 0
hold:
	jsr	check1
	lda	DSTACK-2,x
	jmp	numsign2

; #> ( ud --- addr n )
; end number conversion
; addr = address of string
; n = number of chars
	defcode "#>", 0
numbrack:
	jsr	check2
	clc			; store numptr+1 to stack
	lda	NUMPTR
	adc	#1
	sta	DSTACK-4,x
	lda	NUMPTR+1
	adc	#0
	sta	DSTACK-3,x
	lda	NUMLEN		; store length of string to stack
	sta	DSTACK-2,x
	lda	#0
	sta	DSTACK-1,x
	NEXT

;
; *** DISK I/O ***
;

; INCLUDE ( --- )
; reads forth file from device
; file name is read from the input stream
	defcode "include", 0
include0:
	stx	XSAVE

	lda	#1		; file number
	ldx	DEVICE
	ldy	#0		; secondary address
	jsr	setlfs

	ldx	XSAVE
	lda	#space		; get name of forth file
	sta	DSTACK,x
	lda	#0
	sta	DSTACK+1,x
	inx
	inx
	jsr	word
	lda	DSTACK-2,x	; string address
	sta	TMP1
	lda	DSTACK-1,x
	sta	TMP2
	dex			; delete addr of string from stack
	dex
	stx	XSAVE

	ldy	#0		; copy string length
	lda	(TMP1),y
	beq	@error1		; branch if no string
	sta	LOAD		; set flag
	inc	TMP1		; skip length byte
	bne	@include1
	inc	TMP2
@include1:
	ldx	TMP1
	ldy	TMP2
	jsr	setnam
	jsr	open
	bcs	@error2
	ldx	#1		; file number
	jsr	chkin
	bcs	@error2
	ldx	XSAVE
	NEXT

@error1:
	jsr	primm
	.byte	"no string",eol,0
	jmp	abort
@error2:
	jsr	primm
	.byte	"disk i/o error",eol,0
	jmp	abort

; OPEN-FILE ( addr n id --- ior )
; opens a file
; addr = addr of file name
; n = len of file name
; id = file id
; ior = io result
	defcode "open-file", 0
openfile:
	jsr	check3
	stx	XSAVE
	lda	DSTACK-6,x	; save addr of filename
	sta	TMP1
	lda	DSTACK-5,x
	sta	TMP2
	lda	DSTACK-4,x	; save filename len
	sta	TMP3

	lda	DSTACK-2,x	; file id
	ldx	DEVICE
	ldy	#2		; secondary address
	cmp	#15		; check command channel
	bne	@nocommand
	tay
@nocommand:
	jsr	setlfs

	lda	TMP3		; filename len
	ldx	TMP1		; addr of filename
	ldy	TMP2
	jsr	setnam

	jsr	open
	bcc	@open1
	lda	#1
	bne	@open2
@open1:
	lda	#0
@open2:
	ldx	XSAVE
	jsr	twodrop
	sta	DSTACK-2,x
	lda	#0
	sta	DSTACK-1,x
	NEXT

; CLOSE-FILE ( id --- ior )
; closes a file
; id = file id
; ior = io result
	defcode "close-file", 0
closefile:
	jsr	check1
	stx	XSAVE
	lda	DSTACK-2,x	; file id
	jsr	close
	bcc	@close1
	lda	#1
	bne	@close2
@close1:
	lda	#0
@close2:
	ldx	XSAVE
	sta	DSTACK-2,x
	lda	#0
	sta	DSTACK-1,x
	NEXT

; READ-FILE ( addr n1 id --- n2 ior )
; reads a file
; addr = dst addr
; n1 = bytes to read
; id = file id
; n2 = number of successfully read bytes
; ior = io result
	defcode "read-file", 0
readfile:
	lda	#0
	sta	FLINE
readfile1:
	jsr	check3
	stx	XSAVE
	lda	#0		; bytes read =  0
	sta	AUX
	sta	AUX+1
	lda	DSTACK-6,x	; get dst addr
	sta	TMP1
	lda	DSTACK-5,x
	sta	TMP2
	lda	DSTACK-4,x	; get number of bytes
	sta	TMP3
	lda	DSTACK-3,x
	sta	TMP4

	lda	DSTACK-2,x	; get file id
	tax
	jsr	chkin

@read1:
	jsr	chrin
	tay			; check eol and linefeed
	lda	FLINE
	beq	@eol2
	cpy	#eol
	beq	@eol1
	cpy	#lfeed
	bne	@eol2
@eol1:
	jsr	clrchn
	lda	#0
	beq	@read4
@eol2:	tya			; save char
	ldy	#0
	sta	(TMP1),y
	inc	TMP1
	bne	@read2
	inc	TMP2
@read2:	jsr	readst
	cmp	#0
	bne	@eof		; branch if eof or error
	inc	AUX		; bytes read = bytes read + 1
	bne	@read3
	inc	AUX+1
@read3:
	clc			; check if n1 bytes has been read
	lda	TMP3
	sbc	AUX
	lda	TMP4
	sbc	AUX+1
	bcs	@read1
	jsr	clrchn
	lda	#0
@read4:
	ldx	XSAVE
	dex
	dex
	sta	DSTACK-2,x	; set ior
	lda	#0
	sta	DSTACK-1,x
	lda	AUX		; n2 = bytes read
	sta	DSTACK-4,x
	lda	AUX+1
	sta	DSTACK-3,x
	NEXT
@eof:
	pha
	jsr	clrchn
	pla
	cmp	#eof
	bne	@read4
	inc	AUX
	bne	@eof2
	inc	AUX+1
@eof2:	jmp	@read4
	

; READ-LINE ( addr n1 id --- n2 ior )
; reads a file until n1 bytes has read or end of line is read
; addr = dst addr
; n1 = bytes to read
; id = file id
; n2 = number of successfully read bytes
; ior = io result
	defcode "read-line", 0
readline:
	lda	#$80
	sta	FLINE
	jmp	readfile1

; WRITE-FILE ( addr n1 id --- ior )
; writes a file
; addr = src addr
; n1 = bytes to write
; id = file id
; ior = io result
	defcode "write-file", 0
writefile:
	lda	#0
	sta	FLINE
writefile1:
	jsr	check3
	stx	XSAVE
	lda	#0		; bytes written =  0
	sta	AUX
	sta	AUX+1
	lda	DSTACK-6,x	; get src addr
	sta	TMP1
	lda	DSTACK-5,x
	sta	TMP2
	lda	DSTACK-4,x	; get number of bytes
	sta	TMP3
	lda	DSTACK-3,x
	sta	TMP4

	lda	DSTACK-2,x	; get file id
	tax
	jsr	chkout

@write1:
	ldy	#0
	lda	(TMP1),y
	;tay
	;lda	FLINE
	;beq	@write1b
	;cmp	#eol
	;bne	@write1b
	;jsr	chrout
	;jmp	@write3b
;@write1b:
	;tya
	jsr	chrout
	inc	TMP1
	bne	@write2
	inc	TMP2
@write2:
	jsr	readst
	cmp	#0
	bne	@error		; branch if error
	inc	AUX		; bytes written = bytes written + 1
	bne	@write3
	inc	AUX+1
@write3:
	clc			; check if n1 bytes has been written
	lda	TMP3
	sbc	AUX
	lda	TMP4
	sbc	AUX+1
	bcs	@write1		; branch if not all bytes have been written
	tay			; check if eol should be written
	lda	FLINE
	beq	@write3b
	cpy	#eol
	beq	@write3b
	lda	#eol
	jsr	chrout
@write3b:
	jsr	clrchn
	lda	#0
@write4:
	ldx	XSAVE
	jsr	twodrop
	sta	DSTACK-2,x	; set ior
	lda	#0
	sta	DSTACK-1,x
	NEXT
@error:
	pha
	jsr	clrchn
	pla
	jmp	@write4

; WRITE-LINE ( addr n1 id --- ior )
; write a line to a file
; addr = dst addr
; n1 = bytes to write
; id = file id
; ior = io result
	defcode "write-line", 0
writeline:
	lda	#$80
	sta	FLINE
	jmp	writefile1

;
; *** MEMORY (peek, poke and copy) ***
;

; ! ( n addr --- )
; store int to the addr
	defcode "!", 0
store:	jsr	check2
	lda     DSTACK-2,x      ; save address
        sta     TMP1
        lda     DSTACK-1,x
        sta     TMP2
	lda	DSTACK-4,x	; save the lsb
	ldy	#0
	sta	(TMP1),y
	lda	DSTACK-3,x	; save the msb
	iny
	sta	(TMP1),y
	dex
	dex
	dex
	dex
	NEXT

; @ ( addr --- n )
; fetches int from the addr
	defcode "@", 0
fetch:	jsr	check1
	lda     DSTACK-2,x      ; save address
        sta     TMP1
        lda     DSTACK-1,x
        sta     TMP2
	ldy	#0		; fetch the lsb
	lda	(TMP1),y
	sta	DSTACK-2,x
	iny			; fetch the msb
	lda	(TMP1),y
	sta	DSTACK-1,x
	NEXT

; C! ( n addr --- )
; store byte to the addr
	defcode "c!", 0
cstore:	jsr	check2
	lda     DSTACK-2,x      ; save address
        sta     TMP1
        lda     DSTACK-1,x
        sta     TMP2
	lda	DSTACK-4,x	; save only the lsb
	ldy	#0
	sta	(TMP1),y
	dex
	dex
	dex
	dex
	NEXT

; C@ ( addr --- n )
; fetches byte from the addr
	defcode "c@", 0
cfetch:	jsr	check1
	lda     DSTACK-2,x      ; save address
        sta     TMP1
        lda     DSTACK-1,x
        sta     TMP2
	ldy	#0		; fetch the lsb
	lda	(TMP1),y
	sta	DSTACK-2,x
	tya			; msb is always 0
	sta	DSTACK-1,x
	NEXT

; +! ( n addr --- )
; n is added to the value at addr
	defcode "+!", 0
plusstore:
	jsr	check2
	jsr	swap	; ( addr n )
	jsr	over	; ( addr n addr )
	jsr	fetch	; ( addr n v )
	jsr	plus	; ( addr n+v )
	jsr	swap	; ( n+v addr )
	jsr	store	; ( )
	NEXT

; CMOVE (src dst n --- )
; copy n bytes from source to destination (from low to high)

	defcode "cmove", 0
cmove:	jsr	check3
	jsr	initcmove
@copy2:
@copy2a:
	lda	AUX
	ora	AUX+1
	beq	@exit
	lda	(TMP1),y
	sta	(TMP3),y
	inc	TMP1
	bne	@copy2b
	inc	TMP2
@copy2b:
	inc	TMP3
	bne	@copy2c
	inc	TMP4
@copy2c:
	dec	AUX
	lda	AUX
	cmp	#$ff
	bne	@copy2d
	dec	AUX+1
@copy2d:
	sec
	bcs	@copy2a
@exit:	NEXT

; <CMOVE (src dst n --- )
; copy n bytes from source to destination (from high to low)
	defcode "<cmove", 0
revcmove:
	jsr	check3
	jsr	initcmove
@copy3:
	clc
	lda	TMP1
	adc	AUX
	sta	TMP1
	lda	TMP2
	adc	AUX+1
	sta	TMP2
	clc
	lda	TMP3
	adc	AUX
	sta	TMP3
	lda	TMP4
	adc	AUX+1
	sta	TMP4
@copy3a:
	lda	AUX
	ora	AUX+1
	beq	@exit

	dec	TMP1
	lda	TMP1
	cmp	#$ff
	bne	@copy3b
	dec	TMP2
@copy3b:
	dec	TMP3
	lda	TMP3
	cmp	#$ff
	bne	@copy3c
	dec	TMP4
@copy3c:
	lda	(TMP1),y
	sta	(TMP3),y
	dec	AUX
	lda	AUX
	cmp	#$ff
	bne	@copy3d
	dec	AUX+1
@copy3d:
	sec
	bcs	@copy3a
@exit:
	NEXT

; FILL ( addr n byte --- )
; fill memory beginning at address with a sequence of n copies of byte.
	defcode "fill", 0
fill:
	jsr	initcmove
@memset1:
	lda	TMP3
	ora	TMP4
	beq	@exit
	lda	AUX
	sta	(TMP1),y
@memset2:
	inc	TMP1
	bne	@memset3
	inc	TMP2
@memset3:
	dec	TMP3
	lda	TMP3
	cmp	#$ff
	bne	@memset4
	dec	TMP4
@memset4:
	sec
	bcs	@memset1
@exit:
	NEXT

; routine used by cmove, <cmove and fill
initcmove:
	lda	DSTACK-6,x
	sta	TMP1
	lda	DSTACK-5,x
	sta	TMP2
	lda	DSTACK-4,x
	sta	TMP3
	lda	DSTACK-3,x
	sta	TMP4
	lda	DSTACK-2,x
	sta	AUX
	lda	DSTACK-1,x
	sta	AUX+1
	dex
	dex
	dex
	dex
	dex
	dex
	ldy	#0
	rts


; CONSTANT ( n --- )
; create a constant, e.g. 0 constant nil
	defcode "constant", 0
constant:
	jsr	check1
	jsr	create1
	jsr	literal
	push	$60		; rts
	jsr	ccomma
	NEXT

; VARIABLE ( --- )
; create a variable, e.g. variable var
	defcode "variable", 0
var:
	jsr	create
	jsr	two
	jsr	allot
	NEXT

; PAD ( --- )
; return ptr to scrath pad area
	defcode "pad", 0
pad:
	jsr	here
	jsr	fetch
	push	1024
	jsr	plus
	NEXT

;
; MEMORY DOUBLE NUMBER
;

; 2! ( d addr --- )
; store double number
	defcode "2!", 0
twostore:
	jsr	check3
	jsr	two		; n1 n2 addr 2
	jsr	pick		; n1 n2 addr n1
	jsr	over		; n1 n2 addr n1 addr
	jsr	store		; n1 n2 addr
	jsr	twoplus		; n1 n2 addr+2
	jsr	store		; n1
	jsr	drop
	NEXT

; 2@ ( addr --- d )
; fetch double number
	defcode "2@", 0
twofetch:
	jsr	check1
	jsr	dup		; addr addr
	jsr	fetch		; addr n1
	jsr	swap		; n1 addr
	jsr	twoplus		; n1 addr+2
	jsr	fetch		; n1 n2
	NEXT

; 2CONSTANT ( d --- )
; create a double number constant
	defcode "2constant", 0
twoconstant:
	jsr	check2
	jsr	create1
	jsr	swap		; n2 n1
	jsr	literal		; n2
	jsr	literal		;
	push	$60		; rts
	jsr	ccomma
	NEXT

; 2VARIABLE ( --- )
; create a double number variable, e.g. variable var
	defcode "2variable", 0
twovar:
	jsr	create
	push	4
	jsr	allot
	NEXT


;
; *** ARITHMETIC ***
;

; + ( n1 n2 --- n1+n2 )
; add
	defcode "+", 0
plus:	jsr	check2
	clc
	lda	DSTACK-4,x
	adc	DSTACK-2,x
	sta	DSTACK-4,x
	lda	DSTACK-3,x
	adc	DSTACK-1,x
	sta	DSTACK-3,x
	dex
	dex
	NEXT

; - ( n1 n2 --- n1-n2 )
; subtract
	defcode "-", 0
minus:	jsr	check2
	sec
	lda	DSTACK-4,x
	sbc	DSTACK-2,x
	sta	DSTACK-4,x
	lda	DSTACK-3,x
	sbc	DSTACK-1,x
	sta	DSTACK-3,x
	dex
	dex
	NEXT

; 1+ ( n --- n+1 )
; add one.
	defcode "1+", 0
oneplus:
	jsr	check1
	clc
	lda	DSTACK-2,x
	adc	#1
	sta	DSTACK-2,x
	bcc	@nocarry
	inc	DSTACK-1,x
@nocarry:
	NEXT

; 2+ ( n --- n+2 )
; add two.
	defcode "2+", 0
twoplus:
	jsr	check1
	clc
	lda	DSTACK-2,x
	adc	#2
	sta	DSTACK-2,x
	bcc	@nocarry
	inc	DSTACK-1,x
@nocarry:
	NEXT

; 1- ( n --- n-1 )
; subtract one
	defcode "1-", 0
oneminus:
	jsr	check1
	sec
	lda	DSTACK-2,x
	sbc	#1
	sta	DSTACK-2,x
	bcs	@carry
	dec	DSTACK-1,x
@carry:
	NEXT

; 2- ( n --- n-2 )
; subtract two
	defcode "2-", 0
twominus:
	jsr	check1
	sec
	lda	DSTACK-2,x
	sbc	#2
	sta	DSTACK-2,x
	bcs	@carry
	dec	DSTACK-1,x
@carry:
	NEXT

; 2* ( n --- n*2 )
; multiply by two
	defcode "2*", 0
twomul:	jsr	check1
	asl	DSTACK-2,x
	rol	DSTACK-1,x
	NEXT

; 2/ ( n --- n/2 )
; divide by two
	defcode "2/", 0
twodiv:	jsr	check1
	lsr	DSTACK-1,x
	ror	DSTACK-2,x
	NEXT

; NEGATE ( n --- -n )
	defcode "negate", 0
negate:	jsr	check1
	lda	#0
	sec
	sbc	DSTACK-2,x
	sta	DSTACK-2,x
	lda	#0
	sbc	DSTACK-1,x
	sta	DSTACK-1,x
	NEXT

mulsign:
	lda	DSTACK-3,x	; check sign of result
	eor	DSTACK-1,x
	sta	SIGN		; save sign
	; negate n1 if needed
	lda	DSTACK-3,x
	bpl	@mula
	jsr	swap
	jsr	negate
	jsr	swap
	; negate n2 if needed
@mula:	lda	DSTACK-1,x
	bpl	@mulb
	jsr	negate
@mulb:
	rts

; * ( n1 n2 --- n1*n2 )
; multiply.
	defcode "*", 0
mul:	jsr	check2
	jsr	mulsign
@mul0:	lda	#0
	sta	TMP1
	sta	TMP2
@mul1:	lda	DSTACK-4,x	; operand 1
	ora	DSTACK-3,x
	beq	@mul3
	lsr	DSTACK-3,x	; shift operand 1 right
	ror	DSTACK-4,x
	bcc	@mul2		; if carry clear no addition to previous products
	clc			; else add operand 2 to partial result
	lda	TMP1
	adc	DSTACK-2,x
	sta	TMP1
	lda	TMP2
	adc	DSTACK-1,x
	sta	TMP2
@mul2:	asl	DSTACK-2,x	; shift operand 2 left
	rol	DSTACK-1,x
	jmp	@mul1
@mul3:
	lda	TMP1		; save result and adjust stack pointer
	sta	DSTACK-4,x
	lda	TMP2
	sta	DSTACK-3,x
	lda	SIGN		; adjust sign if needed
	bpl	@mul4
	jsr	negate
@mul4:
	dex
	dex
	NEXT

; /MOD ( n1 n2 --- (n1 mod n2) n1/n2 )
; divide.
	defcode "/mod", 0
divmod:	jsr	check2
	lda	DSTACK-3,x	; check sign of result
	eor	DSTACK-1,x
	php			; save sign
	; negate n1 if needed
	lda	DSTACK-3,x
	bpl	@diva
	jsr	swap
	jsr	negate
	jsr	swap
	; negate n2 if needed
@diva:	lda	DSTACK-1,x
	bpl	@div0
	jsr	negate
@div0:
	lda 	#0
	sta 	TMP2
	ldy	#$10
@div1:	asl	DSTACK-4,x
	rol	DSTACK-3,x
	rol
	rol	TMP2
	pha
	cmp	DSTACK-2,x
	lda	TMP2
	sbc	DSTACK-1,x
	bcc	@div2
	sta	TMP2
	pla
	sbc	DSTACK-2,x
	pha
	inc	DSTACK-4,x
@div2:	pla
	dey
	bne	@div1
	sta	DSTACK-2,x
	lda	TMP2
	sta	DSTACK-1,x
	plp			; adjust sign if needed
	php
	bpl	@div3
	jsr	negate
@div3:	jsr	swap
	plp			; adjust sign if needed
	bpl	@div4
	jsr	negate
@div4:
	NEXT

; MOD ( n1 n2 --- (n1 mod n2) )
	defcode "mod", 0
mod:	jsr	divmod
	jsr	drop
	NEXT

; / ( n1 n2 --- n1/n2 )
	defcode "/", 0
divide:	jsr	divmod
	jsr	swap
	jsr	drop
	NEXT

; MIN ( n1 n2 --- min )
; leave lesser of two items.
	defcode "min", 0
min:	jsr	check2
	jsr	over
	jsr	over
	jsr	less
	lda	DSTACK-2,x
	beq	@min1
	jsr	drop
	jmp	drop
@min1:	jsr	drop
	jsr	swap
	jsr	drop
	NEXT

; MAX ( n1 n2 --- max )
; leave greater of two items.
	defcode "max", 0
max:	jsr	check2
	jsr	over
	jsr	over
	jsr	greater
	lda	DSTACK-2,x
	beq	@max1
	jsr	drop
	jmp	drop
@max1:	jsr	drop
	jsr	swap
	jsr	drop
	NEXT

; ABS ( n --- |n| )
; absolute value.
	defcode "abs", 0
abs:	jsr	check1
	jsr	dup
	jsr	zeroless
	lda	DSTACK-2,x
	beq	@abs1
	jsr	drop
	jmp	negate
@abs1:	jsr	drop
	NEXT

; AND ( n1 n2 --- (n1 and n2) )
; bitwise logical and.
	defcode "and", 0
bitand:	jsr	check2
	lda	DSTACK-4,x
	and	DSTACK-2,x
	sta	DSTACK-4,x
	lda	DSTACK-3,x
	and	DSTACK-1,x
	sta	DSTACK-3,x
	jsr	drop
	NEXT

; OR ( n1 n2 --- (n1 or n2) )
; bitwise logical or.
	defcode "or", 0
bitor:	jsr	check2
	lda	DSTACK-4,x
	ora	DSTACK-2,x
	sta	DSTACK-4,x
	lda	DSTACK-3,x
	ora	DSTACK-1,x
	sta	DSTACK-3,x
	jsr	drop
	NEXT

; XOR ( n1 n2 --- (n1 xor n2) )
; bitwise logical xor.
	defcode "xor", 0
bitxor:	jsr	check2
	lda	DSTACK-4,x
	eor	DSTACK-2,x
	sta	DSTACK-4,x
	lda	DSTACK-3,x
	eor	DSTACK-1,x
	sta	DSTACK-3,x
	jsr	drop
	NEXT

;
; *** DOUBLE AND MIXED LENGTH ARITHMETIC ***
;

; D+ ( d1 d2 --- d1+d2 )
; add

; stack: item1 low, item1 high, item2 low, item2 high
;        8,7        6,5         4,3        2,1

	defcode "d+", 0
dplus:	jsr	check4
	clc
	lda	DSTACK-8,x	; add low int
	adc	DSTACK-4,x
	sta	DSTACK-8,x
	lda	DSTACK-7,x
	adc	DSTACK-3,x
	sta	DSTACK-7,x

	lda	DSTACK-6,x	; add high int
	adc	DSTACK-2,x
	sta	DSTACK-6,x
	lda	DSTACK-5,x
	adc	DSTACK-1,x
	sta	DSTACK-5,x

	jsr	twodrop		; pop two items

	NEXT

; D- ( d1 d2 --- d1-d2 )
; subtract
	defcode "d-", 0
dminus:	jsr	check4
	sec
	lda	DSTACK-8,x	; sub low int
	sbc	DSTACK-4,x
	sta	DSTACK-8,x
	lda	DSTACK-7,x
	sbc	DSTACK-3,x
	sta	DSTACK-7,x

	lda	DSTACK-6,x	; sub high int
	sbc	DSTACK-2,x
	sta	DSTACK-6,x
	lda	DSTACK-5,x
	sbc	DSTACK-1,x
	sta	DSTACK-5,x

	jsr	twodrop		; pop two items

	NEXT

; D0= ( d1 --- flag )
; true if top double number is equal to zero
	defcode "d0=", 0
dzeroequal:
	jsr	check2
	jsr	zero
	jsr	zero
	jsr	dequal
	NEXT

; D= ( d1 d2 --- flag )
; true if two top double numbers are equal.
	defcode "d=", 0
dequal:	jsr	check4
	lda	DSTACK-8,x
	cmp	DSTACK-4,x
	bne	@equal0
	lda	DSTACK-7,x
	cmp	DSTACK-3,x
	bne	@equal0
	lda	DSTACK-6,x
	cmp	DSTACK-2,x
	bne	@equal0
	lda	DSTACK-5,x
	cmp	DSTACK-1,x
	bne	@equal0
	jsr	pushtrue
	NEXT
@equal0:
	jsr	pushfalse
	NEXT

; D2* ( d --- d*2 )
; multiply double number by two
	defcode "d2*", 0
dtwomul:	
	jsr	check2
	asl	DSTACK-4,x
	rol	DSTACK-3,x
	rol	DSTACK-2,x
	rol	DSTACK-1,x
	NEXT

; D2/ ( d --- d/2 )
; divide double number by two
	defcode "d2/", 0
dtwodiv:	
	jsr	check2
	lsr	DSTACK-1,x
	ror	DSTACK-2,x
	ror	DSTACK-3,x
	ror	DSTACK-4,x
	NEXT

; DABS ( d --- |d| )
; absolute value of double number.
	defcode "dabs", 0
dabs:	jsr	check2
	jsr	dup
	jsr	zeroless
	lda	DSTACK-2,x
	beq	@abs1
	jsr	drop
	jsr	dnegate
	NEXT
@abs1:	jsr	drop
	NEXT

; DMIN ( d1 d2 --- min )
; leave lesser of two double numbers.
	defcode "dmin", 0
dmin:	jsr	check4
	jsr	twoover		; d1 d2 d1
	jsr	twoover		; d1 d2 d1 d2
	jsr	dless		; d1 d2 flag
	lda	DSTACK-2,x
	beq	@min1
	jsr	drop
	jsr	twodrop
	NEXT
@min1:	jsr	drop
	jsr	twoswap
	jsr	twodrop
	NEXT

; DMAX ( d1 d2 --- max )
; leave greater of two items.
	defcode "dmax", 0
dmax:	jsr	check4
	jsr	twoover		; d1 d2 d1
	jsr	twoover		; d1 d2 d1 d2
	jsr	dless		; d1 d2 flag
	lda	DSTACK-2,x
	bne	@max1
	jsr	drop
	jsr	twodrop
	NEXT
@max1:	jsr	drop
	jsr	twoswap
	jsr	twodrop
	NEXT

; DNEGATE (d --- d )
; negate
	defcode "dnegate", 0
dnegate:
	jsr	check2
	lda	#0
	sec
	sbc	DSTACK-4,x
	sta	DSTACK-4,x
	lda	#0
	sbc	DSTACK-3,x
	sta	DSTACK-3,x
	lda	#0
	sbc	DSTACK-2,x
	sta	DSTACK-2,x
	lda	#0
	sbc	DSTACK-1,x
	sta	DSTACK-1,x
	NEXT
		
; D< ( d1 d2 --- flag )
; d1 < d2
	defcode "d<", 0
dless:	jsr	check4
	lda	DSTACK-8,x
	cmp	DSTACK-4,x
	lda	DSTACK-7,x
	sbc	DSTACK-3,x
	lda	DSTACK-6,x
	sbc	DSTACK-2,x
	lda	DSTACK-5,x
	sbc	DSTACK-1,x
	bvc	@less1
	eor	#$80
@less1:	bmi	@less2
	jsr	twodrop
	jsr	pushfalse
	NEXT
@less2:	jsr	twodrop
	jsr	pushtrue
	NEXT


; UM* (u1 u2 --- ud ) (n1*n2 32 bit)
; uint 16 bit * uint 16 bit
	defcode "um*", 0
umul:	jsr	check2
	jsr	mul16
	lda	AUX
	sta	DSTACK-4,x
	lda	AUX+1
	sta	DSTACK-3,x
	lda	AUX+2
	sta	DSTACK-2,x
	lda	AUX+3
	sta	DSTACK-1,x
	NEXT

; M* ( n1 n2 --- n1*n2 (32 bit)
; signed 16 bit int * signed 16 bit int = signed 32 bit int
	defcode "m*", 0
mmul:	jsr	check2
	jsr	mulsign
	jsr	mul16
	lda	AUX
	sta	DSTACK-4,x
	lda	AUX+1
	sta	DSTACK-3,x
	lda	AUX+2
	sta	DSTACK-2,x
	lda	AUX+3
	sta	DSTACK-1,x
	lda	SIGN
	bpl	@mmul2
	jsr	dnegate
@mmul2:
	NEXT


; 16 bit uint * 16 bit uint = 32 bit uint
mul16: 	lda	#$00
	sta	AUX+2		; clear upper bits of product
	sta	AUX+3 
	ldy	#$10		; set binary count to 16 
@shift_r:
	lsr	DSTACK-3,x	; divide multiplier by 2 
	ror	DSTACK-4,x
	bcc	@rotate_r 
	lda	AUX+2		; get upper half of product and add multiplicand
	clc
	adc	DSTACK-2,x
	sta	AUX+2
	lda	AUX+3 
	adc	DSTACK-1,x
@rotate_r:
	ror			; rotate partial product 
	sta	AUX+3 
	ror	AUX+2
	ror	AUX+1 
	ror	AUX 
	dey
	bne	@shift_r 
	rts


; UM/MOD ( ud u1 --- u2 u3 )
; ud = 32-bit uint dividend   -6 -5 -4 -3
; u1 = 16-bit uint divisor    -2,-1
; u2 = 16-bit uint remainder
; u3 = 16-bit uint quotient
;
; See http://6502.org/source/integers/ummodfix/ummodfix.htm

	defcode "um/mod", 0
ummod:
	jsr	check3
	sec			; detech overflow or /0 condition
	lda	DSTACK-4,x	; divisor must be more than high cell of dividend
	sbc	DSTACK-2,x
	lda	DSTACK-3,x
	sbc	DSTACK-1,x
	bcs	@oflow
	ldy	#17		; loop counter = 17
@loop:
	rol	DSTACK-6,x	; rotate low cell of dividend
	rol	DSTACK-5,x
	dey
	beq	@end
	rol	DSTACK-4,x	; rotate high cell of dividend
	rol	DSTACK-3,x
	lda	#0
	sta	AUX		; clear carry, AUX = carry
	rol	AUX		; save carry
	sec			; check if divisor will fit into high 17 bits of dividend
	lda	DSTACK-4,x
	sbc	DSTACK-2,x
	sta	TMP1		; save difference low byte temporarily
	lda	DSTACK-3,x
	sbc	DSTACK-1,x
	sta	TMP2		; save difference high byte temporarily
	lda	AUX		; load carry (17th bit)
	sbc	#0		; complete subtraction
	bcc	@loop
	lda	TMP1		; divisor fits so update high cell of dividend
	sta	DSTACK-4,x
	lda	TMP2
	sta	DSTACK-3,x
	jmp	@loop
@oflow:
	lda	#$ff
	sta	DSTACK-6,x
	sta	DSTACK-5,x
	sta	DSTACK-4,x
	sta	DSTACK-3,x
@end:	dex
	dex
	jsr	swap
	NEXT

; m/ (d1 n1 --- n2 )
; 32 bit int / 16 bit int
	defcode "m/", 0
mdiv:
	jsr	mdivmod
	jsr	swap
	jsr	drop
	NEXT
	

; m/mod (d1 n1 --- n2 n3
; d1 = 32 bit int
; n1 = 16 bit int
; n2 = remainder, 16 bit int
; n3 = quotient, 16 bit int
	defcode "m/mod", 0
mdivmod:
	jsr	check3
	lda	DSTACK-3,x	; determine sign of result
	eor	DSTACK-1,x
	sta	SIGN
	lda	DSTACK-1,x	; negate n1 if its negative
	bpl	@mdiv1
	jsr	negate
@mdiv1:
	lda	DSTACK-3,x	; negate d1 if its negative
	bpl	@mdiv2
	jsr	rot		; d1h n1 d1l
	jsr	rot		; n1 d1l d1h
	jsr	dnegate
	jsr	rot
@mdiv2:
	jsr	ummod		; remainder quotient
	lda	SIGN
	bpl	@mdiv3
	jsr	negate
	jsr	swap
	jsr	negate
	jsr	swap
@mdiv3:	NEXT


; */	( n1 n2 n3 --- n )
; n = n1 * n2 / n3, intermediate n1*n2 is 32-bit value
	defcode "*/", 0
muldiv:
	jsr	check3
	jsr	rot		; n2 n3 n1
	jsr	rot		; n3 n1 n2
	jsr	mmul		; n3 n1*n2
	jsr	rot		; n1*n2 n3
	jsr	mdiv
	NEXT

; */mod ( n1 n2 n3 --- n4 n5
; intermediate n1*n2 is 32-bit value
; n4 = remainder
; n5 = quotient
	defcode "*/mod", 0
muldivmod:
	jsr	rot		; n2 n3 n1
	jsr	rot		; n3 n1 n2
	jsr	mmul		; n3 n1*n2
	jsr	rot		; n1*n2 n3
	jsr	mdivmod
	NEXT

; ( ud1 n1 --- n2 ud2 )
; 32 bit divmod
; ud1 = dividend (unsigned 32 bit int)
; n1 = divisor (BASE)
; n2 = remainder (16 bit)
; ud2 = quotient (unsigned 32 bit int)
;
; This routine is used by number conversion routines
	defcode "/ummod32", 0
ummod32:
	jsr	check3
@div0:
	lda 	#0		; set remainder to 0
	sta	TMP1
	sta 	TMP2
	ldy	#$20		; number of bits
@div1:
	asl	DSTACK-6,x
	rol	DSTACK-5,x
	rol	DSTACK-4,x
	rol	DSTACK-3,x
	rol	TMP1
	rol	TMP2
	lda	#0		; set carry
	sta	TMP3
	rol	TMP3
	lda	TMP1		; subtract divisor to see if it fits in
	sec
	sbc	DSTACK-2,x
	sta	AUX
	lda	TMP2
	sbc	DSTACK-1,x
	sta	AUX+1
	lda	TMP3		; carry
	sbc	#0
	bcc	@div2		; if carry=0 then divisor did not fit in yet
	lda	AUX		; else save sub result as remainder
	sta	TMP1
	lda	AUX+1
	sta	TMP2
	inc	DSTACK-6,x
@div2:
	dey
	bne	@div1	
@div3:
	lda	TMP1		; save remainder
	sta	DSTACK-2,x
	lda	TMP2
	sta	DSTACK-1,x	; ud2l ud2h n2
	jsr	rot		; ud2h n2 ud2l
	jsr	rot		; n2 ud2l ud2h
	NEXT
	
;
; *** COMPARISON ***
;

; = ( n1 n2 --- flag )
; true if two top items are equal.
	defcode "=", 0
equal:	jsr	check2
	lda	DSTACK-4,x
	cmp	DSTACK-2,x
	bne	@equal0
	lda	DSTACK-3,x
	cmp	DSTACK-1,x
	bne	@equal0
	jsr	pushtrue
	NEXT
@equal0:
	jsr	pushfalse
	NEXT

; (n1 n2 --- true )
; this is not forth word
; drop two top items and pushes true to the stack.	
pushtrue:
	jsr	drop
	ldy	#1
	sty	DSTACK-2,x
	dey
	sty	DSTACK-1,x
	NEXT

; (n1 n2 --- false )
; this is not forth word
; drop two top items and pushes false to the stack.
pushfalse:
	jsr	drop
	ldy	#0
	sty	DSTACK-2,x
	sty	DSTACK-1,x
	NEXT

; < ( n1 n2 --- flag )
; true if n1<n2.
	defcode "<", 0
	jsr	check2
less:	lda	DSTACK-4,x
	cmp	DSTACK-2,x
	lda	DSTACK-3,x
	sbc	DSTACK-1,x
	bvc	@less1
	eor	#$80
@less1:	bmi	@less2
	jsr	pushfalse
	NEXT
@less2:	jsr	pushtrue
	NEXT

; > ( n1 n2 --- flag )
; true if n1>n2. n2<n1
	defcode ">", 0
greater:
	jsr	check2
	lda	DSTACK-2,x
	cmp	DSTACK-4,x
	lda	DSTACK-1,x
	sbc	DSTACK-3,x
	bvc	@greater1
	eor	#$80
@greater1:	
	bmi	@greater2
	jsr	pushfalse
	NEXT
@greater2:
	jsr	pushtrue
	NEXT
	
; 0= ( n --- flag )
; true if n = 0
	defcode "0=", 0
zeroequal:
	jsr	check1
	jsr	zero
	jsr	equal
	NEXT

; 0< ( n --- flag )
; true if n < 0
	defcode "0<", 0
zeroless:
	jsr	check1
	jsr	zero
	jsr	less
	NEXT

; 0> ( n --- flag )
; true if n > 0
	defcode "0>", 0
zerogreater:
	jsr	check1
	jsr	zero
	jsr	greater
	NEXT

; not ( flag - ~flag )
; reverse truth value.
	defcode "not", 0
not:	jsr	zeroequal
	NEXT

;
; *** BRANCHES ***
;

; BRANCH ( --- )
; branch always.
; return address is removed from the return stack.
	defcode "branch", 0
branch:
	txa			; save data stack ptr
	tay
	tsx			; get (return address)-1
	lda	$101,x
	sta	TMP1
	lda	$102,x
	sta	TMP2
	inx			; remove return address form return stack.
	inx
	txs
	tya			; restore data stack ptr
	tax
	ldy	#1		; get branch address
	lda	(TMP1),y
	sta	TMP3
	iny
	lda	(TMP1),y
	sta	TMP4
	jmp	(TMP3)		; perform branch

; 0BRANCH ( n --- )
; branch if top item is zero
	defcode "0branch", 0
zbranch:
	jsr	check1
	lda	DSTACK-2,x
	ora	DSTACK-1,x
	php
	dex			; remove item from the data stack
	dex
	plp
	beq	branch		; perform branch if n = 0
	txa			; save data stack ptr
	tay
	tsx			; return address = return address + 2
	clc
	lda	$101,x
	adc	#2
	sta	$101,x
	lda	$102,x
	adc	#0
	sta	$102,x
	tya			; restore data stack ptr
	tax
	NEXT

; IF ( n --- ) execute the following statement if n!=0
; (during the execution time)
; compile if statement
	defcode "if", FLAG_I
if:	push	$20		; jsr
	jsr	ccomma
	push	zbranch
	jsr	comma
	jsr	here		; save location of branch operand to stack
	jsr	fetch
	push	0		; store dummy address
	jsr	comma
	NEXT

; THEN ( --- )
; compile then statement
	defcode "then", FLAG_I
then:
	jsr	here		; get current address
	jsr	fetch
	jsr	swap		; save it as a branch address
	jsr	store
	NEXT

; ELSE ( --- )
; compile else statement
	defcode "else", FLAG_I
else:
	push	$20		; jsr
	jsr	ccomma
	push	branch		; branch over the else part
	jsr	comma
	jsr	here		; save location of branch (over the else) operand to stack
	jsr	fetch
	push	0		; store dummy address
	jsr	comma
	jsr	swap		; fill the if's branch operand
	jmp	then	

; DO ( limit index --- )
; compile do statement
	defcode "do", FLAG_I
	; compile time
do:
	push    $20             ; store "jsr"
        jsr     ccomma
        push    @do1            ; store address of @do1
        jsr     comma
	jsr	here		; get branch address to data stack
	jsr	fetch
        NEXT
	; runtime
@do1:
	jsr	check2
	pla			; save return address
	sta	TMP1
	pla
	sta	TMP2
	lda	DSTACK-3,x	; move two items from data stack to return stack
	pha
	lda	DSTACK-4,x
	pha
	lda	DSTACK-1,x
	pha
	lda	DSTACK-2,x
	pha
	dex			; remove two items from data stack
	dex
	dex
	dex
	lda	TMP2		; restore return address
	pha
	lda	TMP1
	pha
	NEXT

; LOOP ( --- )
; compile loop statement
	defcode "loop", FLAG_I
loop:
	; compile time
	push	$20		; store "jsr"
	jsr	ccomma
	push	loop1		; store address of @loop1
	jsr	comma
	push	$20		; store "jsr"
	jsr	ccomma
	push	zbranch		; store address of branch
	jsr	comma
	jsr	comma		; store branch address
	NEXT
	; runtime
loop1:
	stx	XSAVE
	tsx
	inc	$103,x		; index = index + 1
	bne	loop2
	inc	$104,x
loop2:
	clc			; limit - index
	lda	$105,x
	sbc	$103,x
	lda	$106,x
	sbc	$104,x
	ldx	XSAVE
	asl			; check if <= 0
	bcs	loop3
	push	0
	rts			; zbranch
loop3:	
	pla			; save return address
	sta	TMP1
	pla
	sta	TMP2
	pla			; remove two items from the return stack
	pla
	pla
	pla
	lda	TMP2		; restore return address
	pha
	lda	TMP1
	pha
	push	1
	rts			; zbranch will not branch

; +LOOP ( n --- )
; compile +loop statement
	defcode "+loop", FLAG_I
ploop:
	; compile time
	push	$20		; store "jsr"
	jsr	ccomma
	push	@ploop1		; store address of @ploop1
	jsr	comma
	push	$20		; store "jsr"
	jsr	ccomma
	push	zbranch		; store address of branch
	jsr	comma
	jsr	comma		; store branch address
	NEXT
	; runtime
@ploop1:
	jsr	check1
	lda	DSTACK-2,x	; store n
	sta	TMP1
	lda	DSTACK-1,x
	sta	TMP2
	dex			; remove one item from the data stack
	dex
	stx	XSAVE
	tsx
	clc			; index = index + n
	lda	TMP1
	adc	$103,x
	sta	$103,x
	lda	TMP2
	adc	$104,x
	sta	$104,x
	jmp	loop2

; LEAVE ( --- )
; compile leave statement
	defcode "leave", FLAG_I
leave:
	; compile time
	push	$20		; store jsr
	jsr	ccomma
	push	@leave1		; store address of leave1
	jsr	comma
	NEXT
	; runtime
@leave1:
	stx	XSAVE
	tsx
	lda	$105,x		; index = limit
	sta	$103,x
	lda	$106,x
	sta	$104,x
	ldx	XSAVE
	rts

; I ( --- n )
; compile i statement
	defcode "i", FLAG_I
i_statement:
	; compile
	push	$20		; store jsr
	jsr	ccomma
	push	@i1		; store address
	jsr	comma
	NEXT
	; runtime
@i1:	stx	XSAVE
	tsx
	lda	$103,x		; get index
	sta	TMP1
	lda	$104,x
	sta	TMP2
	ldx	XSAVE
	lda	TMP1		; save index to data stack
	sta	DSTACK,x
	lda	TMP2
	sta	DSTACK+1,x
	inx
	inx
	jsr	checkoflow
	rts

; J ( --- n )
; compile j statement
	defcode "j", FLAG_I
j_statement:
	; compile
	push	$20		; store jsr
	jsr	ccomma
	push	@j1		; store address
	jsr	comma
	NEXT
	; runtime
@j1:	stx	XSAVE
	tsx			; stack: addr($101), index1($103), limit1($105), index2($107), limit2($109)
	lda	$107,x		; get index2
	sta	TMP1
	lda	$108,x
	sta	TMP2
	ldx	XSAVE
	lda	TMP1		; save index to data stack
	sta	DSTACK,x
	lda	TMP2
	sta	DSTACK+1,x
	inx
	inx
	jsr	checkoflow
	rts

; BEGIN ( --- )
; compile begin statement
	defcode "begin", FLAG_I
begin:
	jsr	here		; get current address to stack
	jsr	fetch
	NEXT

; UNTIL ( n --- )
; compile until statement
	defcode "until", FLAG_I
until:
	push	$20		; jsr
	jsr	ccomma
	push	zbranch
	jsr	comma
	jsr	comma		; store branch address
	NEXT

; WHILE ( n --- )
; compile while statement
	defcode "while", FLAG_I
while:
	push	$20		; jsr
	jsr	ccomma
	push	zbranch
	jsr	comma
	jsr	here		; get current address to stack
	jsr	fetch
	push	0		; compile dummy address
	jsr	comma
	NEXT

; REPEAT ( --- )
; compile repeat statement
	defcode "repeat", FLAG_I
repeat:
	push	$20		;jsr
	jsr	ccomma
	push	branch
	jsr	comma
	jsr	swap		; get the original address (from begin)
	jsr	comma		; compile it after the branch
	jsr	here		; get the current address
	jsr	fetch
	jsr	swap		; stack: here, address of zbranch operand in while
	jsr	store
	NEXT

;
; *** UTIL ***
;

; .S ( --- )
; print the content of the stack
	defcode ".s", 0
prstack:
	jsr	trace
	NEXT

; WORDS ( --- )
; print words in the dictionary
	defcode "words", 0
words:
	jsr	last
@words1:
	jsr	fetch
	lda	DSTACK-2,x
	ora	DSTACK-1,x
	beq	@words2
	jsr	dup
	push	2
	jsr	plus
	jsr	count
	lda	DSTACK-2,x
	and	#FLAG_M
	sta	DSTACK-2,x
	jsr	type
	jsr	printspc
	jmp	@words1
@words2:
	dex
	dex
	NEXT

; DUMP ( addr n --- )
; dump memory starting from addr
	defcode "dump", 0
dump:
	jsr	check2
	jsr	over		; addr n addr
	jsr	plus		; addr addr+n
	jsr	swap		; addr+n addr
@dump1:
	jsr	printcr
	sec
	lda	DSTACK-4,x
	sbc	DSTACK-2,x
	lda	DSTACK-3,x
	sbc	DSTACK-1,x
	bcc	@dump3
	lda	#7
	sta	DUMPTMP
	jsr	dup		; addr+n addr addr
	jsr	udot		; addr+n addr
@dump2:
	jsr	dup		; addr+n addr addr
	jsr	cfetch		; addr+n addr n
	jsr	udot		; addr+n addr
	jsr	oneplus		; addr+n addr+1
	dec	DUMPTMP
	bpl	@dump2
	bmi	@dump1
@dump3:
	dex
	dex
	dex
	dex
	NEXT

;
; *** SYSTEM ***
;

; SYS ( addr --- )
; call machine code routine
	defcode "sys", 0
sys:
	jsr	check1
	stx	XSAVE		; save data stack pointer
	lda	#>(@sys4-1)	; push return address
	pha
	lda	#<(@sys4-1)
	pha
	sec			; push sys address
	lda	DSTACK-2,x
	sbc	#1
	tay
	lda	DSTACK-1,x
	sbc	#0
	pha
	tya
	pha
	lda	SR		; set sr
	pha
	plp
	lda	ACC		; set a
	ldx	XR		; set x
	ldy	YR		; set y
	rts			; SYS!!!
@sys4:	sta	ACC		; save a, status, x, y
	php
	pla
	sta	SR
	stx	XR
	sty	YR
	ldx	XSAVE		; restore data stack pointer
	dex			; remove address from the data stack
	dex
	NEXT
	
;
; *** COMPILER ***
;

; HIDE ( addr --- )
; hide the word in the dict
	defcode "hide", 0
hide:
	jsr	check1
	lda	DSTACK-2,x
	sta	TMP1
	lda	DSTACK-1,x
	sta	TMP2
	ldy	#2
	lda	(TMP1),y
	ora	#FLAG_H
	sta	(TMP1),y
	dex
	dex
	NEXT

; IMMEDIATE ( --- )
; marks the most recently created dictionary entry as an immediate word
	defcode "immediate", 0
immediate:
	lda	LASTPTR		; save address of the head of list
	sta	TMP1
	lda	LASTPTR+1
	sta	TMP2
	ldy	#2
	lda	(TMP1),y
	ora	#FLAG_I
	sta	(TMP1),y
	NEXT

; UNHIDE ( addr --- )
; unhide the word in the dict
	defcode "unhide", 0
unhide:	jsr	check1
	lda	DSTACK-2,x
	sta	TMP1
	lda	DSTACK-1,x
	sta	TMP2
	ldy	#2
	lda	#FLAG_H ^ $ff
	and	(TMP1),y
	sta	(TMP1),y
	dex
	dex
	NEXT

; ] ( --- )
; set compilation state on
	defcode "]", 0
rightbr:
	lda	#1
	sta	STATE
	NEXT

; [ ( --- )
; set compilation state off
	defcode "[", FLAG_I
leftbr:
	lda	#0
	sta	STATE
	NEXT

; CREATE (interpreter mode):
; create a dictionary entry
;
; CREATE (runtime):
; copies the address to the stack

; CREATE ( --- )
; create a new word
; expects a name in input stream
	defcode "create", 0
create:
	lda	#$80		; set flag (during the runtime, ptr to data is stored to stack)
	sta	CREATE
	bmi	create2
create1:			; entry point when called from column word
	lda	#0		; set flag to 0 (during the runtime, no ptr is stored to stack)
	sta	CREATE
create2:
	lda	#space		; get name of new dict entry
	sta	DSTACK,x
	lda	#0
	sta	DSTACK+1,x
	inx
	inx
	jsr	word
	lda	DSTACK-2,x	; string address
	sta	TMP1
	lda	DSTACK-1,x
	sta	TMP2
	dex			; delete addr of string from stack
	dex
	ldy	#0		; copy string length
	lda	(TMP1),y
	beq	@error1		; branch if no string
	sta	AUX
	inc	AUX		; length = length + length byte
	lda	HEREPTR		; destination address
	sta	TMP3
	lda	HEREPTR+1
	sta	TMP4
	lda	LASTPTR		; create a link to the previous word
	sta	(TMP3),y
	lda	LASTPTR+1
	iny
	sta	(TMP3),y
	lda	HEREPTR		; update LASTPTR
	sta	LASTPTR
	lda	HEREPTR+1
	sta	LASTPTR+1
	clc			; update desination address
	lda	TMP3		; dst = old last ptr + 2
	adc	#2
	sta	TMP3
	lda	TMP4
	adc	#0
	sta	TMP4
	ldy	#0
@create3:
	lda	(TMP1),y	; copy length of name and name
	sta	(TMP3),y
	iny
	cpy	AUX
	bne	@create3
	clc			; update HERE
	lda	TMP3		; HERE = old last ptr + 2 + length + length byte
	adc	AUX
	sta	HEREPTR
	lda	TMP4
	adc	#0
	sta	HEREPTR+1
	lda	CREATE		; check flag
	bpl	@create4	; branch if no ptr saving needed during the runtime
	jsr	here		; address = HERE + 6:
	jsr	fetch		; jsr literal = 3 bytes
	push	6		; address = 2 bytes, rts = 1 byte
	jsr	plus
	jsr	literal		; save literal (address)
	push	$60		; rts
	jsr	ccomma
@create4:
	NEXT
@error1:
	jsr	primm
	.byte	"no string",eol,0
	jmp	abort

; : xxx <BUILDS aaa DOES> bbb ; (compile time):
; execute : = create1 a dictionary entry for xxx
; execute <BUILDS = compile jsr builds1
; compile aaa
; execute DOES> = compile push address (bbb) to stack, compile jmp does1
; compile bbb
; compile rts
;
; xxx yyy (runtime):
; executes builds1 = create a dict entry for yyy, compile push instance (yyy) addr to stack,
;                    compile jsr branch, copy address to "BUILDS", compile dummy addr
; executes aaa
; push address (bbb) to stack
; executes does1 = copy address from "BUILDS" to branch operand location
;
; yyy (runtime):
; copies instance (yyy) address to the stack
; branch to bbb
; executes bbb
; rts
;
; In memory:
; dict entry for xxx, jsr builds1, aaa, push bbb, jmp does1, bbb, rts
; dict entry for yyy, push yyy, jsr branch, address of bbb 

; <BUILDS ( --- )
	defcode "<builds", FLAG_I
	; compile time
builds:
	push	$20		; jsr builds1
	jsr	ccomma
	push	@builds1
	jsr	comma
	NEXT
	; runtime
	; jsr builds1
@builds1:
	jsr	create1		; create a dict entry
	jsr	here		; address of branch operand = HERE + 10:
	jsr	fetch		; jsr lit = 3 bytes, address = 2 bytes
	push	10		; jsr branch = 3 bytes, branch address = 2 bytes
	jsr	plus
	jsr	literal		; save literal (address)
	push    $20             ; jsr branch
        jsr     ccomma
        push    branch
        jsr     comma
        jsr     here            ; save location of branch operand to return stack
        jsr     fetch
	lda	DSTACK-2,x
	sta	BUILDS
	lda	DSTACK-1,x
	sta	BUILDS+1
	dex
	dex	
        push    $1234           ; store dummy address
        jsr     comma
	NEXT

; DOES> ( --- )
	defcode "does>", FLAG_I
	; compile time
does:
	jsr	here
	jsr	fetch
	push	8		; jsr lit = 3 bytes, here+8 = 2 bytes, jmp @does1 = 3 bytes
	jsr	plus
	jsr	literal
	push	$4c		; store jmp
	jsr	ccomma
	push	@does1
	jsr	comma		; store address (does1)
	NEXT
	; runtime
	; push here+8
	; jmp does1
@does1:
	lda	BUILDS
	sta     DSTACK,x
        inx
        lda     BUILDS+1
        sta     DSTACK,x
        inx       
	jsr	store
	NEXT

; ALLOT ( n --- )
; allocate n bytes from the dictionary
	defcode "allot", 0
allot:	jsr	check1
	jsr	here
	jsr	plusstore
	NEXT

; , ( n --- )
; stores n to the dictionary
	defcode ",", 0
comma:	jsr	check1
	jsr	here
	jsr	fetch
	jsr	store
	jsr	two
	jsr	allot
	NEXT

; C, ( n --- )
; store byte to the dictionary
	defcode "c,", 0
ccomma:	jsr	check1
	jsr	here
	jsr	fetch
	jsr	cstore
	jsr	one
	jsr	allot
	NEXT

; : ( --- )
; define a new word
; expects a name 
	defcode ":", 0
colon:
	jsr	rightbr		; enter into the compilation mode
	jsr	create1		; create word (dict entry)
	jsr	last		; hide the created word
	jsr	fetch
	jsr	hide
	NEXT

; ; ( --- )
; stop defining a new word
	defcode ";", FLAG_I
semicolon:
	push	$60		; store rts
	jsr	ccomma
	jsr	last
	jsr	fetch
	jsr	unhide
	jsr	leftbr
	NEXT

; [COMPILE] ( --- )
; enforces compilation of the next word
	defcode "[compile]", FLAG_I
brackcompile:
	push	$20		; store jsr
	jsr	ccomma
	jsr	tick		; store address
	jsr	comma
	NEXT

; FORGET ( --- )
; removes a word and the following words from the dictionary.
	defcode "forget", 0
forget:
	push	space		; delimiter
	jsr	word		; get the next word
	lda	DSTACK-2,x	; check if a word was found
	sta	TMP1
	lda	DSTACK-1,x
	sta	TMP2
	ldy	#0
	lda	(TMP1),y
	beq	@error1		; branch if string length = 0
	jsr	find0
	lda	DSTACK-2,x
	ora	DSTACK-1,x
	beq	@error2
	jsr	dup		; stack: addr addr
	push	dictstart	; stack: addr addr dicstart
	jsr	less		; stack: addr flag
	lda	DSTACK-2,x
	bne	@error3
	dex			; stack: addr
	dex
	jsr	dup		; stack: addr addr
	jsr	fetch		; stack: addr prev_word_addr
	jsr	last		; stack: addr prev_word_addr lastptr_addr
	jsr	store		; stack: addr
	jsr	here		; stack: addr hereptr_addr
	jsr	store		; stack: -
	NEXT
@error1:
	jsr	primm
	.byte	"no word",eol,0
	jmp	abort
@error2:
	jsr	primm
	.byte	"undefined word",eol,0
	jmp	abort
@error3:
	jsr	primm
	.byte	"protected word", eol, 0
	jmp	abort

	 
;
; *** INTERPRETER ***
;

; NUMBER ( addr --- n/d )
; addr = address to string
; n/d = 16-bit or 32-bit number
	defcode "number", 0
number:	
	;jsr	trace
	ldy	#0
	lda	DSTACK-2,x	; save addr
	sta	TMP1
	lda	DSTACK-1,x
	sta	TMP2
	lda	(TMP1),y	; save length
	sta	TMP3
	inc	TMP3
	sty	DSTACK-2,x	; save 0 0
	sty	DSTACK-1,x
	sty	DSTACK,x
	sty	DSTACK+1,x
	inx
	inx
	iny
	lda	(TMP1),y	; check sign
	cmp	#'-'
	php
	bne	@number1
	iny
@number1:
	lda	(TMP1),y
	cmp	#'.'
	beq	@ddigit
	; * base
	jsr	@mulbase
	; + digit
	lda	(TMP1),y
	cmp	#'0'
	bcc	@error1
	cmp	#'g'
	bcs	@error1
	cmp	#':'
	bcc	@dec
	cmp	#'a'
	bcc	@error1
	sec
	sbc	#7		; 'a' - ':'
	pha
	lda	BASE		; check that we are in hex base
	cmp	#16
	bne	@error1
	pla
@dec:
	sec	
	sbc	#'0'
	sta	DSTACK,x
	inx
	lda	#0
	sta	DSTACK,x
	inx
	sta	DSTACK,x
	inx
	sta	DSTACK,x
	inx
	; d1 = d1 * base + digit
	jsr	dplus
	iny
	cpy	TMP3
	bne	@number1
	dex			; remove high 16 bits
	dex
	plp
	bne	@number2
	jsr	negate
@number2:
	NEXT
@ddigit:
	iny
	cpy	TMP3
	bne	@error1
	plp
	bne	@ddigit2
	jsr	dnegate
@ddigit2:
	NEXT
@error1:
	lda	LOAD
	beq	@skip1
	push	$200
	jsr	count
	jsr	type
	jsr	printspc
@skip1:
	jsr	primm
	.byte	"undefined word", eol, 0
	jmp	abort

@mulbase:
	lda	BASE
	cmp	#10
	beq	@mul10
	cmp	#16
	bne	@error2
@mul16:
	jsr	dtwomul		; d1*2
	jsr	dtwomul		; d1*4
	jsr	dtwomul		; d1*8
	jsr	dtwomul		; d1*16
	rts
@mul10:
	jsr	twodup		; d1 d1
	jsr	dtwomul		; d1 d1*2
	jsr	dtwomul		; d1 d1*4
	jsr	dtwomul		; d1 d1*8
	jsr	twoswap		; d1*8 d1
	jsr	dtwomul		; d1*8 d1*2
	jsr	dplus		; d1*10
	rts
@error2:
	jsr	primm
	.byte	"unsupported base", eol, 0
	jmp	abort

; QUERY
; read input from file or keyboard and fill input buffer
	defcode "query", 0
query:
	stx	XSAVE
	ldx	#0
	stx	CPTR
@query1:
	jsr	chrin
	cmp	#lfeed		; convert linefeed to cr
	bne	@query2
	lda	#eol
@query2:
	sta	buffer+1,x
	inx
	cpx	#$41		; check max length
	beq	@error1
	pha
	lda	LOAD		; skip i/o status checking if read from keyboard
	beq	@query4
	jsr	readst
	cmp	#0
	bne	@query3		; branch if eof or error
@query4:
	pla
	cmp	#eol
	bne	@query1		; branch if not end of line
@query5:
	dex
	stx	buffer
	ldx	XSAVE
	NEXT
@query3:
	cmp	#eof
	beq	@eof
	jsr	primm
        .byte   "disk i/o error query",eol,0
        jmp     abort      
@eof:	txa			; save x because clall modifies it
	pha
	jsr	clall
	jsr	primm
	.byte	eol, "eof", eol, 0
	pla
	tax
	lda	#0		; eof thus we can stop load
	sta	LOAD
	pla
	jmp	@query5
@error1:
	jsr	primm
	.byte	" ?", eol, 0
	lda	#eol
	sta	buffer+1
	ldx	XSAVE
	NEXT

; WORD ( delimiter --- string )
; get the next word from the input stream
; string = address of length byte, following string
	defcode "word", 0
word:	jsr	check1
	lda	DSTACK-2,x	; get delimiter
	sta	TMP1
	stx	XSAVE
	ldy	#0
@word1:				; ignore leading delimiters
	ldx	CPTR
	inc	CPTR
	lda	buffer+1,x
@delim1:
	cmp	TMP1		; branch if delimiter
	beq	@word1
	cmp	#eol		; branch if eol
	beq	@error2
	cmp	#'('		; check if comment
	bne	@word3		; branch if not comment
@comment1:
	ldx	CPTR		; ignore comment
	inc	CPTR
	lda	buffer+1,x
	cmp	#eol
	beq	@error2		; branch if eol
	cmp	#')'
	bne	@comment1	; branch if not end of comment
	beq	@word1		; ignore delimiters which follow the comment
@word2:	ldx	CPTR		; get next char
	inc	CPTR
	lda	buffer+1,x
@word3:	sta	buffer+1,y
	iny
	cmp	#eol		; branch if eol
	beq	@word4
@delim2:
	cmp	TMP1		; branch if delimiter
	beq	@word4
	bne	@word2
@error2:
	lda	#0
	sta	buffer
	beq	@word5
@word4:	dey
	sty	buffer
	dec	CPTR
@word5:	ldx	XSAVE
	lda	#<buffer
	sta	DSTACK-2,x
	lda	#>buffer
	sta	DSTACK-1,x
	NEXT

; FIND ( string --- addr )
; find the word from the dictionary
; string = address of length byte, following string
; addr = execution address of word (0 if not found)

	defcode "find", 0
find:	jsr	check1
	jsr	find0
	lda	DSTACK-2,x
	ora	DSTACK-1,x
	beq	@find1		; branch if the word was not found
	lda	DSTACK-2,x
	sta	TMP1
	lda	DSTACK-1,x
	sta	TMP2
	ldy	#2
	lda	(TMP1),y
	and	#$0f
	clc
	adc	#3
	adc	DSTACK-2,x
	sta	DSTACK-2,x
	lda	DSTACK-1,x
	adc	#0
	sta	DSTACK-1,x
@find1:
	NEXT

; find0 a word from the dictionary
; string = address of length byte, following string
; addr = address of word (0 if not found)

find0:
	lda	DSTACK-2,x	; save string address
	sta	TMP1
	lda	DSTACK-1,x
	sta	TMP2
	lda	LASTPTR		; save address of the head of list
	sta	TMP3
	lda	LASTPTR+1
	sta	TMP4
	ldy	#0
	lda	(TMP1),y
	beq	@find9		; branch if no string
@find1:	; compare length of word
	ldy	#2
	lda	(TMP3),y
	bmi	@find4		; branch if hidden (FLAG_H) is set
	pha			; save immediate mode
	and	#FLAG_I
	sta	IMM
	pla
	and	#$0f		; length
	sta	AUX
	sta	AUX+1
	ldy	#0
	cmp	(TMP1),y	; get length byte of string
	bne	@find4		; branch if no match
@find2:	; compare string
	ldy	AUX
	lda	(TMP1),y
	iny
	iny
	cmp	(TMP3),y
	bne	@find4		; branch if no match
	dec	AUX
	bne	@find2		; branch if not last char
	; match was found
	lda	TMP3
	sta	DSTACK-2,x
	lda	TMP4
	sta	DSTACK-1,x
	NEXT	
@find4:
	; get next word
	ldy	#0
	lda	(TMP3),y
	pha
	iny
	lda	(TMP3),y
	sta	TMP4
	pla
	sta	TMP3
	ora	TMP4
	beq	@find9		; branch if this was last word
	jmp	@find1	
@find9:
	sta	DSTACK-2,x
	sta	DSTACK-1,x
	NEXT
	
; ' ( --- addr )
; get the next word from the input stream
; returns execution address of word in the data stack.
;
; Note: this word cannot be used in the interpreter loop,
;       because of exception handling.
	defcode "'", 0
tick:	push	space		; delimiter
	jsr	word		; get the next word
	lda	DSTACK-2,x	; check if a word was found
	sta	TMP1
	lda	DSTACK-1,x
	sta	TMP2
	ldy	#0
	lda	(TMP1),y
	beq	@error1		; branch if string length = 0
	jsr	find
	lda	DSTACK-2,x
	ora	DSTACK-1,x
	beq	@error2
	jsr	checkoflow
	NEXT
@error1:
	jsr	primm
	.byte	"no word",eol,0
	jmp	abort
@error2:
	jsr	primm
	.byte	"undefined word",eol,0
	jmp	abort


; EXECUTE ( addr --- )
; execute the word in addr
	defcode "execute", 0
execute:
	jsr	check1
	lda	DSTACK-2,x
	sta	TMP1
	lda	DSTACK-1,x
	sta	TMP2
	dex
	dex
	ora	TMP1
	beq	@execute1
	jmp	(TMP1)
@execute1:
	NEXT

; INTERPRET
; - word
; - find
; -- if found
; --- execute word
; -- else convert to number
; --- push to stack

	defcode "interpret", 0
interpret:
	jsr	checkoflow
	lda	LOAD
	bne	@interpret0b
	lda	STATE
	bne	@interpret0a
	jsr	primm
	.byte	"ok ",0
@interpret0a:
	jsr	printcr
@interpret0b:
	jsr	query
@interpret1:
	lda	#$20		; space is delimiter
	sta	DSTACK,x
	inx
	lda	#$00
	sta	DSTACK,x
	inx
	jsr	word
	lda	DSTACK-2,x
	sta	TMP1
	lda	DSTACK-1,x
	sta	TMP2
	ldy	#0
	lda	(TMP1),y
	beq	@interpret3
	jsr	find
	lda	DSTACK-2,x
	ora	DSTACK-1,x
	bne	@interpret2
	lda	#<buffer
	sta	DSTACK-2,x
	lda	#>buffer
	sta	DSTACK-1,x
	jsr	number
	lda	STATE
	beq	@interpret1	; branch if in interpreter mode
	jsr	literal		; store literal (compiling)
	jmp	@interpret1
@interpret2:
	lda	IMM
	bne	@execute1	; branch if immediate mode of word
	lda	STATE
	beq	@execute2	; branch if in interpreter mode
	push	$20		; store jsr
	jsr	ccomma
@interpret2b:
	jsr	comma		; save address to dictionary
	jmp	@interpret1
@execute1:
	lda	STATE		; check that not in the interpreter mode
	beq	@error1
@execute2:
	jsr	execute
	jmp	@interpret1
@interpret3:
	dex
	dex
	;jsr	trace
	jmp	interpret
@error1:
	jsr	primm
	.byte	"compile only word",eol,0
	jmp	abort

; COLD
; cold start
	defcode "cold", 0
cold:
	ldy	#8
	sty	DEVICE
	lda	#0
	sta	DEVICE+1
	sta	ACC
	sta	XR
	sta	YR
	lda	#$20
	sta	SR
	lda	#<lastword	; initialize last word pointer	
	sta	LASTPTR
	lda	#>lastword
	sta	LASTPTR+1
	lda	#<dictstart	; initialize dict start (free area pointer)
	sta	HEREPTR	
	lda	#>dictstart
	sta	HEREPTR+1
	lda	#10		; set base to decimal
	sta	BASE
	sty	BASE+1
	jsr	primm
	.byte	eol,lowcase,"    **** SturmFORTH v0.67 ****",eol, eol
	.byte               "       Coded by Juha Ollila",eol,eol,0
	jmp	abort

; ABORT
	defcode "abort", 0
abort:	
	jsr	clall		; close files and set stdin and stdout
	ldx	#0		; initialize data stack ptr
	stx	LOAD		; load is not ongoing anymore
	jmp	quit

; QUIT
; initializes Forth interpreter
lastword:
	defcode "quit", 0
quit:	txa		; quit does not tamper data stack ptr
	tay
	; ldx	SPSAVE	; initialize return stack
	ldx	#$ff
	txs
	lda	#0	; initialize interpreter state and immediate mode flags
	sta	STATE
	sta	STATE+1
	sta	IMM
	tya
	tax
	jmp	interpret
	jmp	:-	; makes assembler happy

trace:
	stx	XSAVE
	jsr	primm
	.byte	eol, "stack(",0
	txa
	clc
	ror
	sta	DSTACK,x
	lda	#0
	sta	DSTACK+1,x
	inx
	inx
	jsr	udot
	jsr	primm
	.byte	"): ",0
	ldx	#0
@trace1:
	cpx	XSAVE
	beq	@trace4
	lda	DSTACK+1,x
	tay
	lda	DSTACK,x
	pha
	lda	BASE
	cmp	#10
	bne	@trace2
	pla
	jsr	printudec
	jmp	@trace3
@trace2:
	pla
	jsr	printuint
@trace3:
	jsr	printspc
	inx
	inx
	jmp	@trace1
@trace4:
	jsr	printcr
	ldx	XSAVE
	rts

; primm routine
; copied from c128 kernal but modified so that it does not tamper
; any zero page location
primm:
	; save registers
	pha
	txa
	pha
	tya
	pha

	lda	TMP1
	pha
	lda	TMP1+1
	pha

	; update return address
@primm1:
	ldy	#0
	tsx
	inc	$106,x
	bne	@primm2
	inc	$107,x
	; print character
@primm2:
	lda	$106,x
	sta	TMP1
	lda	$107,x
	sta	TMP1+1
	lda	(TMP1),y
	beq	@primm3		; exit if 0 byte
	jsr	chrout
	bcc	@primm1		; if no error get next character
@primm3:
	pla
	sta	TMP1+1
	pla
	sta	TMP1

	pla
	tay
	pla
	tax
	pla
	rts


; pet2byte routine converts two hexadecimal characters (petscii) to
; 8-bit byte value.
; input  a = first hex digit
;        y = second hex digit
; output a = 8-bit byte value
; changes all registers
;

pet2byte:
	; convert bits 4-7
	jsr	@cnvbyte
	asl
	asl
	asl
	asl
	pha
	; convert bits 0-3
	tya
	jsr	@cnvbyte
	; combine all 8 bits to one byte
	tsx
	ora	$101,x
	; clean the stack before returning
	tax
	pla
	txa
	rts

@cnvbyte:
	cmp	#'A'
	bcc	@cnvbyte1	; branch if 0-9
	sec
	sbc	#$37
	rts
@cnvbyte1:
	sec
	sbc	#'0'
	rts

; byte2pet routine converts 8-bit byte value to printable
; hexadecimal string (petscii).
;
; input	 a = 8-bit byte value
; output a = first hex digit
;        y = second hex digit
; changes all registers
;

byte2pet:
	pha
	and	#$0f
	tax
	lda	@hex,x
	tay
	pla
	ror
	ror
	ror
	ror
	and	#$0f
	tax
	lda	@hex,x
	rts

@hex:	.byte	"0123456789abcdef"

; printbyte routine prints 8-bit byte in hex format.
; input a = byte
; does not change registers
;

printbyte:
	; save registers
	pha
	txa
	pha
	tya
	pha
	; get byte back
	tsx
	lda	$103,x
	; print byte
	jsr	byte2pet
	jsr	chrout
	tya
	jsr	chrout
	; load registers
	pla
	tay
	pla
	tax
	pla
	rts

; printword routine prints 16-bit word (low endian) in hex format.
;
; input  a = low byte
;	 y = high byte
; does not change registers
printword:
	pha
	tya		; print high byte
	jsr	printbyte
	pla		; print low byte
	jsr	printbyte
	rts	

; printint routine prints signed 16-bit word (low endian) in hex format.
;
; input a = low byte
; 	y = high byte
; does not change registers
printint:
	pha
	sta	TMP1
	tya
	sta	TMP2
	lda	TMP2		; set flags
	bpl	printuint2
	lda	#0		; int = 0 - int
	sec
	sbc	TMP1
	sta	TMP1
	lda	#0
	sbc	TMP2
	sta	TMP2
	lda	#'-'		; print sign
	jsr	chrout
	jmp	printuint2

; printuint routine prints unsigned 16-bit word (low endian) in hex format.
; a = low byte
; y = high byte
; does not change registers
printuint:
	pha
	sta	TMP1
	tya
	sta	TMP2
printuint2:
	;lda	#'$'
	;jsr	chrout
	lda	TMP2
	beq	@printint1	; there are 1-2 digits
	cmp	#$10
	bcc	@printh3	; there are 3 digits
	jmp	@printh4	; thre are 4 digits
@printint1:
	lda	TMP1
	cmp	#$10
	bcc	@printh1	; there is 1 digit
	jmp	@printh2		; there are two digits
@printh4:
	lda	TMP2
	lsr
	lsr
	lsr
	lsr
	tay
	lda	@hex,y
	jsr	chrout
@printh3:
	lda	TMP2
	and	#$0f
	tay
	lda	@hex,y
	jsr	chrout
@printh2:
	lda	TMP1
	lsr
	lsr
	lsr
	lsr
	tay
	lda	@hex,y
	jsr	chrout
@printh1:
	lda	TMP1
	and	#$0f
	tay
	lda	@hex,y
	jsr	chrout
	pla
	rts
@hex:	.byte	"0123456789abcdef"
	
; prints 16-bit word in decimal (string)
;
; input	a = low byte
;	y = high byte
; does not change registers
printdec:
	pha
	sta	AUX
	tya
	sta	AUX+1
	pha
	txa
	pha	
	ldy	#0
	sty	TMP1
	sty	TMP2
	sty	TMP3
	;tsx
	lda	AUX+1
	bpl	printudec2
	lda	#0
	sec
	sbc	AUX
	sta	AUX
	lda	#0
	sbc	AUX+1
	sta	AUX+1
	lda	#'-'
	jsr	chrout
	jmp	printudec2
; prints unsigned 16-bit word in decimal (string)
; a = low byte
; y = high byte
printudec:
	pha
	sta	AUX
	tya
	sta	AUX+1
	pha
	txa
	pha	
	ldy	#0
	sty	TMP1
	sty	TMP2
	sty	TMP3
printudec2:
	sed
@bin0:	asl	AUX
	rol	AUX+1
	bcc	@bin2
@bin1:	clc
	lda	@dectable+2,y
	adc	TMP3
	sta	TMP3
	lda	@dectable+1,y
	adc	TMP2
	sta	TMP2
	lda	@dectable,y
	adc	TMP1
	sta	TMP1
@bin2:	iny
	iny
	iny
	cpy	#48
	beq	@bin3
	jmp	@bin0
@bin3:	cld
	lda	TMP1
	bne	@bin4a
	lda	TMP2
	and	#$f0
	bne	@bin4b
	lda	TMP2
	and	#$0f
	bne	@bin4c
	lda	TMP3
	and	#$f0
	bne	@bin4d
	beq	@bin4e	
@bin4a:
	jsr	@print2
@bin4b:	lda	TMP2
	jsr	@print1
@bin4c:	lda	TMP2
	jsr	@print2
@bin4d:	lda	TMP3
	jsr	@print1
@bin4e:	lda	TMP3
	jsr	@print2

	;ldx	XSAVE
	pla
	tax
	pla
	tay
	pla
	rts

@print1:
	lsr
	lsr
	lsr
	lsr
	clc
	adc	#$30
	jsr	chrout
	rts
@print2:
	and	#$0f
	clc
	adc	#$30
	jsr	chrout
	rts	

@dectable:
.byte	$03, $27, $68
.byte	$01, $63, $84
.byte	$00, $81, $92
.byte	$00, $40, $96
.byte	$00, $20, $48
.byte	$00, $10, $24
.byte	$00, $05, $12
.byte	$00, $02, $56
.byte	$00, $01, $28
.byte	$00, $00, $64
.byte	$00, $00, $32
.byte	$00, $00, $16
.byte	$00, $00, $08
.byte	$00, $00, $04
.byte	$00, $00, $02
.byte	$00, $00, $01

dictstart:

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


; tail for the linked list
:	.word	0
	.byte	0

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
; *** STACK MANIPULATION ***
;

; DROP ( n --- )
; remove the top entry from the stack.
	defcode	"drop", 0
drop:
	dex
	dex
	NEXT

; 2DROP ( n n --- )
; remove top two entries from the stack.
	defcode "2drop", 0
twodrop:
	dex
	dex
	dex
	dex
	NEXT

; DUP ( n --- n n )
; duplicate the top entry on the stack.
	defcode	"dup", 0
dup:
	lda	DSTACK-2,x
	sta	DSTACK,x
	lda	DSTACK-1,x
	sta	DSTACK+1,x
	inx
	inx
	NEXT

; OVER ( n1 n2 --- n1 n2 n1 )
; duplicate the second item on the stack.
	defcode "over", 0
over:
	lda	DSTACK-4,x
	sta	DSTACK,x
	lda	DSTACK-3,x
	sta	DSTACK+1,x
	inx
	inx
	NEXT

; ROT ( n1 n2 n3 --- n2 n3 n1 )
; rotate the third item to the top of the stack.
	defcode	"rot", 0
rot:	; save n1
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
swap:	; save n1
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
qdup:	lda	DSTACK-1,x
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
	NEXT

; PICK ( n1 --- n2 )
; copy n'th item to the top of the stack.          
; 0 PICK is equivalent to DUP   
; 1 PICK is equivalent to OVER
	defcode "pick", 0
pick:	stx	XSAVE
	; get n1 and ignore high byte
	lda	DSTACK-2,x
	tay
@pick1: ; x = x-2 for each count
	dey
	bmi	@pick2
	dex
	dex
	jmp	@pick1
@pick2:
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

; >R ( n --- )
; move the top entry of the stack to the return stack.
	defcode ">r", 0
tor:
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
	tsx
	; copy an item to the stack
	ldx	XSAVE
	lda	TMP1
	sta	DSTACK,x
	lda	TMP2
	sta	DSTACK+1,x
	; adjust stack pointer
	inx
	inx
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
	NEXT

; 0 ( --- 0 )
; push zero to the stack.
	defcode "0", 0
zero:	lda	#0
	sta	DSTACK,x
	inx
	sta	DSTACK,x
	inx
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
	NEXT

;
; *** LITERAL ***
;

; LITERAL ( n --- ) 
; compile literal
	defcode "literal", FLAG_I
literal:
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

; HEX ( --- )
; changes base to hex
	defcode "hex", 0
hex:
	lda	#16
	sta	BASE
	NEXT

; DECIMAL ( --- )
; changes base to decimal
	defcode	"decimal", 0
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
dot:
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
	jmp	printspc

; u. ( n --- )
; print unsigned value of n
	defcode "u.", 0
udot:
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
	jmp	printspc

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
spaces:
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
emit:
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
	inc	TMP3
	bne	@dotquote2
	inc	TMP4
@dotquote2:
	ldy	#1		; ignore leading space
	lda	(TMP3),y
	cmp	#quote		; branch to end if quote char
	beq	@dotquote3
	sta	DSTACK,x
	inx
	lda	#0		
	sta	DSTACK,x
	inx
	jsr	ccomma		; compile the byte
	jmp	@dotquote1
@dotquote3:
	inc	CPTR		; HACK: ignore the following quote in the input stream
	push	0		; compile zero terminator for the primm
	jsr	ccomma	
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
	inc	TMP3
	bne	@dotquote2
	inc	TMP4
@dotquote2:
	ldy	#1		; ignore leading space
	lda	(TMP3),y
	cmp	#')'		; branch to end if quote char
	beq	@dotquote3
	jsr	chrout
	jmp	@dotquote1
@dotquote3:
	inc	CPTR		; HACK: ignore the following char in the input stream
	NEXT
@error:
	jsr	primm
	.byte	"no string",eol,0
	jmp	abort
	
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
;	lda	#$80
;	jsr	setmsg
	jsr	open
	bcs	@error2
;	jsr	readst
;	cmp	#0
;	bne	@error2
	ldx	#1		; file number
	jsr	chkin
	bcs	@error2
;	jsr	readst
;	cmp	#0
;	bne	@error2

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

;
; *** MEMORY (peek, poke and copy) ***
;

; ! ( n addr --- )
; store int to the addr
	defcode "!", 0
store:	
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
fetch:
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
cstore:
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
cfetch:	
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
cmove:
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

initcmove:			; routine used by cmove and <cmove
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

;
; *** ARITHMETIC ***
;

; + ( n1 n2 --- n1+n2 )
; add
	defcode "+", 0
plus:
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
minus:
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
twomul:
	asl	DSTACK-2,x
	rol	DSTACK-1,x
	NEXT

; 2/ ( n --- n/2 )
; divide by two
	defcode "2/", 0
twodiv:
	lsr	DSTACK-1,x
	ror	DSTACK-2,x
	NEXT

; NEGATE ( n --- -n )
	defcode "negate", 0
negate:	lda	#0
	sec
	sbc	DSTACK-2,x
	sta	DSTACK-2,x
	lda	#0
	sbc	DSTACK-1,x
	sta	DSTACK-1,x
	NEXT


; * ( n1 n2 --- n1*n2 )
; multiply.
	defcode "*", 0
mul:
	lda	DSTACK-3,x	; check sign of result
	eor	DSTACK-1,x
	php			; save sign
	; negate n1 if needed
	lda	DSTACK-3,x
	bpl	@mula
	jsr	swap
	jsr	negate
	jsr	swap
	; negate n2 if needed
@mula:	lda	DSTACK-1,x
	bpl	@mul0
	jsr	negate
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
	plp			; adjust sign if needed
	bpl	@mul4
	jsr	negate
@mul4:
	dex
	dex
	NEXT

; /MOD ( n1 n2 --- (n1 mod n2) n1/n2 )
; divide.
	defcode "/mod", 0
divmod:	
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
	jmp	drop

; / ( n1 n2 --- n1/n2 )
	defcode "/", 0
divide:	jsr	divmod
	jsr	swap
	jmp	drop

; MIN ( n1 n2 --- min )
; leave lesser of two items.
	defcode "min", 0
min:	jsr	over
	jsr	over
	jsr	less
	lda	DSTACK-2,x
	beq	@min1
	jsr	drop
	jmp	drop
@min1:	jsr	drop
	jsr	swap
	jmp	drop

; MAX ( n1 n2 --- max )
; leave greater of two items.
	defcode "max", 0
max:	jsr	over
	jsr	over
	jsr	greater
	lda	DSTACK-2,x
	beq	@max1
	jsr	drop
	jmp	drop
@max1:	jsr	drop
	jsr	swap
	jmp	drop

; ABS ( n --- |n| )
; absolute value.
	defcode "abs", 0
abs:	jsr	dup
	jsr	zeroless
	lda	DSTACK-2,x
	beq	@abs1
	jsr	drop
	jmp	negate
@abs1:	jmp	drop

; AND ( n1 n2 --- (n1 and n2) )
; bitwise logical and.
	defcode "and", 0
bitand:	lda	DSTACK-4,x
	and	DSTACK-2,x
	sta	DSTACK-4,x
	lda	DSTACK-3,x
	and	DSTACK-1,x
	sta	DSTACK-3,x
	jmp	drop

; OR ( n1 n2 --- (n1 or n2) )
; bitwise logical or.
	defcode "or", 0
bitor:	lda	DSTACK-4,x
	ora	DSTACK-2,x
	sta	DSTACK-4,x
	lda	DSTACK-3,x
	ora	DSTACK-1,x
	sta	DSTACK-3,x
	jmp	drop

; XOR ( n1 n2 --- (n1 xor n2) )
; bitwise logical xor.
	defcode "xor", 0
bitxor:	lda	DSTACK-4,x
	eor	DSTACK-2,x
	sta	DSTACK-4,x
	lda	DSTACK-3,x
	eor	DSTACK-1,x
	sta	DSTACK-3,x
	jmp	drop

;
; *** DOUBLE LENGTH ***
;

; D+ ( d1 d2 --- d1+d2 )
; add

; stack: item1 low, item1 high, item2 low, item2 high
;        8,7        6,5         4,3        2,1

	defcode "d+", 0
dplus:
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

	dex			; pop two items
	dex
	dex
	dex

	NEXT
	
; D< ( d1 d2 --- flag )
; d1 < d2
	defcode "d<", 0
dless:
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
	jmp	pushfalse
@less2:	jsr	twodrop
	jmp	pushtrue
;
; *** COMPARISON ***
;

; = ( n1 n2 --- flag )
; true if two top items are equal.
	defcode "=", 0
equal:	lda	DSTACK-4,x
	cmp	DSTACK-2,x
	bne	@equal0
	lda	DSTACK-3,x
	cmp	DSTACK-1,x
	bne	@equal0
	jmp	pushtrue
@equal0:
	jmp	pushfalse

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
less:	lda	DSTACK-4,x
	cmp	DSTACK-2,x
	lda	DSTACK-3,x
	sbc	DSTACK-1,x
	bvc	@less1
	eor	#$80
@less1:	bmi	@less2
	jmp	pushfalse
@less2:	jmp	pushtrue

; > ( n1 n2 --- flag )
; true if n1>n2. n2<n1
	defcode ">", 0
greater:
	lda	DSTACK-2,x
	cmp	DSTACK-4,x
	lda	DSTACK-1,x
	sbc	DSTACK-3,x
	bvc	@greater1
	eor	#$80
@greater1:	
	bmi	@greater2
	jmp	pushfalse
@greater2:
	jmp	pushtrue
	
; 0= ( n --- flag )
; true if n = 0
	defcode "0=", 0
zeroequal:
	jsr	zero
	jmp	equal

; 0< ( n --- flag )
; true if n < 0
	defcode "0<", 0
zeroless:
	jsr	zero
	jmp	less

; 0> ( n --- flag )
; true if n > 0
	defcode "0>", 0
zerogreater:
	jsr	zero
	jmp	greater

; not ( flag - ~flag )
; reverse truth value.
	defcode "not", 0
not:	jmp	zeroequal

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
	tsx			; stack: addr($101), index1($103), limit1($105), addr($107) index2($109), limit2($10b)
	lda	$109,x		; get index2
	sta	TMP1
	lda	$10a,x
	sta	TMP2
	ldx	XSAVE
	lda	TMP1		; save index to data stack
	sta	DSTACK,x
	lda	TMP2
	sta	DSTACK+1,x
	inx
	inx
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
; *** COMPILER ***
;

; HIDE ( addr --- )
; hide the word in the dict
	defcode "hide", 0
hide:
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

; UNHIDE ( addr --- )
; unhide the word in the dict
	defcode "unhide", 0
unhide:	lda	DSTACK-2,x
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

; ALLOT ( n --- )
; allocate n bytes from the dictionary
	defcode "allot", 0
allot:	jsr	here
	jsr	plusstore
	NEXT

; , ( n --- )
; stores n to the dictionary
	defcode ",", 0
comma:	jsr	here
	jsr	fetch
	jsr	store
	jsr	two
	jsr	allot
	NEXT

; C, ( n --- )
; store byte to the dictionary
	defcode "c,", 0
ccomma:	jsr	here
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
	jsr	create1		; create word (dict entry)
	jsr	last		; hide the created word
	jsr	fetch
	jsr	hide
	jsr	rightbr		; enter into the compilation mode
	NEXT			; EXIT

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
	NEXT			; EXIT	
;
; *** INTERPRETER ***
;

; NUMBER ( addr --- n )
; addr = address to string
; n = number
	defcode "number", 0
number:	ldy	#0
	sty	TMP1
	sty	TMP2
	lda	buffer+1,y	; check sign
	cmp	#'-'
	php
	bne	@number1
	iny
@number1:
	; * base
	lda	TMP1
	sta	DSTACK,x
	inx
	lda	TMP2
	sta	DSTACK,x
	inx
	;lda	#10
	lda	BASE
	sta	DSTACK,x
	inx
	lda	#0
	sta	DSTACK,x
	inx
	jsr	mul
	; + digit
	lda	buffer+1,y
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
	jsr	plus
	; TMP = TMP * 10 + digit
	lda	DSTACK-2,x
	sta	TMP1
	lda	DSTACK-1,x
	sta	TMP2
	iny
	dex
	dex
	tya
	cmp	buffer
	bne	@number1
	lda	TMP1
	sta	DSTACK-2,x
	lda	TMP2
	sta	DSTACK-1,x
	plp
	bne	@number2
	jsr	negate
@number2:
	NEXT
@error1:
	pla
	dex	; discard *10
	dex
	dex	; discard address
	dex
	jsr	primm
	.byte	"undefined word", eol, 0
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
	;sta	TMP4
	pha
	lda	LOAD		; skip i/o status checking if read from keyboard
	beq	@query4
	jsr	readst
	cmp	#0
	bne	@query3		; branch if eof or error
@query4:
	;lda	TMP4
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

; QUERY
; read input and fill input buffer
;	defcode "query", 0
;query:
;	stx	XSAVE
;	ldx	#0
;	stx	CPTR
;@query1:
;	jsr	chrin
;@query2:
;	sta	buffer+1,x
;	inx
;	cpx	#$41		; check max length
;	beq	@error1
;	cmp	#eol
;	bne	@query1
;	dex
;	stx	buffer
;	ldx	XSAVE
;	NEXT
;
;@error1:
;	jsr	primm
;	.byte	" ?", eol, 0
;	lda	#eol
;	sta	buffer+1
;	ldx	XSAVE
;	NEXT

; WORD ( delimiter --- string )
; get the next word from the input stream
; string = address of length byte, following string
	defcode "word", 0
word:	
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
; addr = execution address of string (0 if not found)

	defcode "find", 0
find:	lda	DSTACK-2,x	; save string address
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
	clc
	lda	AUX+1
	adc	#3
	sta	AUX+1
	lda	TMP3
	adc	AUX+1
	sta	DSTACK-2,x
	lda	TMP4
	adc	#0
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
	lda	LOAD
	bne	@interpret0
	jsr	primm
	.byte	"ok ",eol,0
@interpret0:
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
	jsr	trace
	jmp	interpret
@error1:
	jsr	primm
	.byte	"compile only word",eol,0
	jmp	abort

; COLD
; cold start
	defcode "cold", 0
cold:	; tsx
	; stx	SPSAVE		; save original stack pointer
				; (i.e. sp can be restored when bye command is supported.
	lda	#8
	ldy	#0
	sta	DEVICE
	sty	DEVICE+1
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
	.byte	eol,lowcase,"    **** SturmFORTH ****",eol, eol
        .byte               "    Coded by Juha Ollila",eol,eol,0
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
	jsr	printbyte
	jsr	primm
	.byte	"): ",0
	ldx	#0
@trace1:
	cpx	XSAVE
	beq	@trace2
	lda	DSTACK+1,x
	tay
	lda	DSTACK,x
	jsr	printdec
	jsr	printspc
	inx
	inx
	jmp	@trace1
@trace2:
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

; SturmForth (subroutine threaded code) interpreter
; Coded by Juha Ollila
;

	.setcpu		"6502"
	.include	"define.asm"
	.feature	dollar_is_pc

; In Forth, NEXT routine jumps to the next word.
.macro NEXT
	rts
.endmacro

; In Forth, DOCOL is the interpreter function for : definitions
.macro DOCOL
.endmacro

basic:
	; .org	$801-2
	.word	*+2	; start address

	; 2019 SYS 2062
	.word	@nextbasicline
	.word	2019
	.byte	$9e, " 2062", $00
@nextbasicline:
	.word	0

init:	tsx
	stx	SPSAVE
	ldx	#0	; initialize DATASTACK ptr
	stx	SPSAVE+1

	lda	#<lastword
	sta	LASTPTR
	lda	#>lastword
	sta	LASTPTR+1
	lda	#<dictstart
	sta	HEREPTR
	lda	#>dictstart
	sta	HEREPTR+1

	; TODO: initialize interpreter
	; TODO: run interpreter (QUIT)
	;.word	:+
	;jsr	trace

	;lda	#$ff
	;sta	DSTACK,x
	;inx
	;lda	#$00
	;sta	DSTACK,x
	;inx
	;jsr	trace
	;jsr	bitxor
	;jsr	trace
	;ldx	SPSAVE
	;txs
	;jsr	trace
	;jsr	lit
	;.word	2019
	;jsr	trace
	;rts
	jsr	interpret
	rts

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
	;cpx	#0
	;bcc	@droperror
	NEXT
;@droperror:
;	inc	$d020	; TODO stack underflow handling
;	NEXT

; DUP ( n --- n n )
; duplicate the top entry on the stack.
	defcode	"dup", 0
dup:
	;cpx	#0
	;bcc	@duperror1
	lda	DSTACK-2,x
	sta	DSTACK,x
	lda	DSTACK-1,x
	sta	DSTACK+1,x
	inx
	inx
	;cpx	#MAXSTACK+1
	;bcs	@duperror2
	NEXT
;@duperror1:
;	inc	$d020 ; TODO no item in stack
;	NEXT
;@duperror2:
;	inc	$d020 ; TODO stack overflow
;	NEXT

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
	.byte	$9d, $ff, $00
;	sta	$00ff,x
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

; literal ( --- n ) 
; pushes literal to the stack
	defcode "lit", 0
literal:
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

; hex ( --- )
; changes base to hex
	defcode "hex", 0
hex:
	lda	#16
	sta	BASE
	NEXT

; dec ( --- )
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
	jmp	printdec
@dot1:	pla
	jmp	printword
	
; *** MEMORY (PEEK & POKE) ***
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

; c! ( n addr --- )
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

; c@ ( addr --- n )
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

; negate ( n --- -n )
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

; /mod ( n1 n2 --- (n1 mod n2) n1/n2 )
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

; mod ( n1 n2 --- (n1 mod n2) )
	defcode "mod", 0
mod:	jsr	divmod
	jmp	drop

; div ( n1 n2 --- n1/n2 )
	defcode "div", 0
div:	jsr	divmod
	jsr	swap
	jmp	drop

; min ( n1 n2 --- min )
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

; max ( n1 n2 --- max )
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

; abs ( n --- |n| )
; absolute value.
	defcode "abs", 0
abs:	jsr	dup
	jsr	zeroless
	lda	DSTACK-2,x
	beq	@abs1
	jsr	drop
	jmp	negate
@abs1:	jmp	drop

; and ( n1 n2 --- (n1 and n2) )
; bitwise logical and.
	defcode "and", 0
bitand:	lda	DSTACK-4,x
	and	DSTACK-2,x
	sta	DSTACK-4,x
	lda	DSTACK-3,x
	and	DSTACK-1,x
	sta	DSTACK-3,x
	jmp	drop

; or ( n1 n2 --- (n1 or n2) )
; bitwise logical or.
	defcode "or", 0
bitor:	lda	DSTACK-4,x
	ora	DSTACK-2,x
	sta	DSTACK-4,x
	lda	DSTACK-3,x
	ora	DSTACK-1,x
	sta	DSTACK-3,x
	jmp	drop

; xor ( n1 n2 --- (n1 xor n2) )
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
; *** COMPILER ***
;

; hide the word in the dict
; hide ( addr --- )
	defcode "hide", 0
hide:	;jsr	twoplus		; ( addr+2 )
	;jsr	dup		; ( addr+2 addr+2)
	;jsr	fetch		; ( addr+2 flags)
	;push	FLAG_H		; ( addr+2 flags flag_h)
	;jsr	or		; ( addr+2 flags+h)
	;jsr	swap		; ( flags+h addr+2)
	;jsr	store
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

; unhide the word in the dict
; unhide ( addr --- )
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

; set compilation state on
; ] ( --- )
	defcode "]", 0
rightbr:
	lda	#1
	sta	STATE
;	jsr	primm
;	.byte	"state = 1",eol,0
	NEXT

; set compilation state off
; [ ( --- )
	defcode "[", FLAG_I
leftbr:
	lda	#0
	sta	STATE
;	jsr	primm
;	.byte	"state = 0",eol,0
	NEXT

; create a new word
; expects a name in input stream
	defcode "create", 0
create:	lda	#space		; get name of new dict entry
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
@create1:
	lda	(TMP1),y	; copy length of name and name
	sta	(TMP3),y
	iny
	cpy	AUX
	bne	@create1
	clc			; update HERE
	lda	TMP3		; HERE = old last ptr + 2 + length + length byte
	adc	AUX
	sta	HEREPTR
	lda	TMP4
	adc	#0
	sta	HEREPTR+1
	NEXT
	;clc			; update destination address
	;lda	TMP3		; dst addr = old last ptr + 2 + length + length byte
	;adc	AUX
	;sta	TMP3
	;lda	TMP4
	;adc	#0
	;sta	TMP4
	;clc			; update HERE
	;lda	TMP3		; HERE = dst addr + code length
	;adc	#@code2-@code1
	;sta	HEREPTR
	;lda	TMP4
	;adc	#0
	;sta	HEREPTR+1
	;lda	HEREPTR		; update self modifying code
	;sta	@code1+1
	;lda	HEREPTR+1
	;sta	@code1b+1
	;lda	#<@code1
	;sta	TMP1
	;lda	#>@code1
	;sta	TMP2
	;lda	#@code2-@code1
	;sta	AUX
	;ldy	#0
;@create2:			; copy code
	;lda	(TMP1),y
	;sta	(TMP3),y
	;iny
	;cpy	AUX
	;bne	@create2
	;NEXT
; code which is copied to new word
; when new word is executed it places data fields address to the stack.
;@code1:	;lda	#0
	;sta	DSTACK,x
;@code1b:
	;lda	#0
	;sta	DSTACK+1,x
	;inx
	;inx
	;NEXT
;@code2:

; allocate n bytes from the dictionary
; allot ( n --- )
	defcode "allot", 0
allot:	jsr	here
	;jsr	fetch		
	jsr	plusstore
	NEXT

; stores n to the dictionary
; , ( n --- )
	defcode ",", 0
comma:	jsr	here
	jsr	fetch
	jsr	store
	jsr	two
	jsr	allot
	NEXT

; define a new word
; expects a name 
; : ( --- )
	defcode ":", 0
colon:
	jsr	create		; create word (dict entry)
	jsr	last		; hide the created word
	jsr	fetch
	jsr	hide
	jsr	rightbr		; enter into the compilation mode
	NEXT			; EXIT

; stop defining a new word
; ; ( --- )
	defcode ";", FLAG_I
semicolon:
	jsr	literal
	rts
	.byte	0
	jsr	comma
	jsr	last
	jsr	fetch
	jsr	unhide
	jsr	leftbr
	NEXT			; EXIT	
;
; *** INTERPRETER ***
;



; number ( addr --- n )
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
	; * 10
	lda	TMP1
	sta	DSTACK,x
	inx
	lda	TMP2
	sta	DSTACK,x
	inx
	lda	#10
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
	cmp	#':'
	bcs	@error1
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
	; todo: abort
	NEXT

; query
; read input and fill input buffer
	defcode "query", 0
query:
	stx	XSAVE
	ldx	#0
	stx	CPTR
@query1:
	jsr	chrin
	sta	buffer+1,x
	inx
	cpx	#$41		; check max length
	beq	@error1
	cmp	#eol
	bne	@query1
	dex
	stx	buffer
	ldx	XSAVE
	NEXT
@error1:
	jsr	primm
	.byte	" ?", eol, 0
	lda	#eol
	sta	buffer+1
	ldx	XSAVE
	NEXT


; word ( delimiter --- string )
; get the next word from the input stream
; string = address of length byte, following string
	defcode "word", 0
word:	
	lda	DSTACK-2,x	; get delimiter
	sta	TMP1
	stx	XSAVE
	ldy	#0
	;ldx	#0
	;sty	CPTR
@word1:				; ignore leading delimiters
	ldx	CPTR
	inc	CPTR
	lda	buffer+1,x
	;inx
@delim1:
	cmp	TMP1		; branch if delimiter
	beq	@word1
	cmp	#eol		; branch if eol
	beq	@error2
	bne	@word3
@word2:	ldx	CPTR
	inc	CPTR
	lda	buffer+1,x	; get next char
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

; find ( string --- addr )
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

; execute ( addr --- )
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

; interpret
; - word
; - find
; -- if found
; --- execute word
; -- else convert to number
; --- push to stack

	defcode "interpret", 0
interpret:
	lda	#eol
	jsr	chrout
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
	;jsr	trace
	jsr	find
	;jsr	trace
	lda	DSTACK-2,x
	ora	DSTACK-1,x
	bne	@interpret2
	jsr	number
	lda	STATE
	beq	@interpret1	; branch if in interpreter mode
	push	$2018		; push clc (dummy) + jsr into stack
	jsr	comma		; save jsr to dictionary
	push	literal		; save address of literal to dictionary
	jsr	comma
	jsr	comma		; save number to dictionary
	;jsr	primm
	;.byte	"number compiled",eol,0
	jmp	@interpret1
;@interpret3:
;	jmp	@interpret4
@interpret2:
	lda	IMM
	;jsr	printbyte
	;lda	IMM
	bne	@execute	; branch if immediate mode of word
	lda	STATE
	beq	@execute	; branch if in interpreter mode
	jsr	literal
	.byte	$18		; clc (dummy operation)
	.byte	$20		; jsr
	jsr	comma		; save rts to dictionary
	jsr	comma		; save address to dictionary
	;jsr	primm
	;.byte	"word compiled", eol, 0
	jmp	@interpret1
@execute:
	jsr	execute
	;jsr	primm
	;.byte	"word executed", eol, 0
	;jsr	trace
	jmp	@interpret1
@interpret3:
	dex
	dex
	jsr	trace
	jmp	interpret
	;NEXT

; cold
; cold start
	defcode "cold", 0
cold:	lda	#<lastword
	sta	LASTPTR
	lda	#>lastword
	sta	LASTPTR+1
	lda	#<dictstart
	sta	HEREPTR
	lda	#>dictstart
	sta	HEREPTR+1
	jmp	quit

; abort
	defcode "abort", 0
abort:	jmp	quit

; quit
; initializes Forth interpreter
lastword:
	defcode "quit", 0
quit:	ldx	SPSAVE	; initialize return stack
	txs
	ldx	#0	; initialize data stack
	stx	STATE  ; stop compilation mode
@quit1:	jsr	interpret
	; check stack
	jsr	primm
	.byte	"ok",eol,0
	jmp	@quit1
	jmp	:-	; makes assembler happy
	
;          Nucleus layer
;
;               !  *  */  */MOD  +  +!  -  /  /MOD  0<  0=  0>  1+  1-  2+
;               2-  2/  <  =  >  >R  ?DUP  @  ABS  AND  C!  C@  CMOVE
;               CMOVE>  COUNT  D+  D<  DEPTH  DNEGATE  DROP  DUP  EXECUTE
;               EXIT  FILL  I  J  MAX  MIN  MOD  NEGATE  NOT  OR  OVER  PICK
;               R>  R@  ROLL  ROT  SWAP  U<  UM*  UM/MOD  XOR
;
;
;          Device layer
;
;               BLOCK  BUFFER  CR  EMIT  EXPECT  FLUSH  KEY  SAVE-BUFFERS
;               SPACE  SPACES  TYPE  UPDATE
;
;
;          Interpreter layer
;
;               #  #>  #S  #TIB  '  (  -TRAILING  .  .(  <#  >BODY  >IN
;               ABORT  BASE  BLK  CONVERT  DECIMAL  DEFINITIONS  FIND
;               FORGET  FORTH  FORTH-83  HERE  HOLD  LOAD  PAD  QUIT  SIGN
;               SPAN  TIB  U.  WORD
;
;
;          Compiler layer
;
;               +LOOP  ,  ."  :  ;  ABORT"  ALLOT  BEGIN  COMPILE  CONSTANT
;               CREATE  DO  DOES>  ELSE  IF  IMMEDIATE  LEAVE  LITERAL  LOOP
;               REPEAT  STATE  THEN  UNTIL  VARIABLE  VOCABULARY  WHILE  [
;               [']  [COMPILE]  ]

trace:
	stx	XSAVE
	jsr	primm
	.byte	$d, "stack(",0
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
	;jsr	printword
	jsr	printdec
	jsr	printspc
	inx
	inx
	jmp	@trace1
@trace2:
	ldx	XSAVE
	rts


printcr:
	lda	#eol
	jsr	chrout
	rts

printspc:
	lda	#' '
	jsr	chrout
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
	bpl	@bina
	lda	#0
	sec
	sbc	AUX
	sta	AUX
	lda	#0
	sbc	AUX+1
	sta	AUX+1
	lda	#'-'
	jsr	chrout
@bina:
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

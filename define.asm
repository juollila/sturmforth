; Common definitions for SturmFORTH
; Coded by Juha Ollila

; constants
	eol	= $0d
	space	= $20
	lowcase = $0e

; zero page

	DSTACK	= 3 	; data stack is between $3-$72
	XSAVE	= $8b	; x register
	TMP1	= $8c
	TMP2	= $8d
	TMP3	= $8e
	TMP4	= $8f

	MAXSTACK = $6f

; other memory locations
	buffer	= $200 ; text buffer
	AUX	= $2f0 ; temporary register (2 bytes)
	SPSAVE	= $2f2 ; for debugging purposes (exit to basic)
	STATE	= $2f4 ; compilation state
	HEREPTR	= $2f6 ; data space pointer
	LASTPTR = $2f8 ; last dictionary entry
	CPTR	= $2fa ; character position in buffer
	BASE	= $2fc ; numeric base
	IMM	= $2fe ; immediate mode (used by find and interpreter)
	CREATE	= $2ff ; create1 vs create flag

; kernal functions
;	readst	= $ffb7
;	setlfs	= $ffba
;	setnam	= $ffbd
;	open	= $ffc0
;	close	= $ffc3
;	chkin	= $ffc6
;	chkout	= $ffc9
;	clrchn	= $ffcc
	chrin	= $ffcf
	chrout	= $ffd2
;	load	= $ffd5
;	save	= $ffd8
;	stop	= $ffe1
	getin	= $ffe4
;	setmsg	= $ff90
;	plot	= $fff0


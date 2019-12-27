#!/bin/sh
ca65 sturmforth.asm -t c64
ld65 sturmforth.o -C sturmforth.cfg -o sturmforth.prg

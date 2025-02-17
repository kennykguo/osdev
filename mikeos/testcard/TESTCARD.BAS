REM Testcard generator using default CGA text mode.
REM Original idea, (C)2009, B.Walker, G0LCU.
REM Issued as Public Domain for MikeOS.

REM Load in the binary to the address shown.
LOAD "TESTCARD.DAT" 60000

REM Poke in the values to the binary.
REM DO NOT ALTER THESE VALUES!!!
POKE 136 60022
POKE 234 60023

REM Call the binary to display the testcard.
CALL 60000

REM Hold the picture to remove the flashing cursor.
REM Press <Esc> to stop the program.
keyhold:
GETKEY x
IF x = 27 THEN GOTO cleanexit
GOTO keyhold

REM A clean exit routine.
cleanexit:
CLS
END


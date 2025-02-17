
REM DEMO to toggle the CAPSLOCK key in MikeBASIC.
REM Original idea copyright, (C)2010, B.Walker, G0LCU.
REM Issued as Public Domain for MikeOS, MikeBASIC.

REM Load the binary... NOTE! This CAN be POKEd in to make it purely MikeBASIC.
LOAD "CAPSLOCK.DAT" 65000
CLS
CURSOR OFF

REM The only variable is for keyboard access.
k = 13

REM This is a continuous loop to toggle the CAPSLOCK only.
REM The NUMLOCK and SCROLL LOCK are also available by POKE(ing) the correct
REM values into the correct locations.

toggle:
REM This CALL toggles the CAPSLOCK to ON.
REM Address 65013 is POKEd to enable the various key locks.
REM POKE 96 65013 enables NUMLOCK and CAPSLOCK.

CALL 65000
REM [ENTER/RETURN] to exit this call with the LED ON.
GETKEY k
IF k = 13 THEN GOTO cleanexit
MOVE 0 1
PRINT "Capslock ON!   "
PAUSE 5

REM This CALL toggles tha CAPSLOCK to OFF.
REM Address 65037 is POKEd to disable the various key locks.
REM POKE 159 65037 disables NUMLOCK and CAPSLOCK.

CALL 65024
REM [Space Bar] to exit this call with the LED OFF.
GETKEY k
IF k = 32 THEN GOTO cleanexit
MOVE 0 1
PRINT "Capslock OFF!  "
PAUSE 5
GOTO toggle

REM Clean exit back to MikeOS.
cleanexit:
CURSOR ON
PAUSE 20
CLS
END
REM DEMO program end...

REM Analogue voltmeter for MikeOS issued as PD, 25-11-2009.
REM Original copyright (C)2009, B.Walker, G0LCU.

REM Load in the machine code hardware routine.
LOAD "PARAPORT.DAT" 65000

REM Master variables here...
REM a = analogue value to plot.
REM b = ADC byte grabbed from the port.
REM k = ASCII key code set to the <Esc> key.
REM n = can be anything and is a counter here.
b = 255
a = b / 5
k = 27
n = 0

REM Poke the hardware access binary from here.
REM DO NOT TOUCH THESE FOR THE EXISTING HW!!!
POKE 57 65059
POKE 254 65060

REM Clear the screen area...
CLS

REM This sets up the initial screen.
PRINT ""
PRINT "          Simple analogue voltmeter for MikeOS using the parallel port."
PRINT "          (C)2009, B.Walker, G0LCU. Now issued as PD for use on MikeOS."
PRINT ""
PRINT "              0   2.0  4.0  6.0  8.0 10.0 12.0 14.0 16.0 18.0 20.0"
PRINT "    Scale.    0   1.0  2.0  3.0  4.0  5.0  6.0  7.0  8.0  9.0 10.0"
PRINT "              0   0.5  1.0  1.5  2.0  2.5  3.0  3.5  4.0  4.5  5.0"
PRINT "              ++++++++++++++++++++++++++++++++++++++++++++++++++++"
PRINT "             <|                                                   >"
PRINT "              +----+----+----+----+----+----+----+----+----+----+-"

REM This is the mainloop...
mainloop:
REM Call the subroutine to access the parallel port...
CALL 65000

REM Get the byte stored from the parallel port...
PEEK b 65081
a = b / 5 

REM Slow it down to about 1 sample per second...
PAUSE 5

REM Re-adjust the print position for the animation...
MOVE 14 8
PRINT "|                                                   "

REM This creates the simple linear meter animation...
MOVE 14 8
n = 0
readoutloop:
PRINT "|" ;
n = n + 1
IF n > a THEN GOTO readoutloopout
GOTO readoutloop

REM Once the analogue display has been updated finish off the display.
REM This deliberately slows down the system and places the cursor
REM in the correct position after a full ADC grab.
readoutloopout:
MOVE 28 11
PRINT "Press <Esc> to Quit:- " ;

REM We need to get out of the loop, use <Esc> to do so.
GETKEY k
IF k = 27 THEN GOTO cleanexit
GOTO mainloop

REM Exit the program cleanly...
cleanexit:
CLS
PRINT ""
PRINT "Program terminated..."
END


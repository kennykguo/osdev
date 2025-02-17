REM ADC calibration for MikeOS issued as PD, 08-01-2010.
REM Original copyright (C)2010, B.Walker, G0LCU.

REM Load in the machine code hardware routine.
LOAD "PARAPORT.DAT" 65000

REM Master variable(s) here...
REM a = 0 to 255 from the ADC output.
REM k = ASCII 27, ~Esc~ to quit the progam.
a = 255
k = 27

REM Poke the hardware access binary from here.
REM DO NOT TOUCH THESE FOR THE EXISTING HW!!!
POKE 57 65059
POKE 254 65060

mainloop:
REM Clear the screen area...
CLS

REM Show (C), etc, and generate a simple screen...
PRINT ""
PRINT "ADC Project for MikeOS, (C)2010, B.Walker, G0LCU."
PRINT ""
PRINT "Calibrating the ADC, see ~MANUAL.TXT~ for more info`."
PRINT ""

REM Call the subroutine to access the parallel port...
CALL 65000

REM Get the byte stored from the parallel port...
PEEK a 65081

REM print the value to the screen.
PRINT "Value read from the parallel port is:- " ;
PRINT a ;
PRINT "..."

REM Remind us how to Quit the program.
PRINT ""
PRINT "Press <Esc> to Quit:- " ;

REM Slow it down a bit deliberately...
PAUSE 5

REM We need to get out of the loop, use <Esc> to do so.
GETKEY k
IF k = 27 THEN GOTO cleanexit
GOTO mainloop

REM Exit the program cleanly...
cleanexit:
CLS
END


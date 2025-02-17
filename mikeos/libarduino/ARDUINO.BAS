
REM Arduino DEMO to access ADC1 and display a digital and
REM analogue readout.
REM Original idea copyright, (C)2010, B.Walker, G0LCU.
REM Issued to MikeOS as Public Domain...


REM Set up variables here.
REM a = Arduino 8 bit value in decimal.
REM b = 7 bit depth bar graph value.
REM c = column to be PRINTed to.
REM d = DEMO mode override, d = 1, DEMO mode.
REM k = keyboard value, 27 for the "Esc" key.
REM m = anything and discarded after each use.
REM n = anything and discarded after each use.
REM o = anything and discarded after each use.
a = 255
b = 127
c = 0
d = 1
k = 27
m = 0
n = 0
o = 0
REM Variable list done!


REM Display version, OS and (C) first.
CURSOR OFF
MOVE 25 1
PRINT "Arduino DEMO, Version 0.20...."
MOVE 16 3
PRINT "(C)2010, B.Walker, G0LCU. Issued as PD to MikeOS."
MOVE 21 5
PRINT "[D] or [d] for default DEMO mode......" ;
MOVE 21 6
PRINT "[ENTER/RETURN] for REAL Arduino Mode.." ;
REM Initial display setup done!


demohold:
REM Keyboard selection access.
GETKEY k
IF k = 68 THEN GOTO start
IF k = 100 THEN GOTO start
IF k = 13 THEN GOTO serialon
GOTO demohold
REM Keyboard selection done!


serialon:
REM Switch on the Serial Port to 1200bps.
d = 0
SERIAL ON 1200
GOTO startserial
REM Serial setup done!


start:
REM Draw the display here, digital box first.
d = 1
startserial:
MOVE 20 5
PRINT "                                        "
MOVE 20 6
PRINT "                                        "
MOVE 31 5
n = 218
PRINT CHR n ;
n = 196
FOR c = 32 TO 47
MOVE c 5
PRINT CHR n ;
MOVE c 7
PRINT CHR n ;
NEXT c
MOVE 48 5
n = 191
PRINT CHR n ;
n = 179
MOVE 31 6
PRINT CHR n ;
MOVE 48 6
PRINT CHR n ;
n = 192
MOVE 31 7
PRINT CHR n ;
n = 217
MOVE 48 7
PRINT CHR n ;
MOVE 34 6
PRINT "0.00 Volt(s)."
MOVE 23 8
PRINT "(Digital resolution is 0.02 Volts.)"
REM Digital readout done!
REM Now the analogue box with a 0V to 5+V scale.
n = 218
MOVE 7 14
PRINT CHR n ;
n = 197
m = 193
FOR c = 8 TO 71
MOVE c 14
PRINT CHR n ;
MOVE c 16
PRINT CHR m ;
NEXT c
n = 191
MOVE 72 14
PRINT CHR n ;
n = 179
MOVE 7 15
PRINT CHR n ;
MOVE 8 15
PRINT CHR n ;
MOVE 71 15
PRINT CHR n ;
MOVE 72 15
PRINT CHR n ;
n = 217
MOVE 72 16
PRINT CHR n ;
n = 192
MOVE 7 16
PRINT CHR n ;
c = 8
n = 179
scalemarker:
MOVE c 13
PRINT CHR n ;
c = c + 5
IF c > 72 THEN GOTO scale
GOTO scalemarker
scale:
MOVE 7 12
PRINT "0.0  0.4  0.8  1.2  1.6  2.0  2.4  2.8  3.2  3.6  4.0  4.4  4.8 V."
MOVE 8 17
PRINT "(Analogue resolution is half of one small division, 0.04 Volts.)"
MOVE 5 18
PRINT "Scale in 0.08 Volts per small division, 0.40 Volts per large division."
MOVE 5 20
PRINT "A simple 0.00 to 5.10 Volt DC DEMO voltmeter, using the Arduino Board."
MOVE 29 22
PRINT "Press 'Esc' to Quit..." ;
REM Analogue readout done!


scan:
REM Grab a byte from the serial port or generate one randomly.
IF d = 0 THEN SERIAL REC a
IF d = 1 THEN RAND a 0 255
REM Generate a pseudo-floating point digital readout.
CURSOR OFF
MOVE 34 6
PRINT "0.00" ;
IF a = 0 THEN GOTO rescan
b = a * 2
IF b > 99 THEN MOVE 35 6
IF b > 99 THEN PRINT b ;
IF b > 99 THEN MOVE 35 6
IF b > 99 THEN CURSCHAR n
IF b > 99 THEN MOVE 34 6
IF b > 99 THEN PRINT CHR n ;
IF b > 99 THEN PRINT "." ;
IF b > 99 THEN GOTO rescan
IF b > 9 THEN MOVE 36 6
IF b > 9 THEN PRINT b ;
IF b > 9 THEN GOTO rescan
IF b > 0 THEN MOVE 37 6
IF b > 0 THEN PRINT b ;
REM Pseudo-floating point digital readot done!
rescan:
REM Display the analogue display.
b = a / 2
o = 1
c = 9
n = 179
MOVE 8 15
PRINT CHR n ;
PRINT "                                                              " ;
PRINT CHR n ;
m = 221
n = 222
IF b = 0 THEN GOTO exitkey
MOVE 8 15
IF o = 1 THEN PRINT CHR n ;
n = 219
bargraph:
IF o >= b THEN GOTO exitkey
MOVE c 15
IF o <= b THEN PRINT CHR m ;
o = o + 1
MOVE c 15
IF o <= b THEN PRINT CHR n ;
c = c + 1
o = o + 1
GOTO bargraph
REM Analogue bargraph display done!
exitkey:
PAUSE 10
CURSOR ON
REM Press the "Esc" key to quit.
GETKEY k
IF k = 27 THEN GOTO cleanexit
GOTO scan
REM DEMO or REAL display finished!


cleanexit:
REM Quit the program here.
CLS
END
REM Program end...


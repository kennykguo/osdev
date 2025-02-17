REM Test the BIOS keyflags address in MikeBASIC.
REM Original idea copyright, (C)2011, B.Walker, G0LCU.
REM Issued as Public Domain for MikeOS, MikeBASIC.

REM Poke in the binary to create the KEYLOOK.DAT.
POKE 156 65500
POKE 80 65501
POKE 82 65502
POKE 30 65503
POKE 186 65504
POKE 64 65505
POKE 0 65506
POKE 142 65507
POKE 218 65508
POKE 160 65509
POKE 23 65510
POKE 0 65511
POKE 31 65512
POKE 90 65513
POKE 162 65514
POKE 240 65515
POKE 255 65516
POKE 88 65517
POKE 157 65518
POKE 195 65519
POKE 195 65520

REM Call the above poked, into memory, binary.
begin:
CALL 65500

REM Read the address below to obtain the keyflags.
PEEK c 65520

REM Print the decimal value to the screen.
PRINT c ;
PRINT " " ;

REM If both of the CAPS SHIFT keys are pressed ONLY...
REM ...stop the program and return to MikeOS.
IF c = 3 THEN GOTO cleanexit
GOTO begin

REM Quit the program...
cleanexit:
PAUSE 20
END
REM End of test program.


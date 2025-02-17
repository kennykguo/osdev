
=============================================================================

                   MONITEST.BAS, A Display Alignment Tool.
                   ---------------------------------------

  Overview:-
  ----------

                   MONITEST.BAS, (C)2010, B.Walker, G0LCU.

(Apologies for any typos or grammar errors as it has not been checked
at all.)

This entirely MikeBASIC program is a test tool to set up and align
all types of monitor displays. It generates many different signals
used to set up the geometry of CRT, (Cathode Ray Tube), type
monitors and current monitors too.

The code is FREE and issued as Public Domain for MikeOS 4.0x and
greater. It is as good as if not better than a hardware commercial
item for sale at around 200 UKP, at the time of writing 19-02-2010:-

       http://cpc.farnell.com/ozan/oz-t1500/teletest-pc/dp/IN00454

The main signals the program does are:-

1) Centre Cross.
2) Crosshatch.
3) Colour/Color Bars.
4) Grey/Gray Scales.
5) Black To White Purity, (four levels, black = sync pulses only).
6) Primay Colours/Colors Purity, (Red, Green and Blue).
7) Focus Dots.
8) Multiburst.
9) Chequerboard.

Plus four extra signals:-

10) Pulse And Bar.
11) Line And Block.
12) Astigmatism.
13) Moire Patterning.

Numbers 10), 11) and 12) are added mainly for ancient monochrome displays,
but are still good for old colour monitors with CVBS inputs, (Composite
Video), as well as dedicated video inputs.

Number 13) is NOT a true Moire test but is just OK as this is because
of the limitation of video TEXT MODE 3.

  MINIMUM Requirements:-
  ----------------------

1) Any working complete PC using an 80386 CPU or higher, (this is the bottom
   limit of MikeOS, not what is actually a possible minimum).
2) Any Video Card/Adaptor that is capable of video TEXT MODE 3.
3) At least 1MB RAM.
4) A video output socket, CVBS, CGA, EGA, VGA etc...
5) Any 'working' colour, (black and white too!), monitor to setup/test.
6) A means of booting into MikeOS; (Floppy Disk, USB Stick or CDROM).
7) MikeOS 4.0x.
8) Any necessary interconnecting video cables.

Tested on a(n) RM, (Research Machines), ancient `486 8MB RAM laptop,
this HP Pavilion Dual Core development notebook, and finally, a current
Philips Quad Core desktop; all using Floppy Disk access, using either a USB
Floppy Drive or dedicated internal Floppy Drive...

Enjoy finding simple solutions to often very difficult problems... :)

Bazza, G0LCU... :)

=============================================================================

                                IMPORTANT:-
                                -----------

    The Legal Stuff:-
    -----------------

    These programs are Freeware and no profit will be made from them,
    also all of the files must remain unaltered and intact including this
    one. The author is not responsible for any damage to, or loss of, or
    failure of equipment or data caused in any way by the use of these
    programs. There is NO warranty with the use of these software releases
    and YOU USE THEM AT YOUR OWN RISK.

-----------------------------------------------------------------------------

                                 WARNING.
                                 --------

  1) DISCONNECT any faulty equipment under test from the MAINS supply.
  2) If a DC supply is used do NOT reverse polarity the connections.
  3) Do NOT power up any electronic item until it is safe to do so.
  4) CHECK and RECHECK all of your construction and repair work thoroughly.
  5) Handle ALL tools used with care.
  6) Beware of ALL types of solvents, glues and etching fluids.
  7) NEVER leave a soldering iron switched on unattended.
  8) KEEP everything OUT of the reach of small children.
  9) Switch OFF the computer before disconnecting or connecting any hardware.
 10) And finally read 1) to 9) again.

-----------------------------------------------------------------------------

   Contact:-
   ---------

   Mr Barry Walker, G0LCU,
   70 King George Road,
   Loughborough,
   Leicestershire,
   LE11 2PA,
   England.

   Email:-     wisecracker@tesco.net
   URL:-       http://homepages.tesco.net/wisecracker/G0LCU.HTM

   (Author of the ~TestGear?~ projects in the ~docs/hard~ drawer of AMINET.)

               http://aminet.net/search?readme=wisecracker

=============================================================================

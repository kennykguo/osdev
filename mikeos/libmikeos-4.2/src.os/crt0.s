
! You must assemble this code with as86.
! If you use NASM, you will suffer "ld86: no start symbol" message.

	.text

	entry	startup
	extern	___cstartup

startup:
	br	___cstartup
	hlt


; KEYLOOK.ASM sub-routine to read CAPS LOCK, NUMLOCK,
; left CAPS SHIFT and right CAPS SHIFT for MikeOS.
; Original copyright, (C)2010, B.Walker, G0LCU.
; For use with MikeBASIC and issued as Public Domain.

; Keyoard HW modification, (C)2010, B.Walker, G0LCU.
; 8088 CPU instruction set compatible.

; KEYLOOK.ASM, Version 0.00.10, assembly code.

; Assembled under MS-DOS using the AD86 combination.
; The binary is 21 bytes in size.

	org 65500		;Setup for MikeBASIC program.

; KEYLOOK:
	pushf			;Store flags and all of the used
	push	ax		;registers
	push	dx		;
	push	ds		;
	mov	dx,0040h	;Point to the SEGMENT to access
	mov	ds,dx		;the real time BYTE KB storage.
	mov	al,[0017h]	;Move the BYTE value from memory
				;into the AL register ready to read.
	pop	ds		;Restore the DS and DX registers.
	pop	dx		;
	mov	[store],al	;Move the flags to a known memory area.
	pop	ax		;Restore the remaining registers and
	popf			;flags.
	ret			;Return to the calling routine.
store	db	0c3h		;This is the keyboard flag byte.

; End of KEYLOOK.ASM code...

; CAPSLOCK, (NUMLOCK), ON/OFF sub-routines for MikeOS.
; Original copyright, (C)2010, B.Walker, G0LCU.
; For use with MikeBASIC and issued as Public Domain.

; CAPSLOCK.ASM, Version 0.00.10, assembly code.

; Assembled under MS-DOS using the AD86 combination.
; The binary is 48 bytes in size.

	org 65000		;Setup for MikeBASIC program.

; CAPSLOCKON:
	pushf			;Store flags and all of the used
	push	ax		;registers.
	push	dx		;
	push	ds		;
	mov	dx,0040h	;Point to the SEGMENT to access
	mov	ds,dx		;the REAL time BYTE KB storage.
	mov	al,[0017h]	;Move the BYTE value from memory
				;into the AL register.
	or	al,040h		;SET the required bit, (bit 6),
				;to a ~1~.
				;NOTE, change this value for NUMLOCK!
	mov	[0017h],al	;Replace the modified byte back
				;into the memory location.
	int	9		;Now execute the necessary interrupt.
	pop	ds		;Restore the registers back to
	pop	dx		;their original values...
	pop	ax		;
	popf			;And the flags too.
	ret			;Return to the calling routine.

; CAPSLOCKOFF:
	pushf			;Store flags and all of the used
	push	ax		;registers.
	push	dx		;
	push	ds		;
	mov	dx,0040h	;Point to the SEGMENT to access
	mov	ds,dx		;the REAL time BYTE KB storage.
	mov	al,[0017h]	;Move the BYTE value from memory
				;into the AL register.
	and	al,0bfh		;RESET the required bit, (bit 6),
				;to a ~0~, keeping all other bits intact.
				;NOTE, change this value for NUMLOCK!
	mov	[0017h],al	;Replace the modified byte back
				;into the memory location.
	int	9		;Now execute the necessary interrupt.
	pop	ds		;Restore the registers back to
	pop	dx		;their original values...
	pop	ax		;
	popf			;And the flags too.
	ret			;Return to the calling routine.

; End of CAPSLOCK.ASM code...

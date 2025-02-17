
; CAPSLOCK, (NUMLOCK), ON/OFF sub-routines along with left
; CAPS SHIFT and right CAPS SHIFT read subroutine for MikeOS.
; Original copyright, (C)2010, B.Walker, G0LCU.
; For use with MikeBASIC and issued as Public Domain.

; KEYFLAGS.ASM, Version 0.00.20, assembly code.

; Assembled under MS-DOS using the AD86 combination.
; The binary is 75 bytes in size.

	org 60000

; KEYLOCKON:
	pushf			;Store flags and all of the used
	push	ax		;registers.
	push	dx		;
	push	ds		;
	mov	dx,0040h	;Point to the SEGMENT to access
	mov	ds,dx		;the toggled BYTE KB storage.
	mov	al,[0017h]	;Move the BYTE value from memory
				;into the AL register.
	or	al,040h		;SET the required bit, (bit 6),
				;to a ~1~.
				;NOTE, change this value for NUMLOCK!
	mov	[0017h],al	;Replace the modified byte back
				;into the memory location.
	int	9		;Now execute the necessary interrupt.
	call	readflags	;Make sure to store a copy.
	pop	ds		;Restore the registers back to
	pop	dx		;their original values...
	pop	ax		;
	popf			;And the flags too.
	ret			;Return to the calling routine.

; KEYLOCKOFF:
	pushf			;Store flags and all of the used
	push	ax		;registers.
	push	dx		;
	push	ds		;
	mov	dx,0040h	;Point to the SEGMENT to access
	mov	ds,dx		;the toggled BYTE KB storage.
	mov	al,[0017h]	;Move the BYTE value from memory
				;into the AL register.
	and	al,0bfh		;RESET the required bit, (bit 6),
				;to a ~0~, keeping all other bits intact.
				;NOTE, change this value for NUMLOCK!
	mov	[0017h],al	;Replace the modified byte back
				;into the memory location.
	int	9		;Now execute the necessary interrupt.
	call	readflags	;Make sure to store a copy.
	pop	ds		;Restore the registers back to
	pop	dx		;their original values...
	pop	ax		;
	popf			;And the flags too.
	ret			;Return to the calling routine.

readflags:
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

; End of KEYFLAGS.ASM code...
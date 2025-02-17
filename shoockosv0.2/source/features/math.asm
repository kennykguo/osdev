; ==================================================================
; MikeOS -- The Mike Operating System kernel
; Copyright (C) 2006 - 2009 MikeOS Developers -- see doc/LICENSE.TXT
;
; MATH ROUTINES
; ==================================================================

; ------------------------------------------------------------------
; os_get_random -- Return a random number (0 to 255)
; IN: Nothing; OUT: AL = number

os_get_random:
	pusha

	mov ax, 0
	int 1Ah				; Get BIOS clock counter

	mov byte [.tmp], dl

	popa

	mov byte al, [.tmp]

	ret


	.tmp db 0


; ------------------------------------------------------------------
; os_bcd_to_int -- Converts binary coded decimal number to an integer
; IN: AL = BCD number; OUT: AX = integer value

os_bcd_to_int:
	pusha

	mov bl, al			; Store entire number for now

	and ax, 0Fh			; Zero-out high bits
	mov cx, ax			; CH/CL = lower BCD number, zero extended

	shr bl, 4			; Move higher BCD number into lower bits, zero fill msb
	mov al, 10
	mul bl				; AX = 10 * BL

	add ax, cx			; Add lower BCD to 10*higher
	mov [.tmp], ax

	popa
	mov ax, [.tmp]			; And return it in AX!
	ret


	.tmp	dw 0


; ------------------------------------------------------------------
; os_long_int_negate -- Multiply value in DX:AX by -1
; IN: DX:AX = long integer; OUT: DX:AX = -(initial DX:AX)

os_long_int_negate:
	neg ax
	adc dx, 0
	neg dx
	ret


; ==================================================================


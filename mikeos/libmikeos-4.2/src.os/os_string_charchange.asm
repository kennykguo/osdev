
; @@@ void mikeos_string_charchange(char *string, unsigned char newcharacter, unsigned char oldcharacter);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_string_charchange

_mikeos_string_charchange:
	push	bp
	mov	bp, sp
	push	si

	mov	si, [bp + 4]
	mov	bl, [bp + 6]
	mov	al, [bp + 8]

	mov	bp, os_string_charchange
	call	bp

	pop	si
	pop	bp
	ret

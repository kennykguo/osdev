
; @@@ unsigned int mikeos_find_char_in_string(char *string, char character);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_find_char_in_string

_mikeos_find_char_in_string:
	push	bp
	mov	bp, sp
	push	si

	mov	si, [bp + 4]
	mov	al, [bp + 6]

	mov	bx, os_find_char_in_string
	call	bx

	pop	si
	pop	bp
	ret

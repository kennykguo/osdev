
; @@@ void mikeos_string_strip(char *string, char character);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_string_strip

_mikeos_string_strip:
	push	bp
	mov	bp, sp
	push	si

	mov	si, [bp + 4]
	mov	al, [bp + 6]

	mov	bx, os_string_strip
	call	bx

	pop	si
	pop	bp
	ret

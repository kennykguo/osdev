
; @@@ unsigned int mikeos_string_length(char *string);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_string_length

_mikeos_string_length:
	mov	bx, sp
	mov	ax, [ss:bx + 2]

	mov	bx, os_string_length
	call	bx

	ret

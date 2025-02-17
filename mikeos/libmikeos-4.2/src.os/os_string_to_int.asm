
; @@@ unsigned int mikeos_string_to_int(char *string);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_string_to_int

_mikeos_string_to_int:
	mov	bx, sp
	push	si
	mov	si, [ss:bx + 2]

	mov	bx, os_string_to_int
	call	bx

	pop	si
	ret

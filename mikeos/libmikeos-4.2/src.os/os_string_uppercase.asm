
; @@@ void mikeos_string_uppercase(char *string);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_string_uppercase

_mikeos_string_uppercase:
	mov	bx, sp
	mov	ax, [ss:bx + 2]

	mov	bx, os_string_uppercase
	call	bx

	ret

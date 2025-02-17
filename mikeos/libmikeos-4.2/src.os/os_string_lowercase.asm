
; @@@ void mikeos_string_lowercase(char *string);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_string_lowercase

_mikeos_string_lowercase:
	mov	bx, sp
	mov	ax, [ss:bx + 2]

	mov	bx, os_string_lowercase
	call	bx

	ret


; @@@ void mikeos_input_string(char *buf);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_input_string

_mikeos_input_string:
	mov	bx, sp
	mov	ax, [ss:bx + 2]

	mov	bx, os_input_string
	call	bx

	ret

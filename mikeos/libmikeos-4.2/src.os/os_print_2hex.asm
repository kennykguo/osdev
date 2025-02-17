
; @@@ void mikeos_print_2hex(unsigned char value);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_print_2hex

_mikeos_print_2hex:
	mov	bx, sp
	mov	al, [ss:bx + 2]

	mov	bx, os_print_2hex
	call	bx

	ret

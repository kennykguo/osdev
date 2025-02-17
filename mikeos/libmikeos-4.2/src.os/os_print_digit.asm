
; @@@ void mikeos_print_digit(unsigned char value);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_print_digit

_mikeos_print_digit:
	mov	bx, sp
	mov	al, [ss:bx + 2]

	mov	bx, os_print_digit
	call	bx

	ret

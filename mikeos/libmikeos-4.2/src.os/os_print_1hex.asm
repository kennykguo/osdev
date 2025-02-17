
; @@@ void mikeos_print_1hex(unsigned char value);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_print_1hex

_mikeos_print_1hex:
	mov	bx, sp
	mov	al, [ss:bx + 2]

	mov	bx, os_print_1hex
	call	bx

	ret

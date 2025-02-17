
; @@@ void mikeos_print_4hex(unsigned int value);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_print_4hex

_mikeos_print_4hex:
	mov	bx, sp
	mov	ax, [ss:bx + 2]

	mov	bx, os_print_4hex
	call	bx

	ret

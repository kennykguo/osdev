
; @@@ void mikeos_print_horiz_line(unsigned int style);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_print_horiz_line

_mikeos_print_horiz_line:
	mov	bx, sp
	mov	ax, [ss:bx + 2]

	mov	bx, os_print_horiz_line
	call	bx

	ret

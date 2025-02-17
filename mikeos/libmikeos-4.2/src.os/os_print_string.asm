
; @@@ void mikeos_print_string(char *message);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_print_string

_mikeos_print_string:
	mov	bx, sp
	push	si
	mov	si, [ss:bx + 2]

	mov	bx, os_print_string
	call	bx

	pop	si
	ret

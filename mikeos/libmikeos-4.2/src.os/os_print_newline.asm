
; @@@ void mikeos_print_newline(void);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_print_newline

_mikeos_print_newline:
	mov	bx, os_print_newline
	call	bx

	ret

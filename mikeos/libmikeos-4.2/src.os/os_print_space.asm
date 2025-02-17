
; @@@ void mikeos_print_space(void);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_print_space

_mikeos_print_space:
	mov	bx, os_print_space
	call	bx

	ret

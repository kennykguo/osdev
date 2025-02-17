
; @@@ void mikeos_clear_screen(void);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_clear_screen

_mikeos_clear_screen:
	mov	bx, os_clear_screen
	call	bx

	ret

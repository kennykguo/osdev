
; @@@ void mikeos_show_cursor(void);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_show_cursor

_mikeos_show_cursor:
	mov	bx, os_show_cursor
	call	bx

	ret

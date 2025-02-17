
; @@@ void mikeos_hide_cursor(void);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_hide_cursor

_mikeos_hide_cursor:
	mov	bx, os_hide_cursor
	call	bx

	ret

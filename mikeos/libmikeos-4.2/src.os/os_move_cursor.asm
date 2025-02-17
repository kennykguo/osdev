
; @@@ void mikeos_move_cursor(unsigned char column, unsigned char row);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_move_cursor

_mikeos_move_cursor:
	push	bp
	mov	bp, sp

	mov	dl, [bp + 4]
	mov	dh, [bp + 6]

	mov	bx, os_move_cursor
	call	bx

	pop	bp
	ret

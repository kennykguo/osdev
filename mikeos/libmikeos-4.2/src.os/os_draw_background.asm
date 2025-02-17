
; @@@ void mikeos_draw_background(char *message1, char *message2, unsigned int colour);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_draw_background

_mikeos_draw_background:
	push	bp
	mov	bp, sp

	mov	ax, [bp + 4]
	mov	bx, [bp + 6]
	mov	cx, [bp + 8]

	mov	bp, os_draw_background
	call	bp

	pop	bp
	ret


; @@@ unsigned int mikeos_dialog_box(char *message1, char *message2, char *message3, unsigned int buttons);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_dialog_box

_mikeos_dialog_box:
	push	bp
	mov	bp, sp

	mov	ax, [bp + 4]
	mov	bx, [bp + 6]
	mov	cx, [bp + 8]
	mov	dx, [bp + 10]

	mov	bp, os_dialog_box
	call	bp

	pop	bp
	ret


; @@@ void mikeos_input_dialog(char *buf, char *message);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_input_dialog

_mikeos_input_dialog:
	push	bp
	mov	bp, sp

	mov	ax, [bp + 4]
	mov	bx, [bp + 6]

	mov	bp, os_input_dialog
	call	bp

	pop	bp
	ret

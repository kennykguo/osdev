
; @@@ int mikeos_list_dialog(char *list, char *message1, char *message2);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_list_dialog

_mikeos_list_dialog:
	push	bp
	mov	bp, sp

	mov	ax, [bp + 4]
	mov	bx, [bp + 6]
	mov	cx, [bp + 8]

	mov	bp, os_list_dialog
	call	bp

	jnc	.0
	mov	ax, -1		; failed
.0:
	pop	bp
	ret

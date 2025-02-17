
; @@@ int mikeos_create_file(char *filename);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_create_file

_mikeos_create_file:
	mov	bx, sp
	mov	ax, [ss:bx + 2]
	xor	dx, dx

	mov	bx, os_create_file
	call	bx

	jnc	.0
	dec	dx		; failed (dx = -1)
.0:
	mov	ax, dx
	ret

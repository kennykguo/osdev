
; @@@ int mikeos_file_exists(char *filename);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_file_exists

_mikeos_file_exists:
	mov	bx, sp
	mov	ax, [ss:bx + 2]
	xor	dx, dx

	mov	bx, os_file_exists
	call	bx

	jnc	.0
	dec	dx		; failed (dx = -1)
.0:
	mov	ax, dx
	ret


; @@@ int mikeos_rename_file(char *filename_new, char *filename_old);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_rename_file

_mikeos_rename_file:
	push	bp
	mov	bp, sp

	mov	bx, [bp + 4]
	mov	ax, [bp + 6]
	xor	dx, dx

	mov	bp, os_rename_file
	call	bp

	jnc	.0
	dec	dx		; failed (dx = -1)
.0:
	mov	ax, dx
	pop	bp
	ret

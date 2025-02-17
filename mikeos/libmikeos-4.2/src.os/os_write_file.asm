
; @@@ int mikeos_write_file(char *buf, char *filename, int size);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_write_file

_mikeos_write_file:
	push	bp
	mov	bp, sp

	mov	bx, [bp + 4]
	mov	ax, [bp + 6]
	mov	cx, [bp + 8]
	mov	dx, cx

				; XXX libmikeos does not check file size

	mov	bp, os_write_file
	call	bp

	jnc	.1
.0:
	mov	dx, -1		; failed
.1:
	mov	ax, dx	
	pop	bp
	ret

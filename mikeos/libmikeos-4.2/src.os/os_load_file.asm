
; @@@ int mikeos_load_file(char *buf, char *filename);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_load_file

_mikeos_load_file:
	push	bp
	mov	bp, sp

	mov	cx, [bp + 4]
	mov	ax, [bp + 6]

	mov	bx, os_load_file
	call	bx

	jnc	.0
	mov	bx, -1		; failed
.0:
	mov	ax, bx	
	pop	bp
	ret

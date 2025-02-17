
; @@@ int mikeos_get_file_size(char *filename);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_get_file_size

_mikeos_get_file_size:
	mov	bx, sp
	mov	ax, [ss:bx + 2]

	mov	bx, os_get_file_size
	call	bx

	jnc	.0
	mov	bx, -1		; failed
.0:
	mov	ax, bx
	ret

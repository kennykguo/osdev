
; @@@ unsigned int mikeos_get_random(unsigned int low, unsigned int high);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_get_random

_mikeos_get_random:
	push	bp
	mov	bp, sp

	mov	ax, [bp + 4]
	mov	bx, [bp + 6]

	mov	bp, os_get_random
	call	bp

	mov	ax, cx

	pop	bp
	ret

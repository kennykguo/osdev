
; @@@ long mikeos_long_int_negate(long value);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_long_int_negate

_mikeos_long_int_negate:
	push	bp
	mov	bp, sp

	mov	ax, [bp + 4]
	mov	dx, [bp + 6]

	mov	bx, os_long_int_negate
	call	bx

	mov	bx, dx

	pop	bp
	ret

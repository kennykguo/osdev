
; @@@ void mikeos_long_int_to_string(char *buf, unsigned long value, unsigned int base);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_long_int_to_string

_mikeos_long_int_to_string:
	push	bp
	mov	bp, sp
	push	di

	mov	di, [bp + 4]
	mov	ax, [bp + 6]
	mov	dx, [bp + 8]
	mov	bx, [bp + 10]

	mov	bp, os_long_int_to_string
	call	bp

	pop	di
	pop	bp
	ret

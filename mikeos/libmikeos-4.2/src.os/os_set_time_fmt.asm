
; @@@ void mikeos_set_time_fmt(unsigned char format);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_set_time_fmt

_mikeos_set_time_fmt:
	mov	bx, sp
	mov	al, [ss:bx + 2]

	mov	bx, os_set_time_fmt
	call	bx

	ret

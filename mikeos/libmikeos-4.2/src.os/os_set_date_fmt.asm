
; @@@ void mikeos_set_date_fmt(unsigned int format);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_set_date_fmt

_mikeos_set_date_fmt:
	mov	bx, sp
	mov	ax, [ss:bx + 2]

	mov	bx, os_set_date_fmt
	call	bx

	ret

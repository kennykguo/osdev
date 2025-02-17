
; @@@ unsigned int mikeos_bcd_to_int(unsigned char value);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_bcd_to_int

_mikeos_bcd_to_int:
	mov	bx, sp
	mov	al, [ss:bx + 2]

	mov	bx, os_bcd_to_int
	call	bx

	ret


; @@@ unsigned char mikeos_port_byte_in(unsigned short port);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_port_byte_in

_mikeos_port_byte_in:
	mov	bx, sp
	mov	dx, [ss:bx + 2]

	mov	bx, os_port_byte_in
	call	bx

	ret

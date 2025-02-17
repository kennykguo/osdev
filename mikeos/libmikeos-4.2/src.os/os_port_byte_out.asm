
; @@@ void mikeos_port_byte_out(unsigned short port, unsigned char data);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_port_byte_out

_mikeos_port_byte_out:
	push	bp
	mov	bp, sp

	mov	dx, [bp + 4]
	mov	al, [bp + 6]
	
	mov	bx, os_port_byte_out
	call	bx

	pop	bp
	ret

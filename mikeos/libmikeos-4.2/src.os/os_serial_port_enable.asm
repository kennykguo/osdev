
; @@@ void mikeos_serial_port_enable(int mode);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_serial_port_enable

_mikeos_serial_port_enable:
	mov	bx, sp
	mov	ax, [ss:bx + 2]

	mov	bx, os_serial_port_enable
	call	bx

	ret

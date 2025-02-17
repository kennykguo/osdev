
; @@@ int mikeos_send_via_serial(unsigned char data);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_send_via_serial

_mikeos_send_via_serial:
	mov	bx, sp
	mov	al, [ss:bx + 2]

	mov	bx, os_send_via_serial
	call	bx

	mov	al, ah
	cbw

	ret

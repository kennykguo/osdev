
; @@@ void mikeos_dump_registers(void);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_dump_registers

_mikeos_dump_registers:
	push	bp			; bx must be kept, use bp

	mov	bp, os_dump_registers
	call	bp

	pop	bp
	ret

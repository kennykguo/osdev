
; @@@ void mikeos_dump_string(char *string);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_dump_string

_mikeos_dump_string:
	mov	bx, sp
	push	si
	mov	si, [ss:bx + 2]

	mov	bx, os_dump_string
	call	bx

	pop	si
	ret


; @@@ void mikeos_sint_to_string(char *buf, int value);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_sint_to_string

_mikeos_sint_to_string:
	push	bp
	mov	bp, sp
	push	si
	push	di

	mov	ax, [bp + 6]
	mov	bx, os_sint_to_string
	call	bx

	mov	di, [bp + 4]
	mov	si, ax
	mov	bx, os_string_copy
	call	bx

	pop	di
	pop	si
	pop	bp
	ret

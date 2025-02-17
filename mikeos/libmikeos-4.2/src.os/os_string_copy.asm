
; @@@ void mikeos_string_copy(char *destination, char *source);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_string_copy

_mikeos_string_copy:
	push	bp
	mov	bp, sp
	push	si
	push	di

	mov	di, [bp + 4]
	mov	si, [bp + 6]

	mov	bx, os_string_copy
	call	bx

	pop	di
	pop	si
	pop	bp
	ret

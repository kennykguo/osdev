
; @@@ void mikeos_string_truncate(char *string, unsigned int length);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_string_truncate

_mikeos_string_truncate:
	push	bp
	mov	bp, sp
	push	si

	mov	si, [bp + 4]
	mov	ax, [bp + 6]

	mov	bx, os_string_truncate
	call	bx

	pop	si
	pop	bp
	ret

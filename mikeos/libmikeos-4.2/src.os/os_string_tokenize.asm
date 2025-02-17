
; @@@ char *mikeos_string_tokenize(char *string, char separator);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_string_tokenize

_mikeos_string_tokenize:
	push	bp
	mov	bp, sp
	push	si
	push	di

	mov	si, [bp + 4]
	mov	al, [bp + 6]

	mov	bx, os_string_tokenize
	call	bx

	mov	ax, di

	pop	di
	pop	si
	pop	bp
	ret

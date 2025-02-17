
; @@@ int mikeos_file_selector(char *filename);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_file_selector

_mikeos_file_selector:
	push	bp
	mov	bp, sp
	push	si
	push	di

	mov	bx, os_file_selector
	call	bx

	mov	si, ax
	mov	di, [bp + 4]

	mov	ax, -1

	or	si, si		; os_file_selector is cancelled -> error
	jz	.0
	or	di, di		; buf is NULL -> error
	jz	.0

	mov	bx, os_string_copy
	call	bx

	xor	ax, ax
.0:
	pop	di
	pop	si
	pop	bp
	ret

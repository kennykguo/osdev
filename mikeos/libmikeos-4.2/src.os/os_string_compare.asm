
; @@@ int mikeos_string_compare(char *string1, char *string2);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_string_compare

_mikeos_string_compare:
	push	bp
	mov	bp, sp
	push	si
	push	di

	mov	si, [bp + 4]
	mov	di, [bp + 6]

	mov	bx, os_string_compare
	call	bx

	mov	ax, 0		; xor destroys carry flag, use mov
	rcl	ax, 1

	pop	di
	pop	si
	pop	bp
	ret


; @@@ void mikeos_string_reverse(char *string);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_string_reverse

_mikeos_string_reverse:
	mov	bx, sp
	push	si
	mov	si, [ss:bx + 2]

	mov	bx, os_string_reverse
	call	bx

	pop	si
	ret


; @@@ void mikeos_string_join(char *destination, char *source1, char *source2);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_string_join

_mikeos_string_join:
	push	bp
	mov	bp, sp

	mov	cx, [bp + 4]
	mov	ax, [bp + 6]
	mov	bx, [bp + 8]

	mov	bp, os_string_join
	call	bp

	pop	bp
	ret

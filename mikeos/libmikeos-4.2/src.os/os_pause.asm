
; @@@ void mikeos_pause(unsigned int deciseconds);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_pause

_mikeos_pause:
	push	bp
	mov	bp, sp

	mov	ax, [bp + 4]

	mov	bx, os_pause
	call	bx

	pop	bp
	ret

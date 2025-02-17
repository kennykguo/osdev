
; @@@ int mikeos_run_basic(char *buf, int size);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_run_basic

_mikeos_run_basic:
	push	bp
	mov	bp, sp
	push	si		; os_run_basic don't save SI/DI?
	push	di

	mov	ax, [bp + 4]
	mov	bx, [bp + 6]

	mov	bp, os_run_basic
	call	bp

	pop	di
	pop	si
	pop	bp

	xor	ax, ax		; currently no value (zero)

	ret

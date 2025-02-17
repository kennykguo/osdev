
; @@@ unsigned int mikeos_wait_for_key(void);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_wait_for_key

_mikeos_wait_for_key:
	mov	bx, os_wait_for_key
	call	bx

	ret

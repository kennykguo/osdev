
; @@@ int mikeos_check_for_key(void);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_check_for_key

_mikeos_check_for_key:
	mov	bx, os_check_for_key
	call	bx

	xor	ah, ah

	ret

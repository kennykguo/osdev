
; @@@ int mikeos_get_api_version(void);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_get_api_version

_mikeos_get_api_version:
	mov	bx, os_get_api_version
	call	bx

	xor	ah, ah

	ret

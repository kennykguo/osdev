
; @@@ int mikeos_get_via_serial(unsigned char *data);

%include "os_vector.inc"

	section .text
	use16
	extern	__heap_top
	global	_mikeos_get_via_serial

_mikeos_get_via_serial:
	push	bp
	mov	bp, sp
	
	mov	bx, os_get_via_serial
	call	bx

	mov	bx, [bp + 4]	; get pointer on the stack
	or	bx, bx
	jz	.1		; NULL pointer, do nothing
	cmp	bx, __heap_top	; judge DS/SS
	jb	.0
	db	0x36		; SS prefix
.0:
	mov	[bx], al
.1:
	mov	al, ah
	cbw

	pop	bp
	ret


; @@@ void mikeos_get_cursor_pos(unsigned char *column, unsigned char *row);

%include "os_vector.inc"

	section .text
	use16
	extern	__heap_top
	global	_mikeos_get_cursor_pos

_mikeos_get_cursor_pos:
	push	bp
	mov	bp, sp

	mov	bx, os_get_cursor_pos
	call	bx

	mov	bx, [bp + 4]	; get pointer on the stack
	or	bx, bx
	jz	.1		; NULL pointer, do nothing
	cmp	bx, __heap_top	; judge DS/SS
	jb	.0
	db	0x36		; SS prefix
.0:
	mov	[bx], dl
.1:
	mov	bx, [bp + 6]	; get pointer on the stack
	or	bx, bx
	jz	.3		; NULL pointer, do nothing
	cmp	bx, __heap_top	; judge DS/SS
	jb	.2
	db	0x36		; SS prefix
.2:
	mov	[bx], dh
.3:
	pop	bp
	ret

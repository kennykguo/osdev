
; @@@ void mikeos_draw_block(unsigned char column, unsigned char row, unsigned char width, unsigned char height, unsigned char colour);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_draw_block

_mikeos_draw_block:
	push	bp
	mov	bp, sp
	push	si
	push	di

	mov	dl, [bp + 4]
	movzx	ax, [bp + 6]
	movzx	si, [bp + 8]
	movzx	di, [bp + 10]
	mov	bl, [bp + 12]

	mov	dh, al		; DH: start Y pos
	add	di, ax		; height -> finish Y pos

	mov	bp, os_draw_block
	call	bp

	pop	di
	pop	si
	pop	bp
	ret

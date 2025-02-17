
	section	.text
	use16

	; (signed, unsigned) bx:ax <<= di;

	global	lsll
	global	lslul
lsll:
lslul:
	mov	cx, di
	cmp	cx, 32
	jb	lsl_main

	sub	ax, ax
	mov	bx, ax
	ret

lsl_main:
	push	bx
	push	ax

	pop	eax

	shl	eax, cl

	push	eax

	pop	ax
	pop	bx
	ret

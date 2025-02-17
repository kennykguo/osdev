
	section	.text
	use16

	; (unsigned) bx:ax >>= dl;

	global	lsrul
lsrul:
	mov	cx, di
	cmp	cx, 32
	jb	lsrul_main

	sub	ax, ax
	mov	bx, ax
	ret

lsrul_main:
	push	bx
	push	ax

	pop	eax

	shr	eax, cl

	push	eax

	pop	ax
	pop	bx
	ret

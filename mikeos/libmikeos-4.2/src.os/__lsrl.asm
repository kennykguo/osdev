
	section	.text
	use16

	; (signed) bx:ax >>= di;

	global	lsrl
lsrl:
	mov	cx, di
	cmp	cx, 32
	jb	lsrl_main

	mov	ax, 0xffff
	mov	bx, ax
	ret

lsrl_main:
	push	bx
	push	ax

	pop	eax

	sar	eax, cl

	push	eax

	pop	ax
	pop	bx
	ret

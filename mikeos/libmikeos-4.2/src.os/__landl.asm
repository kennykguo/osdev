
	section	.text
	use16

	extern	__heap_top

	; (signed, unsigned) bx:ax &= *(signed, unsigned *)di

	global	landl
	global	landul
landl:
landul:
	push	bx
	push	ax

	pop	eax

	cmp	di, __heap_top	; judge DS/SS
	jb	.0
	db	0x36		; SS prefix
.0:
	and	eax, [di]

	push	eax

	pop	ax
	pop	bx
	ret

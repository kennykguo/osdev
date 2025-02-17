
	section	.text
	use16

	extern	__heap_top

	; (signed, unsigned) bx:ax += *(signed, unsigned *)di

	global	lmull
	global	lmulul
lmull:
lmulul:
	push	bx
	push	ax

	pop	eax

	cmp	di, __heap_top	; judge DS/SS
	jb	.0
	db	0x36		; SS prefix
.0:
	imul	dword[di]

	push	eax

	pop	ax
	pop	bx
	ret

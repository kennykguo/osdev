
	section	.text
	use16

	extern	__heap_top

	; (unsigned) bx:ax / *di = bx:ax .. edx

	global	ldivul
ldivul:
	push	bx
	push	ax

	pop	eax
	sub	edx, edx

	cmp	di, __heap_top	; judge DS/SS
	jb	.0
	db	0x36		; SS prefix
.0:
	div	dword [di]

	push	eax

	pop	ax
	pop	bx
	ret

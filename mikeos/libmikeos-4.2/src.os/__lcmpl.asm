
	section	.text
	use16

	extern	__heap_top

	; (signed, unsigned) bx:ax == *(signed, unsigned *)di

	global	lcmpl
	global	lcmpul
lcmpl:
lcmpul:
	push	bx
	push	ax

	pop	eax

	cmp	di, __heap_top	; judge DS/SS
	jb	.0
	db	0x36		; SS prefix
.0:
	cmp	eax, [di]

	ret


	section	.text
	use16

	extern	__heap_top

	; *(signed, unsigned *)bx++

	global	lincl
	global	lincul
lincl:
lincul:
	cmp	bx, __heap_top	; judge DS/SS
	jb	.0
	db	0x36		; SS prefix
.0:
	inc	dword [bx]
	ret

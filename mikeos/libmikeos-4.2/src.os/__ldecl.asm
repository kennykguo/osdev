
	section	.text
	use16

	extern	__heap_top

	; *(signed, unsigned *)bx--

	global	ldecl
	global	ldecul
ldecl:
ldecul:
	cmp	bx, __heap_top	; judge DS/SS
	jb	.0
	db	0x36		; SS prefix
.0:
	dec	dword [bx]
	ret

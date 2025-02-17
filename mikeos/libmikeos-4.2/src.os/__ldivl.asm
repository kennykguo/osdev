
	section	.text
	use16

	extern	__heap_top

	; (signed) bx:ax / *di = bx:ax .. edx

	global	ldivl
ldivl:
	push	bx
	push	ax

	pop	eax
	cdq

	cmp	di, __heap_top	; judge DS/SS
	jb	.0
	db	0x36		; SS prefix
.0:
	idiv	dword [di]

	push	eax

	pop	ax
	pop	bx
	ret

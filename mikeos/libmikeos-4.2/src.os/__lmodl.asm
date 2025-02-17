
	section	.text
	use16

	extern	__heap_top

	; (signed) bx:ax / *di = eax .. edx
	; bx:ax <- edx

	global	lmodl
lmodl:
	push	bx
	push	ax

	pop	eax
	cdq

	cmp	di, __heap_top	; judge DS/SS
	jb	.0
	db	0x36		; SS prefix
.0:
	idiv	dword [di]

	push	edx

	pop	ax
	pop	bx
	ret

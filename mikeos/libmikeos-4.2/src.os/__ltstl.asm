
	section	.text
	use16

	; check bx:ax is signed, unsigned, or zero

	global	ltstl
	global	ltstul
ltstl:
ltstul:
	push	bx
	push	ax

	pop	eax
	test	eax, eax

	ret

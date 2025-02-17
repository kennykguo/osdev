
	section	.text
	use16

	; (signed, unsigned) bx:ax = -bx:ax

	global	lnegl
	global	lnegul
lnegl:
lnegul:
	push	bx
	push	ax

	pop	eax

	neg	eax

	push	eax

	pop	ax
	pop	bx
	ret

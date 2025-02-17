
	section	.text
	use16

	; (signed, unsigned) bx:ax = ~bx:ax

	global	lcoml
	global	lcomul
lcoml:
lcomul:
	push	bx
	push	ax

	pop	eax

	not	eax

	push	eax

	pop	ax
	pop	bx
	ret

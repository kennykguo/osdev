
	section	.text
	use16

	; (signed, unsigned) ax <<= cl

	global	isl
	global	islu
isl:
islu:
	mov	cl, bl
	shl	ax, cl
	ret

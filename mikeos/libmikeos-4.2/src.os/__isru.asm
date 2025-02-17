
	section	.text
	use16

	; (unsigned) ax >>= cl

	global	isru
isru:
	mov	cl, bl
	shr	ax, cl
	ret

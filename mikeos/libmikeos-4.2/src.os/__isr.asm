
	section	.text
	use16

	; (signed) ax >>= cl

	global	isr
isr:
	mov	cl, bl
	sar	ax, cl
	ret

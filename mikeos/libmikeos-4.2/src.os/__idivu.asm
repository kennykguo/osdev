
	section	.text
	use16

	; (unsigned) ax / bx = ax .. dx

	global	idiv_u
idiv_u:
	sub	dx, dx
	div	bx
	ret

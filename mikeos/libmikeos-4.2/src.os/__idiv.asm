
	section	.text
	use16

	; (signed) ax / bx = ax .. dx

	global	idiv_
idiv_:
	cwd
	idiv	bx
	ret

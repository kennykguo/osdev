
	section	.text
	use16

	; (signed) ax / bx = ax .. dx
	; ax <- dx

	global	imod
imod:
	cwd
	idiv	bx
	mov	ax, dx
	ret

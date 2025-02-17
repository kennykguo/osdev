
	section	.text
	use16

	; (signed, unsigned) ax * bx = ax
	
	global	imul_
	global	imul_u
imul_:
imul_u:
	imul	bx
	ret

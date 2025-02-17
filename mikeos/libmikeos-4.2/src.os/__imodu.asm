
	section	.text
	use16

	; (unsigned) ax / bx = ax .. dx
	; ax <- dx

	global	imodu
imodu:
	sub	dx, dx
	div	bx
	mov	ax, dx
	ret

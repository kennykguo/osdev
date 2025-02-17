
; @@@ void mikeos_string_parse(char *string, char **token1, char **token2, char**token3, char **token4);

%include "os_vector.inc"

	section .text
	use16
	extern	__heap_top
	global	_mikeos_string_parse

_mikeos_string_parse:
	push	bp
	mov	bp, sp
	push	si

	mov	si, [bp + 4]
	mov	bx, os_string_parse
	call	bx

	mov	si, [bp + 6]	; get pointer on the stack
	or	si, si
	jz	.1		; NULL pointer, do nothing
	cmp	si, __heap_top	; judge DS/SS
	jb	.0
	db	0x36		; SS prefix
.0:
	mov	[si], ax
.1:
	mov	si, [bp + 8]	; get pointer on the stack
	or	si, si
	jz	.3		; NULL pointer, do nothing
	cmp	si, __heap_top	; judge DS/SS
	jb	.2
	db	0x36		; SS prefix
.2:
	mov	[si], bx
.3:
	mov	si, [bp + 10]	; get pointer on the stack
	or	si, si
	jz	.5		; NULL pointer, do nothing
	cmp	si, __heap_top	; judge DS/SS
	jb	.4
	db	0x36		; SS prefix
.4:
	mov	[si], cx
.5:
	mov	si, [bp + 12]	; get pointer on the stack
	or	si, si
	jz	.7		; NULL pointer, do nothing
	cmp	si, __heap_top	; judge DS/SS
	jb	.6
	db	0x36		; SS prefix
.6:
	mov	[si], dx
.7:
	pop	si
	pop	bp
	ret


; @@@ int mikeos_string_strincmp(char *string1, char *string2, unsigned int length);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_string_strincmp

_mikeos_string_strincmp:
	push	bp
	mov	bp, sp
	push	si
	push	di

	mov	si, [bp + 4]
	mov	di, [bp + 6]
	mov	cx, [bp + 8]

	xor	ch, ch		; XXX libmikeos uses "unsigned int" for
				; XXX string length, but this MikeOS API
				; XXX requires "unsigned char".
				; XXX This is libmikeos' extension to
				; XXX support future release of MikeOS,
				; XXX and user have to use 0-255 value for
				; XXX current release.

	mov	bx, os_string_strincmp
	call	bx

	mov	ax, 0		; xor destroys carry flag, use mov
	rcl	ax, 1

	pop	di
	pop	si
	pop	bp
	ret

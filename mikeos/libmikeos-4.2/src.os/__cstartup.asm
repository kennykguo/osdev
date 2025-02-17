
	section .text
	use16

	extern	__edata
	extern	__end
	extern	_MikeMain

	global	___cstartup
	global	_exit
	global	__exit

___cstartup:
	mov	ax, cs			; CS = DS
	mov	ds, ax
					; use current SS (;= CS)
	mov	ax, sp
	mov	[___saved_sp], ax	; save current SP

bss_clear:				; clear BSS area
	mov	bx, __edata
	mov	ax, __end
bss_loop:
	cmp	ax, bx
	jae	bss_clear_end

	mov	byte [bx], 0
	inc	bx
	jmp	bss_loop
bss_clear_end:

run_mikemain:
	push	si
	call	_MikeMain		; MikeMain(argument)
	inc	sp
	inc	sp

_exit:
	mov	bx, [___cleanup]	; check __cleanup != NULL
	test	bx, bx
	je	__exit

	push	ax			; (*__cleanup)(retval)
	call	bx
	inc	sp
	inc	sp

__exit:					; exit, but no cleanup
	mov	ax, [___saved_sp]	; restore original SP
	mov	sp, ax

	ret				; return to caller


	section	.data

	global	___cleanup

___saved_sp:
	dw	0
___cleanup:
	dw	0			; void (*__cleanup)() = 0;

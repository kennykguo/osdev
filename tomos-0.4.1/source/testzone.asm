; =================================================================
; TomOS -- The Mike Operating System kernel
; Copyright (C) 2006 - 2008 TomOS Developers -- see doc/LICENSE.TXT
;
; TEST ZONE -- A place for quickly trying out new code
; =================================================================


test_zone:
	pusha

	mov si, .msg
	call _os_print_string

	popa
	ret


	.msg db "Try out code without altering the OS in testzone.asm!", 10, 13, 0


; =================================================================


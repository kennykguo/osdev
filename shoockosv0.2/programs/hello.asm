	BITS 16
	%INCLUDE "os.inc"
	ORG 32768


start:
	mov si, message
	call os_print_string

	call os_wait_for_key

	ret


	message	db 'Hello, world!', 13, 10, 0

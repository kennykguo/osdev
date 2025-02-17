
; @@@ void mikeos_get_file_list(char *buf);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_get_file_list

_mikeos_get_file_list:
	mov	bx, sp
	mov	ax, [ss:bx + 2]

	mov	bx, os_get_file_list
	call	bx

	ret

; ==================================================================
; MikeOS -- The Mike Operating System kernel
; Copyright (C) 2006 - 2009 MikeOS Developers -- see doc/LICENSE.TXT
;
; COMMAND LINE INTERFACE
; ==================================================================

os_command_line:
	call os_clear_screen

	mov si, version_msg
	call os_print_string
	mov si, help_text
	call os_print_string

get_cmd:
	mov si, prompt			; Main loop; prompt for input
	call os_print_string

	mov ax, input			; Get command string from user
	call os_input_string

	call os_print_newline

	mov ax, input			; Remove trailing spaces
	call os_string_chomp

	mov si, input			; If just enter pressed, prompt again
	cmp byte [si], 0
	je get_cmd

	mov si, input			; Convert to uppercase for comparison
	call os_string_uppercase


	mov si, input			; 'EXIT' entered?
	mov di, exit_string
	call os_string_compare
	jc near exit

	mov si, input			; 'HELP' entered?
	mov di, help_string
	call os_string_compare
	jc near print_help

	mov si, input			; 'CLS' entered?
	mov di, cls_string
	call os_string_compare
	jc near clear_screen

	mov si, input			; 'LS' entered?
	mov di, ls_string
	call os_string_compare
	jc near list_directory
	
	mov si, input			; 'TEST' entered?
	mov di, test_string
	call os_string_compare
	jc near test_c

	mov si, input			; 'VER' entered?
	mov di, ver_string
	call os_string_compare
	jc near print_ver

	mov si, input			; 'TIME' entered?
	mov di, time_string
	call os_string_compare
	jc near print_time

	mov si, input			; 'INFO' entered?
 	mov di, info_string
	call os_string_compare
	jc near print_info

	mov si, input
	mov di, reboot_string
	call os_string_compare
	jc near reboot

	mov si, input			; 'DATE' entered?
	mov di, date_string
	call os_string_compare
	jc near print_date


	mov si, input			; 'CAT' entered?
	mov di, cat_string
	mov cl, 3
	call os_string_strincmp
	jc near cat_file



	; If the user hasn't entered any of the above commands, then we
	; need to check if an executable filename (.BIN) was entered...

	mov si, input			; User entered dot in filename?
	mov al, '.'
	call os_find_char_in_string
	cmp ax, 0
	je suffix

	jmp full_name

suffix:
	mov ax, input
	call os_string_length

	mov si, input
	add si, ax			; Move to end of input string

	mov byte [si], '.'
	mov byte [si+1], 'B'
	mov byte [si+2], 'I'
	mov byte [si+3], 'N'
	mov byte [si+4], 0		; Zero-terminate string


full_name:
	mov si, input			; User tried to execute kernel?
	mov di, kern_file_string
	call os_string_compare
	jc near kern_warning

	mov ax, input			; If not, try to load specified program
	mov bx, 0
	mov cx, 32768
	call os_load_file
	jc fail				; Skip if program not found

	mov ax, 0			; Clear all registers
	mov bx, 0
	mov cx, 0
	mov dx, 0
	mov si, 0
	mov di, 0

	call 32768			; Call the external program

	jmp get_cmd			; When program has finished, start again

fail:
	mov si, invalid_msg
	call os_print_string

	jmp get_cmd


; ------------------------------------------------------------------

print_help:
	mov si, help_text
	call os_print_string
	jmp get_cmd


; ------------------------------------------------------------------

clear_screen:
	call os_clear_screen
	jmp get_cmd


; ------------------------------------------------------------------

reboot:
	mov        al, 0feh 
	out        64h, al


; ------------------------------------------------------------------
print_time:
	mov bx, tmp_string
	call os_get_time_string
	mov si, bx
	call os_print_string
	call os_print_newline
	jmp get_cmd


; ------------------------------------------------------------------


print_info:
	mov si, info_msg
	call os_print_string
	call os_print_newline
	mov si, info_msg2
	call os_print_string
	jmp get_cmd

; ------------------------------------------------------------------
print_date:
	mov bx, tmp_string
	call os_get_date_string
	mov si, bx
	call os_print_string
	call os_print_newline
	jmp get_cmd


; ------------------------------------------------------------------

print_ver:
	mov si, version_msg
	call os_print_string
	jmp get_cmd


; ------------------------------------------------------------------

test_c:
	mov ax, test_file
	call os_create_file
	jmp get_cmd


; ------------------------------------------------------------------

kern_warning:
	mov si, kern_warn_msg
	call os_print_string
	jmp get_cmd


; ------------------------------------------------------------------

list_directory:
	mov cx,	0			; Counter

	mov ax, dirlist			; Get list of files on disk
	call os_get_file_list

	mov si, dirlist
	mov ah, 0Eh			; BIOS teletype function

.repeat:
	lodsb				; Start printing filenames
	cmp al, 0			; Quit if end of string
	je .done

	cmp al, ','			; If comma in list string, don't print it
	jne .nonewline
	pusha
	call os_print_newline		; But print a newline instead
	popa
	jmp .repeat

.nonewline:
	int 10h
	jmp .repeat

.done:
	call os_print_newline
	jmp get_cmd


; ------------------------------------------------------------------

cat_file:
	mov si, input
	call os_string_parse
	cmp bx, 0			; Was a filename provided?
	jne .filename_provided

	mov si, nofilename_msg		; If not, show error message
	call os_print_string
	jmp get_cmd

.filename_provided:
	mov ax, bx

	call os_file_exists		; Check if file exists
	jc .not_found

	mov cx, 32768			; Load file into second 32K
	call os_load_file

	mov si, 32768
	mov ah, 0Eh			; int 10h teletype function
.loop:
	lodsb				; Get byte from loaded file

	cmp al, 0Ah			; Move to start of line if we get a newline char
	jne .not_newline

	call os_get_cursor_pos
	mov dl, 0
	call os_move_cursor

.not_newline:
	int 10h				; Display it
	dec bx				; Count down file size
	cmp bx, 0			; End of file?
	jne .loop

	jmp get_cmd

.not_found:
	mov si, notfound_msg
	call os_print_string
	jmp get_cmd



; ------------------------------------------------------------------

exit:
	ret


; ------------------------------------------------------------------

	input			times 255 db 0
	dirlist			times 255 db 0
	tmp_string		times 15 db 0

	prompt			db '> ', 0
	help_text		db 'Dostepne komendy: LS, CAT, CLS, HELP, TIME, DATE, VER, EXIT, INFO, REBOOT', 13, 10, 0
	invalid_msg		db 'Podana komenda lub program nie istnieje!', 13, 10, 0
	nofilename_msg		db 'No filename specified', 13, 10, 0
	notfound_msg		db 'Plik nie istnieje!', 13, 10, 0
	version_msg		db 'ShoockOS ', SHOOCKOS_VER, 13, 10, 0
	info_msg		db 'ShoockOS jest 16-bitowym systemem.', 13, 10, 0
	info_msg2		db 'Bazuje on na MikeOS.', 13, 10, 0

	exit_string		db 'EXIT', 0
	help_string		db 'HELP', 0
	cls_string		db 'CLS', 0
	ls_string		db 'LS', 0
	time_string		db 'TIME', 0
	date_string		db 'DATE', 0
	ver_string		db 'VER', 0
	cat_string		db 'CAT', 0
	info_string		db 'INFO', 0
	reboot_string		db 'REBOOT', 0
	test_string		db 'TEST', 0

	test_file		db 'TEST.TXT', 0

	kern_file_string	db 'KERNEL.BIN', 0
	kern_warn_msg		db 'Nie mozna zaladowac pliku KERNEL.BIN', 13, 10, 0


; ==================================================================


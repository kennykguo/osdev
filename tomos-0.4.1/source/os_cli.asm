; =================================================================
; TomOS -- The Mike Operating System kernel
; Copyright (C) 2006 - 2008 TomOS Developers -- see doc/LICENSE.TXT
;
; COMMAND LINE INTERFACE
; ================================================================


os_command_line:
	call _os_clear_screen

	mov si, .default_prompt			; Set up default prompt
	mov di, .prompt
	call _os_string_copy

	mov si, .version_msg
	call _os_print_string
	mov si, .help_text
	call _os_print_string

        xor ax, ax
        mov word [cmdln + far_address.offset], ax
        mov word [cmdln + far_address.segment], ax
        mov word [arg_count], ax

.more:

        cmp word [cmdln + far_address.segment], 0
        jz .no_free_cmd
            call _os_cli_remove_command_line
        .no_free_cmd:
        cmp word [cmdln + far_address.segment], 0

	mov si, .prompt				; Prompt for input
	call _os_print_string

	mov dx, .input				; Get string from user
	call _os_input_string

	call _os_print_newline

	mov bx, .input				; Remove trailing spaces
	call _os_string_chomp

	mov si, .input				; If just enter pressed, prompt again
	cmp byte [si], 0
	je .more

	mov si, .input				; Convert to uppercase for comparison
	call _os_string_uppercase

        mov si, .input
        call _os_cli_create_command_line
        test ax, ax
        jz .exit

        mov di, .input
        mov si , word [cmdln + far_address.offset]

        call _os_string_copy

	mov si, .input				; 'EXIT' entered?
	mov di, .exit_string
	call _os_string_compare
	jc near .exit

	mov si, .input				; 'HELP' entered?
	mov di, .help_string
	call _os_string_compare
	jc near .print_help

	mov si, .input				; 'CLS' entered?
	mov di, .cls_string
	call _os_string_compare
	jc near .clear_screen

	mov si, .input				; 'PROMPT' entered?
	mov di, .chprompt_string
	call _os_string_compare
	jc near .change_prompt

	mov si, .input				; 'VER' entered?
	mov di, .ver_string
	call _os_string_compare
	jc near .print_ver

	mov si, .input				; 'VOL' entered?
	mov di, .vol_string
	call _os_string_compare
	jc near .print_vol

	mov si, .input				; 'TIME' entered?
	mov di, .time_string
	call _os_string_compare
	jc near .print_time

	mov si, .input				; 'DATE' entered?
	mov di, .date_string
	call _os_string_compare
	jc near .print_date

	mov si, .input				; 'TEST' entered?
	mov di, .test_string
	call _os_string_compare
	jc near .run_test

	mov si, .input				; 'TEST' entered?
	mov di, .free_string
	call _os_string_compare
	jc near .print_free



	mov si, .input				; User entered dot in filename?
	mov dl, '.'
	call _os_string_find_char
	cmp ax, 0
	je .notadot				; If not, see if it's 11 chars
	dec ax
	jmp .padout				; Otherwise, make sure it's padded out

.notadot:
	mov bx, .input				; User entered full 11-char filename?
	call _os_string_length

	cmp ax, 11
	jge .full_name

.padout:
	mov si, .input				; Pad with spaces and 'BIN'
	add si, ax

.bitmore:
	cmp ax, 8
	jge .suffix
	mov byte [si], ' '
	inc si
	inc ax
	jmp .bitmore


.suffix:
	mov word [si], 'BI'   ; add 'BIN',0 to command
        sub si, -2
	mov word [si], 004Eh  


.full_name:
	mov si, .input				; User tried to execute kernel?
	mov di, .kern_file_string
	call _os_string_compare
	jc near .kern_warning


	mov si, .input				; If not, load specified program
	mov bx, 0
	mov di, 100h
	call _os_load_file
	jc .fail
	mov bx, 0				; Don't clear the screen!
	call _os_execute_program

	jmp .more

.fail:
	mov si, .not_found_msg
	call _os_print_string

	jmp .more


.change_prompt:
	mov si, .chprompt_msg
	call _os_print_string

	mov dx, .prompt
	call _os_input_string

	call _os_print_newline

	jmp .more


.print_help:
	mov si, .help_text
	call _os_print_string
	jmp .more


.clear_screen:
	call _os_clear_screen
	jmp .more


.print_time:
	mov bx, .tmpstring
        call _os_get_time_string
	mov si, bx
	call _os_print_string
	call _os_print_newline
	jmp .more


.print_date:
	mov bx, .tmpstring
	call _os_date_get_string
	mov si, bx
	call _os_print_string
	call _os_print_newline
	jmp .more


.run_test:
	call test_zone
	jmp .more

.print_free:
       call  _mm_get_free_memory_size

       mov dx, ax
       mov bx, .tmp

       call _os_int_to_string

       mov si, .free_msg_1
       call _os_print_string

       mov si, .tmp
       call _os_print_string

       call _os_print_space

       mov si, .free_msg_2
       call _os_print_string
       
       jmp .more


.print_ver:
	mov si, .version_msg
	call _os_print_string
	jmp .more




.print_vol:
	mov ax, 0				; Read floppy sector 0
	call _os_int_l2hts			; Convert

	lea si, [os_buffer]			; Set ES:BX to point to general OS buffer
	mov bx, ds
	mov es, bx
	mov bx, si

	mov ah, 0x02				; Params for int 13h: read floppy sectors
	mov al, 1				; Just read one sector

	stc
	int 13h					; Call BIOS

	mov si, .vol_label_string
	call _os_print_string


	mov si, os_buffer+0x2B			; Point SI at label string

	mov cx, 11				; Counter for 11 chars

	mov ah, 0Eh				; BIOS print char function
.more_label_string:
	mov al, [si]
	int 10h
	inc si
	dec cx					; Count down the 11 chars
	jne .more_label_string


	call _os_print_newline


	mov si, .vol_fs_string
	call _os_print_string

	mov si, os_buffer+0x36			; Point SI at filesystem string

	mov cx, 8				; Counter for 11 chars

	mov ah, 0Eh				; BIOS print char function
.more_fs_string:
	mov al, [si]
	int 10h
	inc si
	dec cx					; Count down for the 11 chars
	jne .more_fs_string


	call _os_print_newline

	jmp .more




.kern_warning:
	mov si, .kern_warn_msg
	call _os_print_string
	jmp .more





.exit:
        cmp word [cmdln + far_address.segment], 0
        jnz .now_exit
            call _os_cli_remove_command_line
        .now_exit:
	ret



	.input		times 255 db 0
	.dirlist	times 255 db 0
	.prompt		times 255 db 0
	.tmpstring	times 15 db 0
        .tmp            times 32 db 1

	.default_prompt		db '> ', 0
	.help_text		db 'Inbuilt commands: VOL, CLS, HELP, PROMPT, TIME, DATE, VER, TEST, FREE, EXIT', 13, 10, 0
	.not_found_msg		db 'No such command or program', 13, 10, 0
	.chprompt_msg		db 'Enter a new prompt:', 13, 10, 0
	.version_msg		db 'TomOS ', TOMOS_VER, 13, 10, 0
	.free_msg_1		db 'Free memory size: ', 0
	.free_msg_2		db 'bytes', 13, 10, 0

	.exit_string		db 'EXIT', 0
	.help_string		db 'HELP', 0
	.cls_string		db 'CLS', 0
	.chprompt_string	db 'PROMPT', 0
	.time_string		db 'TIME', 0
	.date_string		db 'DATE', 0
	.test_string		db 'TEST', 0
	.ver_string		db 'VER', 0
	.vol_string		db 'VOL', 0
	.free_string		db 'FREE', 0

	.vol_label_string	db 'Volume label: ', 0
	.vol_fs_string		db 'Volume filesystem: ', 0

	.kern_file_string	db KERNELNAME_FAT12, 0
	.kern_warn_msg		db 'Cannot execute kernel file!', 13, 10, 0

; ----------------------------------------------------
; os_cli_create_command_line
;
; DS:SI -  input passed by srer
; ES:DI -  returned double zero terminated string zero terminated strings

_os_cli_create_command_line:
    push bx
    push di
    push es
    push si
    push ds
 
    mov bx, si
    call _os_string_length

    mov dx, ax
    sub dx, -2

    mov word [arg_count], 0

    call _os_mm_alloc
    test ax, ax
    jz .no_memory

    mov word [cmdln + far_address.offset], di
    mov ax, es
    mov word [cmdln + far_address.segment], ax
   
    .loop_arg:
      cmp byte [si], 0
      jz .exit

      mov dl, ' ' 
      call _os_string_not_find_char
      test ax, ax
      jz .exit

      dec ax

      add si, ax

      mov dl , ' ' 
      call _os_string_find_char
      test ax, ax
      jnz .found_space

      mov dl , 0
      call _os_string_find_char
      test ax, ax
      jz .exit

      .found_space:


      mov cx, ax
 
      call _os_memory_copy
      add di, cx
      mov byte [di],0
      inc di

      inc word  [arg_count]

      add si,  cx
  
      jmp .loop_arg

    .no_memory:
    pop ds
    pop si
    pop es
    pop di
    pop bx

    ret

    .exit:

    mov byte [di + 1], 0
  
    mov ax, word  [arg_count]

    pop ds
    pop si
    pop es
    pop di
    pop bx

    ret

cmdln times 2 dw 0
arg_count dw 0

; ---------------------------------------------------------
; os_cli_remove_command_line
;
; ES:DI - command line created by os_cli_remove_command_line

_os_cli_remove_command_line:
 
    push di
    push es

    mov di, word [cmdln + far_address.offset]
    push word [cmdln + far_address.segment]
    pop es

    call _os_mm_free

    xor ax, ax
    mov word [cmdln + far_address.offset], ax
    mov word [cmdln + far_address.segment], ax

    mov word [arg_count], ax

    pop es
    pop di
    ret

; ------------------------------------------------------------
; @proc os_command_line_get
; get arguments passed to the program
;
; @param_in Nothing
; @param_out AX - number of arguments
; @param_out ES:DI - command line
; @section "command line"
; @note Format of the command line is arg1[byte 0]arg2[byte 0]argN[byte0][byte 0]

_os_command_line_get:
    
    mov di , word [cmdln + far_address.offset]
    mov ax , word [cmdln + far_address.segment]
    mov es, ax

    mov ax ,word [arg_count]
    ret

; =================================================================


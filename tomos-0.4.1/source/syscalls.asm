; =================================================================
; TomOS -- The Tom Operating System kernel
; Copyright (C) 2006 - 2008 TomOS Developers -- see doc/LICENSE.TXT
;
; SYSTEM CALL SECTION -- Accessible to user programs
; =================================================================

%include "tomos.inc"

; @description File contains system calls derived from MikeOS-2.0.0

; -----------------------------------------------------------------
; @proc os_char_uppercase
; change a character to a capital
;
; @param_in DL - character to change
; @param_out AL - changed 
; @section "char"

_os_char_uppercase:
    cmp dl, 'a'
    jb .exit
    cmp dl, 'z'
    ja .exit
    add dl ,'A'-'a'
    .exit:
    mov al, dl
    ret

; -----------------------------------------------------------------
; @proc os_char_lowwercase
; change a character to a small letter
;
; @param_in DL - character to change
; @param_out AL - changed 
; @section "char"

_os_char_lowwercase:
    cmp dl, 'A'
    jb .exit
    cmp dl, 'Z'
    ja .exit
    add dl ,'a'-'A'
    .exit:
    mov al, dl
    ret


; -----------------------------------------------------------------
; @proc os_print_string
;  Displays text
; 
; @param_in  SI - message location (zero-terminated string)
; @param_out  Nothing (registers preserved)
; @section "print"


_os_print_string:
	pusha

	mov ah, 0Eh		; int 10h teletype function
                                ; Some BIOS will change DX and/or BP

.repeat:
	lodsb			; Get char from string
	cmp al, 0
	je .done		; If char is zero, end of string

	int 10h			; Otherwise, print it
	jmp .repeat

.done:
	popa
	ret


; -----------------------------------------------------------------
; @proc os_cursor_move
;  Moves cursor in text mode
;
; @param_in DH - row
; @param_in DL - column
; @param_out Nothing (registers preserved)
; @section "cursor"

_os_cursor_move:
	pusha

	mov bh, 0
	mov ah, 2
	int 10h

	popa
	ret


; -----------------------------------------------------------------
; @proc os_cursor_get_pos
; Return position of text cursor
;
; @param_in Nothing
; @param_out  DH - row
; @param_out  DL - column
; @section "cursor"

_os_cursor_get_pos:
	pusha

	mov bh, 0
	mov ah, 3
	int 10h

	mov [.tmp], dx
	popa
	mov dx, [.tmp]
	ret


	.tmp dw 0


; -----------------------------------------------------------------
; @proc os_cursor_show
;  Turns on cursor in text mode
;
; @param_in  Nothing
; @param_out  Nothing
; @section "cursor"

_os_cursor_show:
	pusha

	mov ch, 6
	mov cl, 7
	mov ah, 1
	mov al, 3
	int 10h

	popa
	ret


; -----------------------------------------------------------------
; @proc os_cursor_hide
; Turns off cursor in text mode
;
; @param_in  Nothing
; @param_out  Nothing
; @section "cursor"

_os_cursor_hide:
	pusha

	mov ch, 32
	mov ah, 1
	mov al, 3			; Must be video mode for buggy BIOSes!
	int 10h

	popa
	ret


; -----------------------------------------------------------------
; @proc os_draw_block
; Render block of specified colour
;
; @param_in BL - colour
; @param_in DL - start X pos
; @param_in DH - start Y pos
; @param_in SI - width
; @param_in DI - finish Y pos
; @param_out Nothing
; @section "drawing"

_os_draw_block:
	pusha

.more:
	call _os_cursor_move		; Move to block starting position

	mov ah, 09h			; Draw colour section
	mov bh, 0
	mov cx, si
	mov al, ' '
	int 10h

	inc dh				; Get ready for next line

	mov ax, 0
	mov al, dh			; Get current Y position into DL
	cmp ax, di			; Reached finishing point (DI)?
	jne .more			; If not, keep drawing

	popa
	ret


; -----------------------------------------------------------------
; @proc os_file_selector
; Show a file selection dialog
;
; @param_in  Nothing
; @param_out  AX - location of filename string (or 0 if Esc pressed)
; @section "dialog"

_os_file_selector:
	pusha

	mov word [.filename], 0		; Terminate string in case leave without select

	mov bx, .buffer1
	call _os_get_file_list


	; Time to convert the results of os_get_file_list into something more
	; readable in a list -- eg 'FILE    BIN' to 'FILE.BIN'

	mov si, .buffer1
	mov di, .buffer2

	mov cx, 0			; Counter

.loop:
	mov al, [si]
	cmp al, 0
	je .finished
	cmp al, ' '
	je .skip_space
	cmp al, ','
	je .next_file

	mov [di], al
	inc si
	inc di
	inc cx				; Reached extension point?
	cmp cx, 8
	je .need_dot
	jmp .loop

.skip_space:
	inc si
	inc cx
	cmp cx, 8
	je .need_dot
	jmp .loop

.need_dot:
	mov byte [di], '.'
	inc di
	jmp .loop

.next_file:
	mov byte [di], ','
	inc di
	inc si
	mov cx, 0
	jmp .loop



.finished:
	mov byte [di], 0			; Zero-terminate new string

	mov ax, .buffer2
	mov bx, .help_msg1
	mov cx, .help_msg2
	call _os_list_dialog

	jc .esc_pressed

	dec ax				; Result from os_list_box starts from 1, but
					; for our file list offset we want to start from 0


	mov bx, 12			; Each file is 12 bytes in list (11 + comma)
	mul bx

	mov si, .buffer1
	add si, ax

	mov di, .filename		; Copy filename we need into .filename
	mov cx, 11
	rep movsb

	mov byte [di], 0		; Zero terminate it

	popa

	mov ax, .filename

	clc

	ret


.esc_pressed:
	popa
	stc
	ret


	.buffer1	times 256 db 0
	.buffer2	times 256 db 0

	.help_msg1	db 'Please select a file using the cursor', 0
	.help_msg2	db 'keys from the list below...', 0

	.filename	times 12 db 0


; -----------------------------------------------------------------
; @proc os_list_dialog
; Show a dialog with a list of options
;
; @param_in  AX - comma-separated list of strings to show (zero-terminated),
; @param_in BX - first help string, CX = second help string
; @param_out AX - number, starting from 1, of entry selected or carry set if Esc pressed
; @section "dialog"

_os_list_dialog:
	pusha

	push ax				; Store string list for now

	push cx				; And help strings
	push bx

	call _os_cursor_hide

	mov bl, 01001111b		; White on red
	mov dl, 20			; Start X position
	mov dh, 2			; Start Y position
	mov si, 40			; Width
	mov di, 23			; Finish Y position
	call _os_draw_block		; Draw option selector window

	mov dl, 21			; Show first line of help text...
	mov dh, 3
	call _os_cursor_move

	pop si				; Get back first string
	call _os_print_string

	inc dh				; ...and the second
	call _os_cursor_move

	pop si
	call _os_print_string

	mov bl, 01110000b		; Black on grey for option list box
	mov dl, 21
	mov dh, 6
	mov si, 38
	mov di, 22
	call _os_draw_block

	mov dl, 33			; Get into position for option list text
	mov dh, 7
	call _os_cursor_move


	pop si				; SI = location of option list string (pushed earlier)

	mov bx, 0			; Counter for total number of options

.more:
	lodsb				; Get next character in file name, increment pointer

	cmp al, 0			; End of string?
	je .done_list

	cmp al, ','			; Next option? (String is comma-separated)
	je .newline

	mov ah, 0Eh
	int 10h
	jmp .more

.newline:
	mov dl, 33			; Go back to starting X position
	inc dh				; But jump down a line
	call _os_cursor_move

	inc bx				; Update the number-of-options counter
	cmp bx, 14			; Limit to one screen of options
	jl .more


.done_list:
	cmp bx, 0			; BX is our number-of-options counter
	jle .leave			; Nothing to process

	add bl, 7			; Last option -> line number (option 1 on line 7)

	mov dl, 25			; Set up starting position for selector
	mov dh, 7

.more_select:
	call _os_cursor_move

	mov si, .position_string	; Show '>>>>>' next to option
	call _os_print_string

.another_key:
	call _os_kbd_wait_for_key		; Move / select option
	cmp ah, 48h			; Up pressed?
	je .go_up
	cmp ah, 50h			; Down pressed?
	je .go_down
	cmp al, 13			; Enter pressed?
	je .option_selected
	cmp al, 27			; Esc pressed?
	je .esc_pressed
	jmp .more_select		; If not, wait for another key


.go_up:
	cmp dh, 7			; Already at top?
	jle .another_key

	mov dl, 25
	call _os_cursor_move

	mov si, .position_string_blank	; Otherwise overwrite '>>>>>'
	call _os_print_string

	dec dh                          ; Row to select (increasing down)
	jmp .more_select


.go_down:				; Already at bottom?
	cmp dh, bl
	jae .another_key

	mov dl, 25
	call _os_cursor_move

	mov si, .position_string_blank	; Otherwise overwrite '>>>>>'
	call _os_print_string

	inc dh
	jmp .more_select


.option_selected:
	call _os_cursor_show

	sub dh, 7

	mov ax, 0
	mov al, dh

	inc al				; Options start from 1

	mov word [.tmp], ax		; Store option number before restoring all other regs

	popa

	mov word ax, [.tmp]
	clc				; Clear carry as Esc wasn't pressed

	ret


.leave:
	call _os_cursor_show

	popa
	clc

	ret


	.tmp dw 0


.esc_pressed:
	call _os_cursor_show

	popa

	stc					; Set carry for Esc

	ret


	.position_string_blank	db '     ', 0
	.position_string	db '>>>>>', 0



; -----------------------------------------------------------------
; @proc os_draw_background
; Clear screen with white top and bottom bars,
; containing text, and a coloured middle section.
;
; @param_in  DX - top string locations
; @param_in  BX - bottom string locations
; @param_in  CX - colour
; @param_out Nothing
; @section "drawing"

_os_draw_background:
	pusha

        mov ax, dx

	push ax				; Store params to pop out later
	push bx
	push cx

	mov dl, 0
	mov dh, 0
	call _os_cursor_move

	mov ah, 09h			; Draw white bar at top
	mov bh,0
	mov cx, 80
	mov bl, 01110000b
	mov al, ' '
	int 10h

	mov dh, 1
	mov dl, 0
	call _os_cursor_move

	mov ah, 09h			; Draw colour section
	mov cx, 1840
	pop bx				; Get colour param (originally in CX)
	mov bh, 0
	mov al, ' '
	int 10h

	mov dh, 24
	mov dl, 0
	call _os_cursor_move

	mov ah, 09h			; Draw white bar at bottom
	mov bh, 0
	mov cx, 80
	mov bl, 01110000b
	mov al, ' '
	int 10h

	mov dh, 24
	mov dl, 1
	call _os_cursor_move
	pop bx				; Get bottom string param
	mov si, bx
	call _os_print_string

	mov dh, 0
	mov dl, 1
	call _os_cursor_move
	pop ax				; Get top string param
	mov si, ax
	call _os_print_string

	mov dh, 1			; Ready for app text
	mov dl, 0
	call _os_cursor_move

	popa
	ret


; -----------------------------------------------------------------
; @proc os_clear_screen
; Clears the screen to background
;
; @param_in Nothing
; @param_out Nothing (registers preserved)
; @section "screen"

_os_clear_screen:
	pusha

	mov dx, 0			; Position cursor at top-left
	call _os_cursor_move

	mov ah, 6			; Scroll full-screen
	mov al, 0			; Normal white on black
	mov bh, 7			;
	mov cx, 0			; Top-left
	mov dh, 24			; Bottom-right
	mov dl, 79
	int 10h

	popa
	ret


; -----------------------------------------------------------------
; @proc os_bcd_to_int
; Converts binary coded decimal number to an integer
;
; @param_in DL - BCD number
; @param_out  AX - integer value
; @section "integer"

_os_bcd_to_int:
	pusha

	mov bl, al			; Store entire number for now

	and ax, 0xF			; Zero-out high bits
	mov cx, ax			; CH/CL = lower BCD number, zero extended

	shr bl, 4			; Move higher BCD number into lower bits, zero fill msb
	mov al, 10
	mul bl				; AX = 10 * BL

	add ax, cx			; Add lower BCD to 10*higher
	mov [.tmp], ax

	popa
	mov ax, [.tmp]			; And return it in AX!
	ret


	.tmp	dw 0


; -----------------------------------------------------------------
; @proc os_serial_send
; Send a byte via the serial port
;
; @param_in  DL - byte to send via serial
; @param_out AH - Bit 7 clear on success
; @section "serial port"

_os_serial_send:
	pusha

        mov al, dl
	mov ah, 01h
	mov dx, 0		; COM1, as it's configured by the OS

	int 14h

	mov [.tmp], ax

	popa

	mov ax, [.tmp]

	ret


	.tmp dw 0


; -----------------------------------------------------------------
; @proc os_serial_get
; Get a byte from the serial port
;
; @param_in Nothing
; @param_out AL - byte that was received
; @param_out AH - Bit 7 clear on success
; @section "serial port"

_os_serial_get:
	pusha

	mov ah, 02h
	mov dx, 0		; COM1, as it's configured by the OS

	int 14h

	mov [.tmp], ax

	popa

	mov ax, [.tmp]

	ret


	.tmp dw 0


; -----------------------------------------------------------------
; @proc os_set_time_fmt
; Set time reporting format (eg '10:25 AM' or '2300 hours')
;
; @param_in  DL - format flag, 0 - 12-hr format
; @param_out Nothing
; @section "time"

_os_set_time_fmt:
	pusha
        mov al, dl
	cmp al, 0
	je .store
	mov al, 0FFh
.store:
	mov [fmt_12_24], al
	popa
	ret


; -----------------------------------------------------------------
; @proc os_get_time_string
; Get current time in a string (eg '10:25')
;
; @param_in  BX - string location
; @param_out BX - string location
; @section "time"

_os_get_time_string:
	pusha

	mov di, bx			; Location to place time string

	clc				; For buggy BIOSes
        mov ah, 0x02			; Get time data from BIOS in BCD format
        int 0x1A
	jnc .read

	clc
        mov ah, 0x02			; BIOS was updating (~1 in 500 chance), so try again
        int 0x1A

.read:
	mov dl, ch			; Convert hours to integer for AM/PM test
	call _os_bcd_to_int
	mov dx, ax			; Save

        mov al,	ch			; Hour
	shr al, 4			; Tens digit - move higher BCD number into lower bits
	and ch, 0x0F			; Ones digit
	test byte [fmt_12_24], 0FFh
	jz .twelve_hr

	call .add_digit			; BCD already in 24-hour format
	mov al, ch
	call .add_digit
	jmp short .minutes

.twelve_hr:
	cmp dx, 0			; If 00mm, make 12 AM
	je .midnight

	cmp dx, 10			; Before 1000, OK to store 1 digit
	jl .twelve_st1

	cmp dx, 12			; Between 1000 and 1300, OK to store 2 digits
	jle .twelve_st2

	mov ax, dx			; Change from 24 to 12-hour format
	sub ax, 12
	mov bl, 10
	div bl
	mov ch, ah

	cmp al, 0			; 1-9 PM
	je .twelve_st1

	jmp short .twelve_st2		; 10-11 PM

.midnight:
	mov al, 1
	mov ch, 2

.twelve_st2:
	call .add_digit			; Modified BCD, 2-digit hour
.twelve_st1:
	mov al, ch
	call .add_digit

	mov al, ':'			; Time separator (12-hr format)
	stosb

.minutes:
        mov al, cl			; Minute
	shr al, 4			; Tens digit - move higher BCD number into lower bits
	and cl, 0x0F			; Ones digit
	call .add_digit
	mov al, cl
	call .add_digit

	mov al, ' '			; Separate time designation
	stosb

	mov si, .hours_string		; Assume 24-hr format
	test byte [fmt_12_24], 0FFh
	jnz .copy

	mov si, .pm_string		; Assume PM
	cmp dx, 12			; Test for AM/PM
	jg .copy

	mov si, .am_string		; Was actually AM

.copy:
	lodsb				; Copy designation, including terminator
	stosb
	cmp al, 0
	jne .copy

	popa
	ret


.add_digit:
	add al, '0'			; Convert to ASCII
	stosb				; Put into string buffer
	ret


	.hours_string	db 'hours', 0
	.am_string 	db 'AM', 0
	.pm_string 	db 'PM', 0


; -----------------------------------------------------------------
; @proc os_date_set_fmt
; Set date reporting format (M/D/Y, D/M/Y or Y/M/D - 0, 1, 2)
;
; @param_in DX - format flag, 0-2
; @section "date"
; @note
; If DX bit 7 = 1 = use name for months
; If DX bit 7 = 0, high byte = separator character

_os_date_set_fmt:
	pusha
        mov ax, dx
	test al, 0x80		; ASCII months (bit 7)?
	jnz .fmt_clear

	and ax, 0x7F03		; 7-bit ASCII separator and format number
	jmp short .fmt_test

.fmt_clear:
	and ax, 0003		; Ensure separator is clear

.fmt_test:
	cmp al, 3		; Only allow 0, 1 and 2
	jae .leave
	mov [fmt_date], ax

.leave:
	popa
	ret


; -----------------------------------------------------------------
; @proc os_date_get_string
; Get current date in a string (eg '12/31/2007')
;
; @param_in  BX - string location
; @param_out BX - string location
; @section "date"

_os_date_get_string:
	pusha

	mov di, bx			; Store string location for now
	mov bx, [fmt_date]		; BL = format code
	and bx, 0x7F03			; BH = separator, 0 = use month names

	clc				; For buggy BIOSes
        mov ah, 0x04			; Get date data from BIOS in BCD format
        int 0x1A
	jnc .read

	clc
        mov ah, 0x04			; BIOS was updating (~1 in 500 chance), so try again
        int 0x1A

.read:
	cmp bl, 2			; YYYY/MM/DD format, suitable for sorting
	jne .try_fmt1

	mov ah, ch			; Always provide 4-digit year
	call .add_2digits
	mov ah, cl
	call .add_2digits		; And '/' as separator
	mov al, '/'
	stosb

	mov ah, dh			; Always 2-digit month
	call .add_2digits
	mov al, '/'			; And '/' as separator
	stosb

	mov ah, dl			; Always 2-digit day
	call .add_2digits
	jmp near .done

.try_fmt1:
	cmp bl, 1			; D/M/Y format (military and Europe)
	jne .do_fmt0

	mov ah, dl			; Day
	call .add_1or2digits

	mov al, bh
	cmp bh, 0
	jne .fmt1_day

	mov al, ' '			; If ASCII months, use space as separator

.fmt1_day:
	stosb				; Day-month separator

	mov ah,	dh			; Month
	cmp bh, 0			; ASCII?
	jne .fmt1_month

	call .add_month			; Yes, add to string
	mov ax, ', '
	stosw
	jmp short .fmt1_century

.fmt1_month:
	call .add_1or2digits		; No, use digits and separator
	mov al, bh
	stosb

.fmt1_century:
	mov ah,	ch			; Century present?
	cmp ah, 0
	je .fmt1_year

	call .add_1or2digits		; Yes, add it to string (most likely 2 digits)

.fmt1_year:
	mov ah, cl			; Year
	call .add_2digits		; At least 2 digits for year, always

	jmp short .done

.do_fmt0:				; Default format, M/D/Y (US and others)
	mov ah,	dh			; Month
	cmp bh, 0			; ASCII?
	jne .fmt0_month

	call .add_month			; Yes, add to string and space
	mov al, ' '
	stosb
	jmp short .fmt0_day

.fmt0_month:
	call .add_1or2digits		; No, use digits and separator
	mov al, bh
	stosb

.fmt0_day:
	mov ah, dl			; Day
	call .add_1or2digits

	mov al, bh
	cmp bh, 0			; ASCII?
	jne .fmt0_day2

	mov al, ','			; Yes, separator = comma space
	stosb
	mov al, ' '

.fmt0_day2:
	stosb

.fmt0_century:
	mov ah,	ch			; Century present?
	cmp ah, 0
	je .fmt0_year

	call .add_1or2digits		; Yes, add it to string (most likely 2 digits)

.fmt0_year:
	mov ah, cl			; Year
	call .add_2digits		; At least 2 digits for year, always


.done:
	mov ax, 0			; Terminate date string
	stosw

	popa
	ret


.add_1or2digits:
	test ah, 0x0F0
	jz .only_one
	call .add_2digits
	jmp short .two_done
.only_one:
	mov al, ah
	and al, 0x0F
	call .add_digit
.two_done:
	ret

.add_2digits:
	mov al, ah			; Convert AH to 2 ASCII digits
	shr al, 4
	call .add_digit
	mov al, ah
	and al, 0x0F
	call .add_digit
	ret

.add_digit:
	add al, '0'			; Convert AL to ASCII
	stosb				; Put into string buffer
	ret

.add_month:
	push bx
	push cx
	mov dl, ah			; Convert month to integer to index print table
	call _os_bcd_to_int
	dec al				; January = 0
	mov bl, 4			; Multiply month by 4 characters/month
	mul bl
	mov si, .months
	add si, ax
	mov cx, 4
	rep movsb
	cmp byte [di-1], ' '		; May?
	jne .done_month			; Yes, eliminate extra space
	dec di
.done_month:
	pop cx
	pop bx
	ret


	.months db 'Jan.Feb.Mar.Apr.May JuneJulyAug.SeptOct.Nov.Dec.'


; -----------------------------------------------------------------
; @proc os_print_horiz_line
; Draw a horizontal line on the screen
;
; @param_in AX - line type (1 for double, otherwise single)
; @param_out Nothing (registers preserved)
; @section "print"


_os_print_horiz_line:
	pusha

	mov cx, ax			; Store line type param
	mov al, 196			; Default is single-line code

	cmp cx, 1			; Was double-line specified in AX?
	jne .ready
	mov al, 205			; If so, here's the code

.ready:
	mov cx, 0			; Counter
	mov ah, 0Eh			; BIOS output char routine

.restart:
	int 10h
	inc cx
	cmp cx, 80			; Drawn 80 chars yet?
	je .done
	jmp .restart

.done:
	popa
	ret

; -----------------------------------------------------------------
; @proc os_print_char
; Reset cursor to start of next line
;
; @param_in DL - character to print
; @param_out Nothing (registers preserved)
; @section "print"


_os_print_char:
	pusha

	mov ah, dl			; BIOS output char code

	mov al, 13
	int 10h
	mov al, 10
	int 10h

	popa
	ret


; -----------------------------------------------------------------
; @proc os_print_newline
; Reset cursor to start of next line
;
; @param_in Nothing
; @param_out Nothing (registers preserved)
; @section "print"


_os_print_newline:
	pusha

	mov ah, 0Eh			; BIOS output char code

	mov al, 13
	int 10h
	mov al, 10
	int 10h

	popa
	ret

; ------------------------------------------------------------
; @proc os_kbd_flush_buffer
; clear the keyboard buffer
;
; @param_in Nothing
; @param_out Nothing
; @section "keyboard"

_os_kbd_flush_buffer:
    push ds
    push si
    push ax
    cli

    push BIOS_SEGMENT
    pop ds                              ; load ds with BIOS segment
    mov si, BIOS_KEYBOARD_HEAD          
    mov ax, word [ds:si]                 
    mov word [ds:si-2], ax              ; tail = head

    sti
    pop ax
    pop si
    pop ds
    ret

; ------------------------------------------------------------
; @proc os_kbd_check_buffer
; check if any character in the keyboard buffer
;
; @param_in Nothing
; @param_out if the keyboard buffer empty the AX register is 0
; @section "keyboard"

_os_kbd_check_buffer:
    push ds
    push si
    cli

    push BIOS_SEGMENT                ; load DS with BIOS segment
    pop ds
    mov si, BIOS_KEYBOARD_TAIL       ; get tail of the keyboard buffer
    mov ax, word [ds:si]
    sub ax, word [ds:si+2]           ; substract a head of the buffer and return in AX

    pop si
    pop ds
    ret

; ------------------------------------------------------------
; @proc os_kbd_get_char
; get charcter from the keyboard buffer. If the buffer is empty 
; does not wait for a character and return AX=0. If is not empty 
; return scan code and ascii code of the character in the buffer
;
; @param_in Nothing
; @param_out if the keyboard buffer empty AX=0
; @param_out if the keyboard buffer is not empry AH=scan code
; @param_out if AH=scan code
; @param_out if AL=ascii code
; @section "keyboard"

_os_kbd_get_char:
    push ds
    push si
    push bx
    push dx
    cli

    push BIOS_SEGMENT
    pop ds
    mov si, BIOS_KEYBOARD_TAIL   ; head of the keyboard buffer
    mov ax, word [ds:si]         ; tail of the keyboard buffer
    cmp ax, word [ds:si+2]       ; if tail equals head means buffer empty and exit
    jz .empty                    

    mov si, ax                   ; move tail to AX
    push word [ds:si]            ; save one char from the buffer

    mov si, BIOS_KEYBOARD_BUFOR_STOP     
    mov bx, word [ds:si]               ; read stop of keyboard buffer
    mov si, BIOS_KEYBOARD_BUFOR_START
    sub bx, word [ds:si]               ; campute size of the keyboard buffer
 
    sub ax, -2                         ; jump on the next character in the buffer
    sub ax, word [ds:si]               ; compute offset of the character in the buffer

    cwd

    div bx                             ; calculate (offset of charcter) modulo ( size of the buffor) (result in DX)
   
    mov si, BIOS_KEYBOARD_BUFOR_START
    add dx, word [ds:si]               ; keyboard buffer is a ring; Add DX to start of the buffer; this is a new tail
    mov si, BIOS_KEYBOARD_TAIL
    mov word [ds:si], dx               ; save it into BIOS memory

    pop ax 

    pop dx
    pop bx
    pop si
    pop ds

    test ax, ax

    sti

    ret

    .empty:
    pop dx
    pop bx
    pop si
    pop ds
    xor ax, ax

    sti
    ret


; -----------------------------------------------------------------
; @proc os_kbd_wait_for_key
; Waits for keypress and returns key
;
; @param_in Nothing
; @param_out AX - key pressed, other regs preserved
; @section "keyboard"

_os_kbd_wait_for_key:

        call _os_kbd_flush_buffer

        .loop:
        call _os_kbd_get_char
        test ax, ax
        jz .loop

	ret


; -----------------------------------------------------------------
; @proc os_kbd_check_for_key
; Scans keyboard for input, but doesn't wait
; 
; @param_in Nothing
; @param_out AX - 0 if no key pressed, otherwise scan code
; @section "keyboard"

_os_kbd_check_for_key:
	pusha

        call _os_kbd_get_char

        test ax, ax
        jz .nokey

	mov [.tmp_buf], ax		; Store resulting keypress

	popa				; But restore all other regs
	mov ax, [.tmp_buf]
	ret

.nokey:
	popa
	mov al, 0			; Zero result if no key pressed
	ret


	.tmp_buf	dw 0


; -----------------------------------------------------------------
; @proc os_dump_registers
; Displays register contents in hex on the screen
;
; @param_in  AX/BX/CX/DX - registers to show
; @param_out Nothing
; @section "debug"

_os_dump_registers:
	pusha

	call _os_print_newline

	push di
	push si
	push dx
	push cx
	push bx

        mov dx, ax
	mov si, .ax_string
	call _os_print_string
	call _os_print_4hex

	pop dx
	mov si, .bx_string
	call _os_print_string
	call _os_print_4hex

	pop dx
	mov si, .cx_string
	call _os_print_string
	call _os_print_4hex

	pop dx
	mov si, .dx_string
	call _os_print_string
	call _os_print_4hex

	pop dx
	mov si, .si_string
	call _os_print_string
	call _os_print_4hex

	pop dx
	mov si, .di_string
	call _os_print_string
	call _os_print_4hex

	call _os_print_newline

	popa
	ret


	.ax_string		db 'AX:', 0
	.bx_string		db ' BX:', 0
	.cx_string		db ' CX:', 0
	.dx_string		db ' DX:', 0
	.si_string		db ' SI:', 0
	.di_string		db ' DI:', 0


; -----------------------------------------------------------------
; @proc os_int_to_string
; Convert value in DX to string
;
; @param_in DX - integer
; @param_in BX - location of string
; @param_out BX - location of converted string (other regs preserved)
; @section "string"
; @note Based on public domain code

_os_int_to_string:
    pusha 

    call _os_int_to_string_rec

    mov byte [bx], 0

    popa
    ret


_os_int_to_string_rec:

        mov cx, 10
        mov ax, dx

        cwd

        div cx

        test ax, ax
        jz .exit

        .rec:
        push dx
        mov dx, ax

        call _os_int_to_string_rec

        pop dx

        .exit:    

        add dl, '0'
        mov byte [bx], dl
        inc bx

        ret

; -----------------------------------------------------------------
; @proc os_speaker_tone
; Generate PC speaker tone (call _os_speaker_off after)
;
; @param_in DX - note frequency
; @param_out  Nothing (registers preserved)
; @section "PC speaker"

_os_speaker_tone:
	pusha

        mov ax, dx
	mov cx, ax		; Store note value for now

	mov al, 182
	out 43h, al
	mov ax, cx		; Set up frequency
	out 42h, al
	mov al, ah
	out 42h, al

	in al, 61h		; Switch PC speaker on
	or al, 03h
	out 61h, al

	popa
	ret


; -----------------------------------------------------------------
; @proc os_speaker_off
; Turn off PC speaker
;
; @param_in Nothing
; @param_out Nothing (registers preserved)
; @section "PC speaker"

_os_speaker_off:
	pusha

	in al, 61h		; Switch PC speaker off
	and al, 0FCh
	out 61h, al

	popa
	ret


; --------------------------------------------------------------------------
; @proc os_input_dialog
; Get text string from user via a dialog box
;
; @param_in DX - string location
; @param_in BX - message to show
; @param_out AX - string location
; @section "dialog"

_os_input_dialog:
	pusha

        mov ax, dx

	push ax				; Save string location
	push bx				; Save message to show


	mov dh, 10			; First, draw red background box
	mov dl, 12

.redbox:				; Loop to draw all lines of box
	call _os_cursor_move

	pusha
	mov ah, 09h
	mov bh, 0
	mov cx, 55
	mov bl, 01001111b		; White on red
	mov al, ' '
	int 10h
	popa

	inc dh
	cmp dh, 16
	je .boxdone
	jmp .redbox


.boxdone:
	mov dl, 14
	mov dh, 11
	call _os_cursor_move


	pop bx				; Get message back and display it
	mov si, bx
	call _os_print_string

	mov dl, 14
	mov dh, 13
	call _os_cursor_move


	pop ax				; Get input string back
	call _os_input_string

	popa
	ret


; -----------------------------------------------------------------
; @proc os_dialog_box
; Print dialog box in middle of screen, with button(s)
;
; @param_in  SI, BX, DI - string locations (set registers to 0 for no display)
; @param_in  DX = 0 for single 'OK' dialog, 1 for two-button 'OK' and 'Cancel'
; @param_out If two-button mode, AX = 0 for OK and 1 for cancel
; @section "dialog"
; @note: Each string is limited to 40 characters

_os_dialog_box:
	pusha

        mov ax, si
        mov cx, di

	mov [.tmp], dx

	push bx				; Store first string location...
        push ax
        mov bx, ax
	call _os_string_length		; ...because this converts AX to a number
	cmp ax, 40			; Check to see if it's less than 30 chars
	jg .string_too_long

	call _os_string_length
	cmp ax, 40
	jg .string_too_long

	mov bx, cx			; Check third string length
	call _os_string_length
	cmp ax, 40
	jg .string_too_long

        pop ax
	pop bx				; Get first string location back
	jmp .strings_ok			; All string lengths OK, so let's move on


.string_too_long:
        pop ax
	pop bx				; We pushed this before
	mov bx, .err_msg_string_length
	call _os_fatal_error


.strings_ok:
	call _os_cursor_hide

	mov dh, 9			; First, draw red background box
	mov dl, 19

.redbox:				; Loop to draw all lines of box
	call _os_cursor_move

	pusha
	mov ah, 09h
	mov bh, 0
	mov cx, 42
	mov bl, 01001111b		; White on red
	mov al, ' '
	int 10h
	popa

	inc dh
	cmp dh, 16
	je .boxdone
	jmp .redbox


.boxdone:
	cmp ax, 0			; Skip string params if zero
	je .no_first_string
	mov dl, 20
	mov dh, 10
	call _os_cursor_move

	mov si, ax			; First string
	call _os_print_string

.no_first_string:
	cmp bx, 0
	je .no_second_string
	mov dl, 20
	mov dh, 11
	call _os_cursor_move

	mov si, bx			; Second string
	call _os_print_string

.no_second_string:
	cmp cx, 0
	je .no_third_string
	mov dl, 20
	mov dh, 12
	call _os_cursor_move

	mov si, cx			; Third string
	call _os_print_string

.no_third_string:
	mov dx, [.tmp]
	cmp dx, 0
	je .one_button
	cmp dx, 1
	je .two_button


.one_button:
	mov dl, 35			; OK button, centered at bottom of box
	mov dh, 14
	call _os_cursor_move
	mov si, .ok_button_string
	call _os_print_string

	jmp .one_button_wait


.two_button:
	mov dl, 27			; OK button
	mov dh, 14
	call _os_cursor_move
	mov si, .ok_button_string
	call _os_print_string

	mov dl, 42			; Cancel button
	mov dh, 14
	call _os_cursor_move
	mov si, .cancel_button_noselect
	call _os_print_string

	mov cx, 0			; Default button = 0
	jmp .two_button_wait



.one_button_wait:
	call _os_kbd_wait_for_key
	cmp al, 13			; Wait for enter key (13) to be pressed
	jne .one_button_wait

	call _os_cursor_show

	popa
	ret


.two_button_wait:
	call _os_kbd_wait_for_key

	cmp ah, 75			; Left cursor key pressed?
	jne .noleft

	mov dl, 27			; If so, change printed buttons
	mov dh, 14
	call _os_cursor_move
	mov si, .ok_button_string
	call _os_print_string

	mov dl, 42			; Cancel button
	mov dh, 14
	call _os_cursor_move
	mov si, .cancel_button_noselect
	call _os_print_string

	mov cx, 0			; And update result we'll return
	jmp .two_button_wait


.noleft:
	cmp ah, 77			; Right cursor key pressed?
	jne .noright

	mov dl, 27			; If so, change printed buttons
	mov dh, 14
	call _os_cursor_move
	mov si, .ok_button_noselect
	call _os_print_string

	mov dl, 42			; Cancel button
	mov dh, 14
	call _os_cursor_move
	mov si, .cancel_button_string
	call _os_print_string

	mov cx, 1			; And update result we'll return
	jmp .two_button_wait


.noright:
	cmp al, 13			; Wait for enter key (13) to be pressed
	jne .two_button_wait

	call _os_cursor_show

	mov [.tmp], cx			; Keep result after restoring all regs
	popa
	mov ax, [.tmp]

	ret


	.err_msg_string_length	db 'os_dialog_box: Supplied string too long', 0
	.ok_button_string	db '[= OK =]', 0
	.cancel_button_string	db '[= Cancel =]', 0
	.ok_button_noselect	db '   OK   ', 0
	.cancel_button_noselect	db '   Cancel   ', 0

	.tmp dw 0


; -----------------------------------------------------------------
; @proc os_input_string
; Take string from keyboard entry
;
; @param_in DX = location of string, other regs preserved
; @param_out AX = location of string, other regs preserved
; @section "keyboard"
; @note (Location will contain up to 255 characters, zero-terminated)

_os_input_string:
	pusha

        mov ax, dx

	mov di, ax		; DI is where we'll store input (buffer)
	mov cx, 0		; Character received counter for backspace


.more:				; Now onto string getting
	call _os_kbd_wait_for_key

	cmp al, 13		; If Enter key pressed, finish
	je .done

	cmp al, 8		; Backspace pressed?
	je .backspace		; If not, skip following checks

	cmp al, ' '		; In ASCII range (32 - 126)?
	jb .more		; Ignore most nonprinting characters

	cmp al, '~'
	ja .more

	jmp .nobackspace


.backspace:
	cmp cx, 0			; Backspace at start of string?
	je .more			; Ignore it if so

	call _os_cursor_get_pos		; Backspace at start of screen line?
	cmp dl, 0
	je .backspace_linestart

	pusha
	mov ah, 0Eh			; If not, write space and move cursor back
	mov al, 8
	int 10h				; Backspace twice, to clear space
	mov al, 32
	int 10h
	mov al, 8
	int 10h
	popa

	dec di				; Character position will be overwritten by new
					; character or terminator at end

	dec cx				; Step back counter

	jmp .more


.backspace_linestart:
	dec dh				; Jump back to end of previous line
	mov dl, 79
	call _os_cursor_move

	mov al, ' '			; Print space there
	mov ah, 0Eh
	int 10h

	mov dl, 79			; And jump back before the space
	call _os_cursor_move

	dec di				; Step back position in string
	dec cx				; Step back counter

	jmp .more


.nobackspace:
	pusha
	mov ah, 0Eh			; Output entered, printable character
	int 10h
	popa

	stosb				; Store character in designated buffer
	inc cx				; Characters processed += 1
	cmp cx, 254			; Make sure we don't exhaust buffer
	jae near .done

	jmp near .more			; Still room for more


.done:
	mov ax, 0
	stosb

	popa
	ret


; -----------------------------------------------------------------
; @proc os_string_length
; Return length of a string
;
; @param_in  BX - string location
; @param_out AX - length (other regs preserved)
; @section "string"

_os_string_length:
	pusha

	mov cx, 0

.more:
	cmp byte [bx], 0	; Zero (end of string) yet?
	je .done
	inc bx			; If not, keep adding
	inc cx
	jmp .more


.done:
	mov word [.tmp_counter], cx
	popa

	mov ax, [.tmp_counter]
	ret


	.tmp_counter	dw 0

; -----------------------------------------------------------------
; @proc os_string_not_find_char
; Find location of first non character in a string
;
; @param_in  SI - string location
; @param_in  DL - character to find
; @param_out AX - location in string, or 0FFFFh if char not present
; @section "string"

_os_string_not_find_char:
    
    push bx
    mov bx, 1
    xor ax, ax
    .loop_1:
        mov al, byte [si + bx - 1]
        cmp byte al , dl
        jnz .found
        test al, al
        jz .no_found
        inc bx
    jmp .loop_1
    .found:

    mov ax, bx
    pop bx
  
    .no_found:

    ret
   

; -----------------------------------------------------------------
; @proc os_string_find_char
; Find location of character in a string
;
; @param_in  SI - string location
; @param_in  DL - character to find
; @param_out AX - location in string, or 0 if char not present
; @section "string"

_os_string_find_char:
        push bx

	mov bx, 1		; Counter -- start at first char
        xor ax, ax

.more:
        mov al, byte [si + bx]
	cmp byte al, dl
	je .done
         test al, al
	jz .notfound
	inc bx
	jmp .more

.done:
        mov ax, bx
        pop bx
	ret

.notfound:
        pop bx
	ret

; -----------------------------------------------------------------
; @proc os_string_uppercase
; Convert zero-terminated string to upper case
;
; @param_in  SI - string location
; @param_out Nothing
; @section "string"

_os_string_uppercase:
	pusha

.more:
	cmp byte [si], 0		; Zero-termination of string?
	je .done			; If so, quit

	cmp byte [si], 'a'		; In the lower case A to Z range?
	jb .noatoz
	cmp byte [si], 'z'
	ja .noatoz

	sub byte [si], 20h		; If so, convert input char to upper case

	inc si
	jmp .more

.noatoz:
	inc si
	jmp .more

.done:
	popa
	ret

; -----------------------------------------------------------------
; @proc os_string_n_uppercase
; Convert n first characters of zero-terminated string to upper case
;
; @param_in  SI - string location
; @param_in  CX - number of character to uppercase
; @param_out Nothing
; @section "string"

_os_string_n_uppercase:
	pusha

.more:

        test cx, cx
        jz .done
        dec cx

	cmp byte [si], 0		; Zero-termination of string?
	je .done			; If so, quit

	cmp byte [si], 'a'		; In the lower case A to Z range?
	jb .noatoz
	cmp byte [si], 'z'
	ja .noatoz

	sub byte [si], 20h		; If so, convert input char to upper case

	inc si
	jmp .more

.noatoz:
	inc si
	jmp .more

.done:
	popa
	ret

; -----------------------------------------------------------------
; @proc os_string_lowercase
; Convert zero-terminated string to lower case
;
; @param_in  SI - string location
; @param_out Nothing
; @section "string"

_os_string_lowercase:
	pusha

.more:
	cmp byte [si], 0		; Zero-termination of string?
	je .done			; If so, quit

	cmp byte [si], 'A'		; In the upper case A to Z range?
	jb .noatoz
	cmp byte [si], 'Z'
	ja .noatoz

	add byte [si], 20h		; If so, convert input char to lower case

	inc si
	jmp .more

.noatoz:
	inc si
	jmp .more

.done:
	popa
	ret


; -----------------------------------------------------------------
; @proc os_string_copy
; Copy one string into another
;
; @param_in SI - source
; @param_in DI - destination (programmer ensure sufficient room)
; @param_out Nothing
; @section "string"

_os_string_copy:
	pusha

.more:
	mov al, [si]		; Transfer contents (at least one byte terminator)
	mov [di], al
	inc si
	inc di
	cmp byte al, 0		; If source string is empty, quit out
	jne .more

.done:
	popa
	ret


; -----------------------------------------------------------------
; @proc os_string_truncate
; Chop string down to specified number of characters
; 
; @param_in SI - string location
; @param_in AX - number of characters
; @param_out String modified, registers preserved
; @section "string"

_os_string_truncate:
	pusha

        mov dx, ax
        mov bx, si
        call _os_string_length

        cmp ax, dx
        jb .exit

	add si, dx
	mov byte [si], 0

        .exit:

	popa
	ret


; -----------------------------------------------------------------
; @proc os_string_join
; Join two strings into a third string
;
; @param_in SI - string one,
; @param_in BX - string two,
; @param_in DI - destination string
; @section "string"

_os_string_join:
	pusha

	call _os_string_copy

        push bx
        mov bx, ax

	call _os_string_length	; Get length of first string

        pop bx

	add cx, ax		; Position at end of first string

	mov si, bx		; Add second string onto it
	mov di, cx
	call _os_string_copy

	popa
	ret


; -----------------------------------------------------------------
; @proc os_string_chomp
; Strip leading and trailing spaces from a string
;
; @param_in BX - string location
; @param_out Nothing
; @section "string"

_os_string_chomp:
	pusha

	mov dx, bx			; Save string location

	mov di, bx			; Put location into DI
	mov cx, 0			; Space counter

.keepcounting:				; Get number of leading spaces into BX
	cmp byte [di], ' '
	jne .counted
	inc cx
	inc di
	jmp .keepcounting

.counted:
	cmp cx, 0			; No leading spaces?
	je .finished_copy

	mov si, di			; Address of first non-space character
	mov di, dx			; DI = original string start

.keep_copying:
	mov al, [si]			; Copy SI into DI
	mov [di], al			; Including terminator
	cmp al, 0
	je .finished_copy
	inc si
	inc di
	jmp .keep_copying

.finished_copy:
	mov bx, dx			; AX = original string start

	call _os_string_length
	cmp ax, 0			; If empty or all blank, done, return 'null'
	je .done

	mov si, dx
	add si, ax			; Move to end of string

.more:
	dec si
	cmp byte [si], ' '
	jne .done
	mov byte [si], 0		; Fill end spaces with 0s
	jmp .more			; (First 0 will be the string terminator)

.done:
	popa
	ret


; -----------------------------------------------------------------
; @proc os_string_strip
; Removes specified character from a string
;
; @param_in SI - string location
; @param_in DL - character to remove
; @section "string"

_os_string_strip:
	pusha

	mov di, si

.more:
	mov al, byte [si]
   
	cmp al, 0
	je .done

	inc si
	cmp al, dl
	je .more

	mov byte [di], al
	inc di
	jmp .more

.done:
	mov [di], al

	popa
	ret


; -----------------------------------------------------------------
; @proc os_string_compare
;  See if two strings match
;
; @param_in SI - string one
; @param_in DI - string two
; @param_out  CF set if same, clear if different
; @section "string"

_os_string_compare:
	pusha

.more:
	mov al, [si]			; Retrieve string contents
	mov bl, [di]

	cmp al, bl			; Compare characters at current location
	jne .not_same

	cmp al, 0			; End of first string?  Must also be end of second
	je .terminated

	inc si
	inc di
	jmp .more


.not_same:				; If unequal lengths with same beginning, the byte
	popa				; comparison fails at shortest string terminator
	clc				; Clear carry flag
	ret


.terminated:				; Both strings terminated at the same position
	popa
	stc				; Set carry flag
	ret

; -----------------------------------------------------------------
; @proc os_string_n_compare
;  See if n characters of two strings match.
;
; @param_in SI - string one
; @param_in DI - string two
; @param_in CX - number of characters
; @param_out  CF set if same, clear if different
; @section "string"

_os_string_n_compare:
	pusha

.more:
        test cx, cx
        jz .terminated

        dec cx

	mov al, [si]			; Retrieve string contents
	mov bl, [di]

	cmp al, bl			; Compare characters at current location
	jne .not_same

	cmp al, 0			; End of first string?  Must also be end of second
	je .terminated


	inc si
	inc di
	jmp .more


.not_same:				; If unequal lengths with same beginning, the byte
	popa				; comparison fails at shortest string terminator
	clc				; Clear carry flag
	ret


.terminated:				; Both strings terminated at the same position
	popa
	stc				; Set carry flag
	ret


; -----------------------------------------------------------------
; @proc os_string_strincmp
; See if two strings match up to set number of chars
;
; @param_in SI - string one
; @param_in DI - string two
; @param_in CL - chars to check
; @param_out carry set if same, clear if different
; @section "string"

_os_string_strincmp:
	pusha

.more:
	mov al, [si]			; Retrieve string contents
	mov bl, [di]

	cmp al, bl			; Compare characters at current location
	jne .not_same

	cmp al, 0			; End of first string?  Must also be end of second
	je .terminated

	inc si
	inc di

	dec cl				; If we've lasted through our char count
	cmp cl, 0			; Then the bits of the string match!
	je .terminated

	jmp .more


.not_same:				; If unequal lengths with same beginning, the byte
	popa				; comparison fails at shortest string terminator
	clc				; Clear carry flag
	ret


.terminated:				; Both strings terminated at the same position
	popa
	stc				; Set carry flag
	ret


; -----------------------------------------------------------------
; @proc os_print_space
; Print a space to the screen
; 
; @param_in Nothing
; @param_out Nothing
; @section "print"


_os_print_space:
	pusha

	mov ah, 0Eh		; BIOS teletype function
	mov al, 20h		; Space is character 0x20
	int 10h

	popa
	ret


; -----------------------------------------------------------------
; @proc os_dump_string
; Dump string as hex bytes and printable characters
;
; @param_in  SI - points to string to dump
; @param_out Nothing
; @section "print"


_os_dump_string:
	pusha

	mov bx, si		; Save for final print

.line:
	mov di, si		; Save current pointer
	mov cx, 0		; Byte counter

.more_hex:
	lodsb
	cmp al, 0
	je .chr_print

        mov dx, ax

	call _os_print_2hex
	call _os_print_space	; Single space most bytes
	inc cx

	cmp cx, 8
	jne .q_next_line

	call _os_print_space	; Double space center of line
	jmp .more_hex

.q_next_line:
	cmp cx, 16
	jne .more_hex

.chr_print:
	call _os_print_space
	mov ah, 0Eh		; BIOS teletype function
	mov al, '|'		; Break between hex and character
	int 10h
	call _os_print_space

	mov si, di		; Go back to beginning of this line
	mov cx, 0

.more_chr:
	lodsb
	cmp al, 0
	je .done

	cmp al, ' '
	jae .tst_high

	jmp short .not_printable

.tst_high:
	cmp al, '~'
	jbe .output

.not_printable:
	mov al, '.'

.output:
	mov ah, 0Eh
	int 10h

	inc cx
	cmp cx, 16
	jl .more_chr

	call _os_print_newline	; Go to next line
	jmp .line

.done:
	call _os_print_newline	; Go to next line

	popa
	ret


; -----------------------------------------------------------------
; @proc os_print_digit
; Displays contents of AX as a single digit. 
; Works up to base 37, ie digits 0-Z
;
; @param_in  DX - "digit" to format and print
; @section "print"


_os_print_digit:
	pusha

        mov ax, dx
	cmp ax, 9		; There is a break in ASCII table between 9 and A
	jle .digit_format

	add ax, 'A'-'9'-1	; Correct for the skipped punctuation

.digit_format:
	add ax, '0'		; 0 will display as '0', etc.	

	mov ah, 0Eh		; May modify other registers
	int 10h

	popa
	ret


; -----------------------------------------------------------------
; @proc os_print_1hex
; Displays low nibble of DL in hex format
;
; @param_in  DL = number to format and print
; @param_out Nothing
; @section "print"


_os_print_1hex:
	pusha

	and dx, 0Fh		; Mask off data to display
	call _os_print_digit

	popa
	ret


; -----------------------------------------------------------------
; @proc os_print_2hex
; Displays DL in hex format
;
; @param_in DL - number to format and print
; @param_out Nothing
; @section "print"



_os_print_2hex:
	pusha

	push dx			; Output high nibble
	shr dx, 4
	call _os_print_1hex

	pop dx			; Output low nibble
	call _os_print_1hex

	popa
	ret


; -----------------------------------------------------------------
; @proc os_print_4hex
; Displays DX in hex format
;
; @param_in DX - number to format and print
; @section "print"


_os_print_4hex:
	pusha

	push dx			; Output high byte
	mov dl, dh
	call _os_print_2hex

	pop dx			; Output low byte
	call _os_print_2hex

	popa
	ret


; -----------------------------------------------------------------
; @proc os_long_int_to_string
; Convert value in DX:CX to string
; 
; @param_in  DX:CX - long unsigned integer
; @param_in  BX - number base
; @param_in  DI - string location
; @param_out DI - location of converted string
; @section "long int"

_os_long_int_to_string:
	pusha

        mov ax, cx

	mov si, di		; Prepare for later data movement

	mov word [di], 0	; Terminate string, creates 'null'

	cmp bx, 37		; Base > 37 or < 0 not supported, return null
	ja .done

	cmp bx, 0		; Base = 0 produces overflow, return null
	je .done

.conversion_loop:
	mov cx, 0		; Zero extend unsigned integer, number = CX:DX:AX
				; If number = 0, goes through loop once and stores '0'

	xchg ax, cx		; Number order DX:AX:CX for high order division
	xchg ax, dx
	div bx			; AX = high quotient, DX = high remainder

	xchg ax, cx		; Number order for low order division
	div bx			; CX = high quotient, AX = low quotient, DX = remainder
	xchg cx, dx		; CX = digit to send

.save_digit:
	cmp cx, 9		; Eliminate punctuation between '9' and 'A'
	jle .convert_digit

	add cx, 'A'-'9'-1

.convert_digit:
	add cx, '0'		; Convert to ASCII

	push ax			; Load this ASCII digit into the beginning of the string
	push bx
	mov bx, si
	call _os_string_length	; AX = length of string, less terminator
	mov di, si
	add di, ax		; DI = end of string
	inc ax			; AX = nunber of characters to move, including terminator

.move_string_up:
	mov bl, [di]		; Put digits in correct order
	mov [di+1], bl
	dec di
	dec ax
	jnz .move_string_up

	pop bx
	pop ax
	mov [si], cl		; Last digit (lsd) will print first (on left)

.test_end:
	mov cx, dx		; DX = high word, again
	or cx, ax		; Nothing left?
	jnz .conversion_loop

.done:
	popa
	ret


; -----------------------------------------------------------------
; @proc os_long_int_negate
; Multiply value in DX:BX by -1
;
; @param_in  DX:BX - long integer
; @param_out DX:BX = -(initial DX:AX)
; @section "long int"

_os_long_int_negate:
	neg bx
	adc dx, 0
	neg dx
	ret


; -----------------------------------------------------------------
; @proc os_pause
; Delay execution for specified microseconds
;
; @param_in CX:DX - number of microseconds to wait
; @param_out Nothing
; @section "misc"

_os_pause:
	pusha

	mov ah, 86h
	int 15h

	popa
	ret


; -----------------------------------------------------------------
; @proc os_get_api_version
; Return current version of TomOS API
;
; @param_in  Nothing
; @param_out AL - API version number
; @section "system"

_os_get_api_version:
	mov al, TOMOS_API_VER
	ret


; -----------------------------------------------------------------
; @proc os_get_int_handler
; Get the segment:offset of an interrupt handler
;
; @param_in  DX - int number
; @param_out ES:BX - contents of handler location
; @section "system"

_os_get_int_handler:
	push ax				; A pusha won't allow parameter return
	push cx
        push dx
	push ds

        mov ax, dx

	and ax, 0FFh			; Ensure number is within range
	mov cl, 4			; Beginning address = base + 4 * number
	mul cl				; Base = 0000, 4 bytes per entry
	mov si, ax

	mov ax, 0			; Interrupt table is in segment 0
	mov ds, ax

	mov bx, [ds:si]			; Get interrupt service address
	mov ax, [ds:si+2]		; Get interrupt service segment
	mov es, ax

	pop ds
        pop dx
	pop cx
	pop ax

	ret


; -----------------------------------------------------------------
; @proc os_modify_int_handler
; Change location of interrupt handler
;
; @param_in  CX - int number
; @param_in  SI - handler location
; @section "system"

_os_modify_int_handler:
	pusha

	cli

	mov dx, es			; Store original ES

	mov ax, 0			; Clear AX for new ES value
	mov es, ax

	mov al, cl			; Move supplied int into AL

	mov bl, 4			; Multiply by four to get position
	mul bl				; (Interrupt table = 4 byte sections)
	mov bx, ax

	mov [es:bx], si			; First store offset
	add bx, 2

	mov ax, 0x2000			; Then segment of our handler
	mov [es:bx], ax

	mov es, dx			; Finally, restore data segment

	sti

	popa
	ret


; -----------------------------------------------------------------
; @proc os_fatal_error
; Display error message, take keypress, and restart OS
; 
; @param_in  BX - error message string location
; @param_out Nothing
; @section "error"

_os_fatal_error:

	mov dh, 0
	mov dl, 0
	call _os_cursor_move

	pusha
	mov ah, 09h			; Draw red bar at top
	mov bh, 0
	mov cx, 240
	mov bl, 01001111b
	mov al, ' '
	int 10h
	popa

	mov dh, 0
	mov dl, 0
	call _os_cursor_move

	mov si, .msg_inform		; Inform of fatal error
	call _os_print_string

	mov si, bx			; Program-supplied error message
	call _os_print_string

	call _os_print_newline

	mov si, .msg_prompt		; Restart prompt
	call _os_print_string

	mov ax, 0
	mov ah, 00			; BIOS call to wait for key
	int 16h

	jmp _os_int_reboot


	.msg_inform		db '>>> FATAL OPERATING SYSTEM ERROR', 13, 10, 0
	.msg_prompt		db 'Press a key to restart TomOS...', 0


; -----------------------------------------------------------------
; @proc os_get_file_list
; Generate comma-separated string of files on floppy
;
; @param_in BX - location of string to store filenames
; @section "files"

_os_get_file_list:
	pusha

        mov ax, bx

	mov word [.file_list_tmp], ax	; Store string location

	mov eax, 0			; Needed for some older BIOSes

	call _os_int_reset_floppy	; Just in case disk was changed
	jnc .floppy_ok			; Did the floppy reset OK?

	mov bx, .err_msg_floppy_reset	; If not, bail out
	jmp _os_fatal_error


.floppy_ok:				; Ready to read first block of data
	mov ax, 19			; Root dir starts at logical sector 19
	call _os_int_l2hts

	lea si, [os_buffer]		; ES:BX should point to our buffer
	mov bx, si

	mov ah, 0x02			; Params for int 13h: read floppy sectors
	mov al, 14			; And read 14 of them

	pusha				; Prepare to enter loop


.read_root_dir:
	popa
	pusha

	stc
	int 13h				; Read sectors
	call _os_int_reset_floppy	; Check we've read them OK
	jnc .show_dir_init		; No errors, continue

	call _os_int_reset_floppy	; Error = reset controller and try again
	jnc .read_root_dir
	jmp .done			; Double error, exit 'dir' routine

.show_dir_init:
	popa

	mov ax, 0
	mov si, os_buffer		; Data reader from start of filenames

	mov word di, [.file_list_tmp]	; Name destination buffer


.start_entry:
	mov al, [si+11]			; File attributes for entry
	cmp al, 0Fh			; Windows marker, skip it
	je .skip

	test al, 10h			; Is this a directory entry?
	jnz .skip			; Yes, ignore it

	mov al, [si]
	cmp al, 229			; If we read 229 = deleted filename
	je .skip

	cmp al, 0			; 1st byte = entry never used
	je .done


	mov cx, 1			; Set char counter
	mov dx, si			; Beginning of possible entry

.testdirentry:
	inc si
	mov al, [si]			; Test for most unusable characters
	cmp al, ' '			; Windows sometimes puts 0 (UTF-8) or 0FFh
	jl .nxtdirentry
	cmp al, '~'
	ja .nxtdirentry

	inc cx
	cmp cx, 11			; Done 11 char filename?
	je .gotfilename
	jmp .testdirentry


.gotfilename:				; Got a filename that passes testing
	mov si, dx			; DX = where getting string
	mov cx, 11
	rep movsb
	mov ax, ','			; Use comma to separate for next file
	stosb

.nxtdirentry:
	mov si, dx			; Start of entry, pretend to skip to next

.skip:
	add si, 32			; Shift to next 32 bytes (next filename)
	jmp .start_entry


.done:
	dec di				; Zero-terminate string
	mov byte [di], 0		; Gets rid of final comma

	popa
	ret


	.file_list_tmp	dw 0
	.err_msg_floppy_reset	db 'os_get_file_list: Floppy failed to reset', 0


; -----------------------------------------------------------------
; @proc os_filename_convert
; Change 'TEST.BIN' into 'TEST    BIN' as per FAT12
;
; @param_in SI - original filename string
; @param_in DI - location of 12-char string to place the result (zero-terminated)
; @param_out  new filename in BX, or carry set if filename invalid
; @section "files"

; XXX

_os_filename_convert:
	pusha

	mov word [.source_string], si	; Save for using later
	mov word [.dest_string], di

	mov bx, si
	call _os_string_length
	cmp ax, 12			; Bigger than name + dot + extension?
	jg .failure			; Fail if so

	cmp ax, 0
	je .failure			; Similarly, fail if zero-char string

	mov dx, ax			; Store string length for now

	mov cx, 0
.copy_loop:
	lodsb
	cmp al, '.'
	je .extension_found
	stosb
	inc cx
	cmp cx, dx
	jg .failure			; No extension found = wrong
	jmp .copy_loop

.extension_found:
	cmp cx, 0
	je .failure			; Fail if extension dot is first char

	cmp cx, 8
	je .do_extension		; Skip spaces if first bit is 8 chars

	; Now it's time to pad out the rest of the first part of the filename
	; with spaces, if necessary

.add_spaces:
	mov byte [di], ' '
	inc di
	inc cx
	cmp cx, 8
	jl .add_spaces


	; Finally, copy over the extension
.do_extension:
	lodsb
	stosb
	lodsb
	stosb
	lodsb
	stosb

	mov byte [di], 0		; Zero-terminate filename

	popa
	clc				; Clear carry for success
	ret


.failure:
	popa
	stc				; Set carry for failure
	ret


	.source_string	dw 0
	.dest_string	dw 0



; -----------------------------------------------------------------
; @proc os_load_file
; Load file into bottom-half of TomOS RAM
;
; @param_in  SI - location of filename
; @param_in  DI - location in RAM to load file
; @param_out BX - file size (in bytes), carry set if program not found on the disk or too big
; @section "files"
; @note Must be 32K or less, including load offset address)
;       Based on free bootloader code by E Dehling

_os_load_file:
        mov ax, si
        mov cx, di
	mov [.filename_loc], ax		; Store filename location
	mov [.load_position], cx	; And where to load the file!

        mov bx, os_buffer
        call _os_fat_get_root_directory
        test ax, ax
        jz .root_problem

	mov cx, word 224		; Search all entries in root dir
	mov bx, -32			; Begin searching at offset 0 in root dir

.next_root_entry:
	add bx, 32			; Bump searched entries by 1 (offset + 32 bytes)
	lea di, [os_buffer+bx]		; Point root dir at next entry

	mov al, [di]			; First character of name

	cmp al, 0			; Last file name already checked?
	je .root_problem

	cmp al, 229			; Was this file deleted?
	je .next_root_entry		; If yes, skip it

	mov al, [di + FAT12_83FILEFORMAT.Attributes]	; Get the attribute byte

	cmp al, 0Fh			; Is this a special Windows entry?
	je .next_root_entry

	test al, 10h			; Is this a directory entry?
	jnz .next_root_entry

	mov si, di			; Convert root buffer name to upper case
        mov cx, 11
	call _os_string_n_uppercase

	mov si, [.filename_loc]		; DS:SI = location of filename to load

        mov cx, 11
	call _os_string_n_compare		; Current entry same as requested?
	jc .found_file_to_load

	loop .next_root_entry

.root_problem:
	mov bx, 0			; If file not found or major disk error,
	stc				; return with size = 0 and carry set
	ret


.found_file_to_load:			; Now fetch cluster and load FAT into RAM
	mov ax, [di + FAT12_83FILEFORMAT.Size]			; Check size will fit in available RAM
	mov word [.file_size], ax
	mov dx, [di + FAT12_83FILEFORMAT.Size + 2]
	add ax, [.load_position]	; Add starting location
	adc dx, 0
	mov cx, [boot_record + BOOT_RECORD_STRUC.BytesPerSector]			; Round up to next sector size
	add ax, cx
        dec ax
	adc dx, 0
	div cx				; AX = number of sectors to load
	cmp ax, 64			; Maximimum sectors that will fit
        ja .file_too_big

.initial_fat_read:
	mov ax, [di + FAT12_83FILEFORMAT.FirstClusterLow]			; Now fetch cluster and load FAT into RAM
	mov word [.cluster], ax

        mov bx, boot_record
        mov dx, 1
        mov si, os_buffer
        mov di, si
        call _os_fat_get_FAT_table
        test ax, ax
        jz .root_problem
        

.load_file_sector:
	mov dx, word [.cluster]		; Convert sector to logical
        mov bx, boot_record
        call _os_fat_get_first_sector_of_cluster

	call _os_int_l2hts		; Make appropriate params for int 13h

	mov bx, [.load_position]


	mov ah, 02			; AH = read sectors, AL = just read 1
	mov al, 01

	stc
	int 13h
	jnc .calculate_next_cluster	; If there's no error...

	call _os_int_reset_floppy	; Otherwise, reset floppy and retry
	jnc .load_file_sector

	mov bx, .err_msg_floppy_reset	; Reset failed, bail out
	jmp _os_fatal_error


.calculate_next_cluster:
	mov dx, [.cluster]
     
        call _os_fat_calculate_next_cluster

	mov word [.cluster], ax		; Store cluster

	cmp ax, 0x0FF8
	jae .end

	add word [.load_position], 512
	jmp .load_file_sector		; Onto next sector!


.end:
	mov bx, [.file_size]		; Get file size to pass back in BX
	clc				; Carry clear = good load
	ret


.file_too_big:				; AX = integer number of sectors
	mov si, .err_msg_big_file
	call _os_print_string
	mov bx, .string_buff
        mov dx, ax
	call _os_int_to_string
	mov si, bx
	call _os_print_string
	call _os_print_newline
	mov bx, 0
	stc
	ret


	.bootd		db 0 		; Boot device number
	.cluster	dw 0 		; Cluster of the file we want to load
	.pointer	dw 0 		; Pointer into os_buffer, for loading 'file2load'

	.filename_loc	dw 0		; Temporary store of filename location
	.load_position	dw 0		; Where we'll load the file
	.file_size	dw 0		; Size of the file

	.string_buff	times 12 db 0	; For size (integer) printing

	.err_msg_floppy_reset	db 'os_load_file: Floppy failed to reset', 0
	.err_msg_big_file	db 'os_load_file: File too big to fit in available RAM: ', 0



; -----------------------------------------------------------------
; @proc os_execute_program
; Run code loaded at 100h in RAM (current CS)
;
; @param_in  BX = 1 if screen is to be cleared first, otherwise 0
; @param_out Nothing (registers may be corrupt)
; @section "system"

_os_execute_program:
	cmp bx, 1
	jne .run_program

	call _os_clear_screen


.run_program:

	; The following four lines set up a very basic Program Segment Prefix,
	; aka PSP, which provides some information for DOS programs. For
	; instance, CD 20 = 'int 20h', or 'return to DOS' -- a program can
	; use this code to quit

	mov byte [0], 0xCD		; int 20h
	mov byte [1], 0x20
	mov byte [2], 0xA0		; Always 0xA000 for COM executables
	mov byte [3], 0x00


	pusha                           ; Save all registers and stack pointer
	push ds
	push es
	pushf				; Save flag register for retf
	mov [.mainstack], sp
	xor ax, ax			; Clear registers to be DOS compatible
	xor bx, bx
	xor cx, cx
	xor dx, dx
	xor si, si
	xor di, di
	xor bp, bp
	mov byte [now_run_a_program], 1

	call 0x0100			; Jump to newly-loaded program!

.end_the_program:			; End of the program run
	mov byte [now_run_a_program], 0
	mov sp, [.mainstack]		; Restore stack, segment and...
	popf
	pop es				; ...common registers
	pop ds
	popa

        call _os_mouse_disable          ; disable a mouse just in case program didnt do it

	clc

	ret


        .mainstack dw 0
         now_run_a_program db 0


; -------------------------------------------------------------
; @proc os_memory_set
; Set block of memory.
;
; @param_in  DL - character to set
; @param_in  CX - number of bytes
; @param_in ES:DI - memory address
; @param_out nothing
; @section "memory"
      
_os_memory_set:
    push cx
    push di
    .memset_loop:
        test cx ,cx
        jz .memset_exit
        mov byte [es:di], dl
        inc di
        dec cx
    jmp .memset_loop
    .memset_exit:
    pop di
    pop cx
    ret

; -------------------------------------------------------------
; @proc os_memory_copy
; Copy block of bytes.
;
; @param_in ES:DI - desination
; @param_in DS:SI - source
; @param_in CX - number of byte
; @param_out Nothing
; @section "memory"

_os_memory_copy:
    push bx
    push dx
    mov bx, cx

    .copy_loop:
        mov dl, byte [ds:si+bx-1]
        mov byte [es:di+bx-1], dl
        dec bx
        test bx, bx
    jnz .copy_loop
    
    pop dx
    pop bx
    ret




; =================================================================
; INTERNAL OS ROUTINES -- Not accessible to user programs

; -----------------------------------------------------------------
; @proc os_int_reboot
; Reboot machine via keyboard controller
;
; @param_in Nothing
; @param_out Nothing
; @section "system"

_os_int_reboot:
	; XXX -- We should check that keyboard buffer is empty first
	mov al, 0xFE
	out 0x64, al


; -----------------------------------------------------------------
; @proc _os_int_restet_floppy
; Reset floppy drive
;
; @param_in Nothing
; @param_out Nothing
; @section "floppy"

_os_int_reset_floppy:
	push ax
	push dx
	xor ax, ax
	mov dl, 0
	stc
	int 13h
	pop dx
	pop ax
	ret


; -----------------------------------------------------------------
; @proc _os_int_l2hts
; Calculate head, track and sector settings for int 13h
; Convert floppy sector from logical to physical
;
; @param_in AX = logical sector; OUT: correct regs for int 13h
; @param_out Nothing
; @section "floppy"

_os_int_l2hts:
	
	push bx
	push ax

	mov bx, ax			; Save logical sector

	xor dx, dx			; First the sector
	div word [.sectors_per_track]
	add dl, 01h			; Physical sectors start at 1
	mov cl, dl			; Sectors belong in CL for int 13h
	mov ax, bx

	xor dx, dx			; Now calculate the head
	div word [.sectors_per_track]
	xor dx, dx
	div word [.sides]
	mov dh, dl			; Head/side
	mov ch, al			; Track

	pop ax
	pop bx

	mov dl, 0			; Boot device = 0

	ret


	.sectors_per_track	dw 18	; Floppy disc info
	.sides			dw 2


; =================================================================


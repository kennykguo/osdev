; =================================================================
; TomOS -- The Mike Operating System kernel
; Copyright (C) 2006 - 2008 TomOS Developers -- see doc/LICENSE.TXT
;
; This is loaded from floppy, by BOOTLOAD.BIN, as MIKEKERN.BIN.
; First section is 32K of empty space for program data (which we
; can load from disk and execute). Then we have the system call
; vectors, which start at a static point for programs to jump to.
; Following that is the main kernel code and system calls.
; =================================================================


	BITS 16

        %ifndef TOMOS_VER
	    %DEFINE TOMOS_VER '0.1'
        %endif

	%DEFINE TOMOS_API_VER 7
        %DEFINE TOMOS_KERNEL_SEGMENT 02000h
        %DEFINE NASM_VERSION __NASM_VER__
        %XDEFINE COMP_TIME __DATE__
        %XDEFINE COMP_DATE __TIME__

        %ifndef KERNELNAME
            %DEFINE KERNELNAME 'TOMOS.BIN'
        %endif

        %ifndef KERNELNAME_FAT12
            %DEFINE KERNELNAME 'TOMOS   BIN'
        %endif

        %ifndef EMAIL
            %DEFINE KERNELNAME 'gavian@tlen.pl'
        %endif

        %INCLUDE 'tomos.inc'


; -----------------------------------------------------------------
; Program data section -- Pad out for app space (DO NOT CHANGE)

os_app_data:
	times (2000h)-($-$$)	db 0	; 32K of program space

; =================================================================
; START OF MAIN KERNEL CODE

os_main:
	cli				; Clear interrupts
	mov ax, 0
	mov ss, ax			; Set stack segment and pointer
	mov sp, 0xF000
	sti				; Restore interrupts

	cld				; The default direction for string operations
					; will be 'up' - incrementing address

	mov ax, 0x2000
	mov ds, ax			; Set segments to where we loaded
	mov es, ax
	mov fs, ax


	mov cx, 00h                     ; Divide by 0 error handler
	mov si, os_compat_int00
	call _os_modify_int_handler

	mov cx, 20h			; Set up DOS compatibility...
	mov si, os_compat_int20		; ...for interrupts 20h and 21h
	call _os_modify_int_handler

	mov cx, 21h
	mov si, os_compat_int21
	call _os_modify_int_handler

        call _os_int47h_setup

        call _os_mm_init

        call _os_fat_setup
        test ax, ax
        jnz .next
        
	mov si , kerndlg_string_1	
        call _os_print_string
        jmp $

        .next: 


	mov dx, 0			; Configure serial port 1
	mov al, 11100011b		; 9600 baud, no parity, 8 data bits, 1 stop bit
	mov ah, 0
	int 14h

	mov ax, 1003			; Set to normal (80x25 text) video mode
	mov bx, 0			; with text intensity (no blinking)
	int 10h


        call _os_mouse_init

        call _os_mouse_disable


.redraw_select:
	mov dx, os_init_msg		; Set up screen
	mov bx, os_version_msg
	mov cx, 10011111b		; White text on light blue
	call _os_draw_background

	mov si, dialog_string_1		; Ask if user wants app selector or CLI
	mov bx, dialog_string_2
	mov di, dialog_string_3
	mov dx, 1
	call _os_dialog_box

	cmp ax, 1			; If Cancel selected, start CLI
	je .command_line

.more_apps:
	call app_selector		; Otherwise show program menu

	call _os_clear_screen		; When program finished, clear screen
	jmp .redraw_select		; and offer command line or menu again


.command_line:
	call os_command_line

	jmp .redraw_select		; Offer menu after exiting from CLI



	os_init_msg		db 'Welcome to TomOS', 0
	os_version_msg		db 'Version ', TOMOS_VER, 0

	dialog_string_1		db 'Thanks for trying out TomOS!', 0
	dialog_string_2		db 'Contact ',EMAIL,' for more info.', 0
	dialog_string_3		db 'OK for program selector, Cancel for CLI.', 0


app_selector:
	mov dx, os_init_msg		; Redraw screen to remove dialog box
	mov bx, os_version_msg
	mov cx, 10011111b
	call _os_draw_background

	call _os_file_selector		; Get user to select a file, and store
					; the resulting string in AX (other registers are undetermined)

	jc .done			; Return to the CLI / menu choice screen if Esc pressed

	mov si, ax			; User tried to run TOMOS KERNEL?
	mov di, kern_file_name
	call _os_string_compare
	jc no_kernel_execute


	mov di, 100h			; Where to load the file (.COM and our .BIN)

	call _os_load_file		; Load filename pointed to by AX
	mov bx, 1			; Clear screen before running!
	call _os_execute_program		; And execute the code!


	mov dh, 22			; Move below selection block
	mov dl, 0
	call _os_cursor_move

	mov si, prog_finished_string	; When finished, print a message
	call _os_print_string

	call _os_kbd_wait_for_key

.done:
	ret				; And go back to the prog list



no_kernel_execute:			; Warning about executing kernel!
	mov dx, os_init_msg		; Redraw screen to remove file selector
	mov bx, os_version_msg
	mov cx, 10011111b
	call _os_draw_background

	mov dx, 0			; One button for dialog box
	mov si, kerndlg_string_1
	mov bx, kerndlg_string_2
	mov di, kerndlg_string_3
	call _os_dialog_box

	jmp app_selector		; Start over again...


	prog_finished_string	db 10, 13, '>>> Program has terminated - press a key', 10, 13, 0

	kern_file_name		db KERNELNAME_FAT12, 0

	kerndlg_string_1	db 'Cannot load and execute TomOS kernel!', 0
	kerndlg_string_2	db KERNELNAME,' is the core of TomOS, and', 0
	kerndlg_string_3	db 'is not a normal program.', 0

        kernellg_string_3       db "Can't load disk description table!",10,13,0

        dirn db 'DIRO       ',0
      
        filent times  SIZE_OF_FAT12_83FILEFORMAT db 0

        doter times  13 db 0


; =================================================================
; SYSTEM VARIABLES -- Settings for programs and system calls


	; Time and date formatting

	fmt_12_24 db 0		; Non-zero = 24-hr format

	fmt_date  db 0, '/'	; 0, 1, 2 = M/D/Y, D/M/Y or Y/M/D
				; Bit 7 = use name for months
				; If bit 7 = 0, second byte = separator character


; =================================================================
; SYSTEM CALL SECTION -- Accessible to user programs


        %INCLUDE "syscalls.asm"
        %INCLUDE "os_fat.asm"


; =================================================================
; TEST ZONE -- A place to try out new code without changing the OS


        %INCLUDE "testzone.asm"


; =================================================================
; COMMAND LINE INTERFACE


	%INCLUDE "os_cli.asm"


; =================================================================
; DOS COMPATIBILITY INTERRUPT HANDLERS


	%INCLUDE "os_dos.asm"

; =================================================================
; DOS COMPATIBILITY MOUSE INTERRUPT HANDLERS


	%INCLUDE "os_mouse.asm"
	%INCLUDE "os_mouse_syscalls.asm"

; =================================================================
; 
        %INCLUDE "os_int47h.asm"
        %INCLUDE "os_mm.asm"

; =================================================================
; END OF KERNEL

	times 57344-8192-($-$$)	db 0		; Pad up to 56K



os_buffer:
	times 8192		db 0		; 8K is kernel buffer (uses by kernel syscalls)


mm_buffer:
	times 8192		db 0		; Final 8K is generic buffer

; =================================================================


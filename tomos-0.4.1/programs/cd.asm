; ---------------------------------------------------------------
; 'cd' program for TomOS 
; 
; Author: Tomasz Gorol
;
; To find out more about writing applications for TomOS
; see doc/examples.html
; ---------------------------------------------------------------

CPU 386
BITS 16
ORG 100h

%include '../include/tomos.inc'

start:

    syscall os_command_line_get   ; get command line arguments

    mov si, 0FFFFh
    cmp ax, 2               ; if number of argumens == 1 
    jb .go_to_root          ; go to root
    je .go_to_arg           ; if 2, the secorn arg is a file path
    jmp .too_many_arguments ; if more error

    .go_to_arg:

  
        mov bx, di
        syscall os_string_length   ; get length of first arg (name of the executed program)

        inc ax
        add bx, ax
        mov si, bx        ; jump to the next argumnet 

        mov di, .fat_format
        call os_file_convert_d2f    ; it should be a directory name so convert ot FAT12 format

        mov si, di

    .go_to_root:

    syscall os_dir_change      ; change a directory
    test ax, ax
    jz .change_error           ; if error jump to .change_error

    xor ax, ax                 ; return 0
    ret

    .too_many_arguments:

    mov si, .too_many_args_str
    syscall os_print_string    ; print a message about too many arguments
    xor ax, ax
    inc ax
    ret

    .change_error:
    mov si, .change_error_str
    syscall os_print_string  ; print a message about changing directory error
    xor ax, ax
    inc ax
    ret
  

.too_many_args_str db 'too many arguments.',10,13,0
.change_error_str db 'no such directory.',10,13,0

.fat_format db 'DIRO       ',0

; end of program

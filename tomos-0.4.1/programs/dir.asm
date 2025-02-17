; ---------------------------------------------------------------
; 'Dir' program for TomOS 
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

FILENAME_RECORD_WIDTH equ 20

start:

   
    mov si, 0FFFFh 
    syscall os_dir_open        ; open directory 
    test ax, ax
    jnz .success
        mov si, msg_error
        syscall os_print_string
        xor ax, ax
        inc ax
        ret
    .success:

    mov word [dir_handle + far_address.offset], di    ; save ES and DI for later
    mov ax, es
    mov word [dir_handle + far_address.segment], ax

    .loop:

    push cs
    pop ds
   
    mov si, file_struc 

    mov di, word [dir_handle + far_address.offset]
    mov ax, word [dir_handle + far_address.segment]
    mov es, ax

    call os_dir_next_file           ; get a file information from opened directory
    test ax, ax                     ; of return 0 no more files get so exit program
    jz .exit
    inc ax
    test ax, ax
    jnz .success_file
        mov si, msg_error
        syscall os_print_string
        xor ax, ax
        inc ax
        ret
    .success_file:
        
        mov di,  dot_name
        push cs
        pop es

        syscall os_file_convert_f2d    ; convert FAT12 filename format to name.ext


        IS_DIRECTORY(DS:SI)            ; check if directory 
        jz .directory
           mov di, size_str
           mov bx, 10
	   mov cx, word [si + FAT12_ENTITY.Size]
	   mov dx, word [si + FAT12_ENTITY.Size+2]
        
           syscall os_long_int_to_string       ; change size (long int) to string

           mov word [pt_size], di
           jmp .format_record
        .directory:
           
           mov word [pt_size], directory_str   ; if directory returned dont show size, show <DIR>

        .format_record:
        mov bx, size_str

        mov bx, dot_name

        syscall os_string_length         ; get length of filename in bytes

        sub ax, FILENAME_RECORD_WIDTH    
        neg ax
        mov cx, ax                 ; in cx FILENAME_RECORD_WIDTH - lenght

        call pad_with_space        ; fille space pad with spaces, number of spaces in cx

        mov si, dot_name
        syscall os_print_string    ; print filemane.ext
  
        mov si, space_pad
        syscall os_print_string    ; print spaces

        mov si, word [pt_size]
        syscall os_print_string    ; print size/<DIR>
        
        syscall os_print_newline   ; print end line
     
    jmp .loop

    .exit:

    mov di, word [dir_handle + far_address.offset]
    mov ax, word [dir_handle + far_address.segment]
    mov es, ax

    syscall os_dir_close

    xor ax, ax
    ret




; -------------------------------------------------------------
; pad_with_space - fills 'space_pad' with spaces, number of
;                  spaces in CX

pad_with_space:
   push ax
   push di
   push es
   push cx

   cmp cx, FILENAME_RECORD_WIDTH      ; if cx > FILENAME_RECORD_WIDTH
   jbe .width_ok
       mov cx, FILENAME_RECORD_WIDTH  ; load cx with FILENAME_RECORD_WIDTH
   .width_ok:

   mov di, space_pad      ; memory to fill with ' '
   push cs
   pop es

   mov dl, ' '            ; character to write is ' '

   call os_memory_set     ; fill it

   mov bx , cx
   mov byte [di + bx], 0  ; terminate with NULL


   pop cx
   pop es
   pop di
   pop ax

   ret


space_pad times FILENAME_RECORD_WIDTH+1 db 0
file_struc times SIZE_OF_FAT12_ENTITY db 0
dot_name times 13 db 0
size_str times 13 db 0
msg_error db 'error',13,10,0

pt_size dw 0

directory_str db '<DIR>',0
dir_handle times 2 dw 0


; end of program

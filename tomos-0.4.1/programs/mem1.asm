; ---------------------------------------------------------------
; Memory program for TomOS
;
; To find out more about writing applications for TomOS
; see doc/examples.html
; ---------------------------------------------------------------

CPU 386
BITS 16
ORG 100h
	
%include '../include/tomos.inc'

start:
    mov dx , 128
    syscall os_mm_alloc    ; allocate 128 bytes
    test ax , ax
    jz .exit
    SAVE_FAR_PTR .mem1     ; if allocation succesfull save the returned pointer

; bit of code copys the  string to the allocated memory and prints it from there.

    mov si, .text         
    syscall os_string_copy  ; copy string

    mov di, si
    syscall os_print_string  ; print the string

    LOAD_FAR_PTR .mem1
    syscall os_mm_free     ; frees the allocated memory

    .exit:
    ret
	

FAR_PTR .mem1
.text db "Hello World !!!!!!!!!!",13,10,0    ; string to copy

; end of program

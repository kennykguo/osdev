; ---------------------------------------------------------------
; Hello World program for TomOS
;
; To find out more about writing applications for TomOS
; see doc/examples.html
; ---------------------------------------------------------------

CPU 386
BITS 16
ORG 100h

%include '../include/tomos.inc'

start:
    mov si , .hello_world         ; address of the strng to print goes to the SI register
    mov ax , ID_os_print_string   ; load the AX register with the ID of os_print_string
    int OSINT                     ; call the tomos interrupt
    ret
	
.hello_world db 'Hello World!',13,10,0    ; string to print

; end of program

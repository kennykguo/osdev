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
    mov si , .hello_world       ; address of the strng to print goes to the SI register
    syscall os_print_string     ; print the string
    ret                         ; return 
	

.hello_world db 'Hello World!',13,10,0    ; string to print
	
; end of program

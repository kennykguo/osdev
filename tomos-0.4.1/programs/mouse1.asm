; ---------------------------------------------------------------
; Mouse program for TomOS
;
; To find out more about writing applications for TomOS
; see doc/examples.html
; ---------------------------------------------------------------

CPU 386
BITS 16
ORG 100h
	
%include '../include/tomos.inc'

start:
    syscall os_mouse_enable          ; enable a mouse
    cmp ax , 0FFFFh
    jz .exit                         ; if enabling error exit the program
        syscall os_mouse_show        ; show the cursor on the screen
        syscall os_kbd_wait_for_key  ; wait for a key to be pressed
        syscall os_mouse_disable     ; disable the mouse
    .exit:

    ret                              ; exit the program

; end of program

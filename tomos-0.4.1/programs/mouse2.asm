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
    syscall os_mouse_enable                ; enable a mouse
    cmp ax , 0FFFFh
    jz .exit                               ; if enabling failed exit the program
       
        mov dx, mouse_user_handler         ; loads the DX reg. with address of mouse handler
        mov cx, MOUSE_EVENT_LEFT_PRESSED   ; loads the CX reg. with events handled by the handler
        call os_mouse_set_user_subroutine  ; set the mouse user subroutine
 
        syscall os_mouse_show              ; show the mouse

        mov word [exit_flag], 0            ; reset exit_flag

        .loop:
        mov ax, word [exit_flag]           ; loads exit_flag to the AX reg.
        test ax, ax                        ; if exit_flag eq 0 wait
        jz .loop                           ; otherwise exit the program

        syscall os_mouse_disable           ; disable the mouse
    .exit:

    ret

mouse_user_handler:
    test ax, MOUSE_EVENT_LEFT_PRESSED     ; check if the left mouse button pressed 
    jz .exit                              ; if no exit
        inc word [exit_flag]              ; if yes increments exit_flag
    .exit:
    retf                                  ; return from the handler

exit_flag dw 0

; end of program

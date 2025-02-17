%INCLUDE "os_mouse.asm"

; @description
; File contains system calls handling mouse

;------------------------------------------------------------
; @proc os_mouse_init
; Init mouse.
;
; @param_in Nothing
; @param_out AH - 0000h hardware/driver not installed
; @param_out AH - FFFFh hardware/driver installed, BX - number of buttons (TomOS supports only 2 so far)
; @section "mouse"


_os_mouse_init:

    cmp byte [installed], 0
    jnz .exit
    mov si, os_mouse_int33
    mov cx, 33h
    call _os_modify_int_handler

    xor ax, ax
    int 33h
    ret

    .exit:
    mov ax, 0FFFFh
    ret

 

;------------------------------------------------------------
; @proc os_mouse_show
; Show mouse cursor.
;
; @param_in Nothing
; @param_out Nothing
; @section "mouse"

_os_mouse_show:
    mov ax, 1
    int 33h
    ret

;------------------------------------------------------------
; @proc os_mouse_hide
;  Hide mouse cursor.
;
; @param_in Nothing
; @param_out Nothing
; @section "mouse"

_os_mouse_hide:
   mov ax, 2
   int 33h
   ret


;------------------------------------------------------------
; @proc os_mouse_get_position
; Get cursor position.
;
; @param_in  Nothing
; @param_out BX - button states (see #1)
; @param_out CX - horizontal position (row)
; @param_out DX - vertical position (column)
; @section "mouse"
; @note
;   (#1) Button states: (defined in include/tomos.inc)
;   MOUSE_FLAG_LEFT_PRESSED (equals 0) - left button pressed if 1
;   MOUSE_FLAG_RIGHT_PRESSED (equals 1) - right button pressed if 1
      
_os_mouse_get_position:
    mov ax, 3
    int 33h
    ret

; -------------------------------------------------------------------
; @proc os_mouse_set_position
; Set cursor position.
;
; @param_in  CX - column
; @param_in DX - row
; @section "mouse"

_os_mouse_set_position:
    mov ax, 4
    int 33h
    ret

; ---------------------------------------------------------------------
; @proc os_mouse_get_bt_press_info
; Get mouse button press information.
;
; @param_in  BX - button number (see #1)
; @param_out AX - button states (see #2)
; @param_out BX - number of times specified button has been pressed since last call
; @param_out CX - column at time specified button was last pressed
; @param_out DX - row at time specified button was last pressed
; @section "mouse"
; @note
;   (#1)Button states: (defined in include/tomos.inc)
;   MOUSE_LEFT_BUTTON (equals 0) - left button.
;   MOUSE_RIGHT_BUTTON (equals 1) - right button.
;   (#2) Values for mouse button number (BX):
;    MOUSE_STATE_LEFT_BUTTON (bit 0) - value in BX coresponds to a left button.
;    MOUSE_STATE_RIGHT_BUTTON (bit 1) - value in BX coresponds to a right button.

_os_mouse_get_bt_press_info:
    mov ax, 5 
    int 33h
    ret

; ---------------------------------------------------------------------
; @proc os_mouse_get_bt_release_info
; Get mouse button press information.
;
; @param_in  BX - button number (see #1)
; @param_out AX - button states (see #2)
; @param_out BX - number of times specified button has been pressed since last call
; @param_out CX - column at time specified button was last pressed
; @param_out DX - row at time specified button was last pressed
; @section "mouse"
; @note
;   (#1)Button states: (defined in include/tomos.inc)
;   MOUSE_LEFT_BUTTON (equals 0) - left button.
;   MOUSE_RIGHT_BUTTON (equals 1) - right button.
;   (#2) Values for mouse button number (BX):
;    MOUSE_STATE_LEFT_BUTTON (bit 0) - value in BX coresponds to a left button.
;    MOUSE_STATE_RIGHT_BUTTON (bit 1) - value in BX coresponds to a right button.

_os_mouse_get_bt_release_info:
    mov ax, 6 
    int 33h
    ret

; --------------------------------------------------------------
; @proc os_mouse_set_h_limit
; Set mouse horizontal min/max position.
;
; @param_in  CX - minimum column
; @param_in DX - maximum column
; @param_out Nothing
; @section "mouse"
; @note 
;     When minimum is greater than maximum it swaps them

_os_mouse_set_h_limit:
    mov ax, 7
    int 33h
    ret

; --------------------------------------------------------------
; @proc os_mouse_set_v_limit
; Set mouse vertical min/max position.
;
; @param_in CX - minimum row
; @param_in DX - maximum row
; @param_out Nothing
; @section "mouse"
; @note
;     When minimum is greater than maximum it swaps them

_os_mouse_set_v_limit:
    mov ax, 8
    int 33h
    ret

; ---------------------------------------------------------------
; @proc os_mouse_set_text_cursor
; Sets mouse cursor.
;
; @param_in CX - screen mask
; @param_in DX - cursor mask
; @param_out  Nothing
; @section "mouse"
; @note 
; Character/attribute data at the current screen position is ANDed with the screen mask 
;       and then XORed with the  cursor mask 

_os_mouse_set_text_cursor:
    mov ax, 0Ah
    int 33h
    ret

; ------------------------------------------------------------------
; @proc os_mouse_set_user_subroutine
; Set Mouse User Defined Subroutine and Input Mask.
;
; @param_in CX - call mask (see #4)
; @param_in ES:DX - FAR routine (see #5)
; @param_out  Nothing
; @section "mouse"
; @note
;  The driver calls a user subroutine if one of the events occurs specified int call mask.
;  (#4) Bitfields for mouse call mask:
;    defined in include/tomos.inc.
;    Bit(s) Description 
;    MOUSE_EVENT_MOVED (bit 0) call if mouse moves 
;    MOUSE_EVENT_LEFT_PRESSED (bit 1) call if left button pressed 
;    MOUSE_EVENT_LEFT_RELEASED(bit 2) call if left button released 
;    MOUSE_EVENT_RIGHT_PRESSED (bit 3) call if right button pressed 
;    MOUSE_EVENT_RIGHT_RELEASED (bit 4) call if right button released 
;    7-15 unused
;  (#5) Values interrupt routine is called with:. 
;    AX = condition mask (same bit assignments as call mask). 
;    BX = button state. 
;    CX = cursor column. 
;    DX = cursor row. 
;    SI = horizontal mickey count. 
;    DI = vertical mickey count 

_os_mouse_set_user_subroutine:
    mov ax, 0Ch
    int 33h
    ret

;---------------------------------------------------------------
; @proc os_mouse_disable
; Disable mouse driver.
;
; @param_in Nothing
; @param_out AX - status 001Fh successful
; @param_out AX - status FFFFh unsuccessful
; @section "mouse"
; @note
; Restores vectors for INT 10 and INT 74

_os_mouse_disable:
    mov ax, 1Fh
    int 33h
    ret

; -------------------------------------------------------------------------------
; @proc os_mouse_enable
; Enable mouse driver.
;
; @param_in  Nothing
; @param_out AX - 0020h successful
; @param_out AX - FFFFh unsuccesful
; @section "mouse"
; @note 
;      Restores vectors for INT 10h and INT 74h
;      which were removed by function 1Fh.

_os_mouse_enable:
    mov ax, 20h
    int 33h
    ret


; ------------------------------------------------------------------------------
; @proc os_mouse_wait_for_l_click
; Waits for left mouse button click.
;
; @param_in Nothing
; @param_out AX - x postion of click
; @param_out BX - y position of click 
; @section "mouse"

_os_mouse_wait_for_l_click:
    push cx
    push dx
    .loop1:
    mov bx, 0
    call _os_mouse_get_bt_press_info
    and ax, 1
    jz .loop1    
    .loop2:
    mov bx, 0
    call _os_mouse_get_bt_press_info
    and ax, 1
    jnz .loop2
    mov bx, dx
    mov ax, cx
    pop dx
    pop cx
    ret

; --------------------------------------------------------------------------------
; H------------------------------------------------------------------------------
; os_mouse_soft_reset - software reset
; IN: Nothing
; OUT: AX - status FFFFh if mouse driver installed 
;         BX - number of buttons (supports only 2)
;      AX - status 0021h if mouse driver not installed
;
;Note: This call is identical to funtion 00h, but does not reset the mouse 
;os_mouse_soft_reset:
;    mov ax, 21h
;	int 33h
;	ret




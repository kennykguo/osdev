%ifndef TOMOS_MOUSE
%define TOMOS_MOUSE 1


%INCLUDE "tomos.inc"


; ----------------------------------------
; commands for a mouse
; ----------------------------------------
M_CMD_RESET                       equ 0FFh
M_CMD_RESEND                      equ 0FEh
M_CMD_SET_DEFAULTS                equ 0F6h
M_CMD_DISABLE_DATA_REPORTING      equ 0F5h
M_CMD_ENABLE_DATA_REPORTING       equ 0F4h
M_CMD_SET_SAMPLE_RATE             equ 0F3h
M_CMD_GET_DEVICE_ID               equ 0F2h
M_CMD_SET_REMOTE_MODE             equ 0F0h
M_CMD_SET_WRAP_MODE               equ 0EEh
M_CMD_RESET_WRAP_MODE             equ 0ECh
M_CMD_READ_DATA                   equ 0EBh
M_CMD_SET_STREAM_MODE             equ 0EAh
M_CMD_STATUS_REQUEST              equ 0E9h
M_CMD_SET_RESOLUTION              equ 0E8h
M_CMD_SET_SCALLING_21             equ 0E7h
M_CMD_SET_SCALLING_11             equ 0E6h
; ----------------------------------------
; values return by mouse
; ----------------------------------------
M_RET_ACKNOWLEGE                  equ 0FAh
M_RET_ERROR                       equ 0FCh
M_RET_RESEND                      equ 0EAh
M_RET_RESET_OK                    equ 0AAh

; -----------------------------------------
;   mouse resolutions
;-----------------------------------------
M_RES_1                           equ 0    ; 1mm per mickie
M_RES_2                           equ 1    ; 2mm per mickie
M_RES_4                           equ 2    ; 4mm per mickie
M_RES_8                           equ 3    ; 8mm per mickie
; ----------------------------------------
; keyboard controler commands
;-----------------------------------------
KC_CMD_READ_COMMAND_BYTE          equ 020h
KC_CMD_WRITE_COMMAND_BYTE         equ 060h
KC_CMD_DISABLE_MOUSE_INTERFACE    equ 0A7h
KC_CMD_ENABLE_MOUSE_INTERFACE     equ 0A8h
KC_CMD_MOUSE_INTERFACE_TEST       equ 0A9h
KC_CMD_CONTROLLER_SELF_TEST       equ 0AAh
KC_CMD_KEYBOARD_INTERFACE_TEST    equ 0ABh
KC_CMD_DISABLE_KEYBOARD_INTERFACE equ 0ADh
KC_CMD_ENABLE_KEYBOARD_INTERFACE  equ 0AEh
KC_CMD_GET_VERSION                equ 0AEh
KC_CMD_READ_INPUT_PORT            equ 0C0h
KC_CMD_COPY_INPUT_PORT_LSN        equ 0C1h
KC_CMD_COPY_INPUT_PORT_MSN        equ 0C2h
KC_CMD_READ_OUTPUT_PORT           equ 0D0h
KC_CMD_WRITE_OUTPUT_PORT          equ 0D1h
KC_CMD_WRITE_KEYBOARD_BUFFER      equ 0D2h
KC_CMD_WRITE_MOUSE_BUFFER         equ 0D3h
KC_CMD_WRITE_MOUSE_DEVICE         equ 0d4h
KC_CMD_READ_TEST_PORT             equ 0E0h

; ----------------------------------------------
; --- consts witch makes program better redable
; ----------------------------------------------

LEFT_BUTTON_PRESSED           equ 00000001b
RIGHT_BUTTON_PRESSED          equ 00000010b

MOUSE_DEFAULT_H_RATIO         equ 8
MOUSE_DEFAULT_V_RATIO         equ 16


; ---------------------------------------------------
; wait_ibf_kbd - waits for data from keyboard
; IN: Nothing
; OUT: Nothing

%macro wait_ibf_kbd 0
    %%wait:
    read64                ; read port 64h (control register)
    test al, 00000001b    ; check if we can write data to keyboard
    jz %%wait             ; if no wait
%endmacro

; ---------------------------------------------------
; wait_ibf_mouse - waits for data from mouse
; IN: Nothing
; OUT: Nothing

%macro wait_ibf_mouse 0
    %%wait:
    read64                ; read port 64h (control register)
    test al, 00100000b    ; check if we can write data to mouse
    jz %%wait             ; if no wait
%endmacro

; ---------------------------------------------------
; wait_ibf - waits for data from mouse/keyboard
; IN: Nothing
; OUT: Nothing
; NOTES: same as wait_ibf_kdb

%macro wait_ibf 0
    %%wait:
    read64              
    test al, 00000001b
    jz %%wait
%endmacro


; ---------------------------------------------------
; wait_obf - waits for output buffer
; IN: Nothing
; OUT: Nothing

%macro wait_obf 0
    push ax
    %%wait:
    read64                ; read port 64h (control register)
    test al, 00000010b    ; check if we can read from keyboard controler (output buffor full)
    jnz %%wait            ; if no wait
    pop ax
%endmacro

; ------------------------------------------------------------------------------
; write64 -  write byte to port 64h
; IN: data to write
; OUT: Nothing
; NOTE: overwrites AL

%macro write64 1
;   wait_obf
    mov al, %1
    out 64h, al          ; write port 64h
%endmacro

; ------------------------------------------------------------------------------
; read64 -  reads byte from port 64h
; IN: Nothing
; OUT: al - readed byte

%macro read64 0
    in al, 64h          ; read port 64h
%endmacro

; ------------------------------------------------------------------------------
; read60_kbd -  reads byte from keyboard
; IN: addres to jump if timeout
; OUT: al - readed byte

%macro read60_kbd 1
    wait_ibf_kbd        ; wait for data
    in al, 60h          ; read it from port 60h
%endmacro

; ------------------------------------------------------------------------------
; read60_mouse -  reads byte from mouse
; IN: arg1 - addres to jump if timeout
; OUT: al - readed byte

%macro read60_mouse 1
    wait_ibf_mouse     ; wait for data
    in al, 60h         ; read it from port 60h
%endmacro

; ------------------------------------------------------------------------------
; read60 -  reads byte from keyboard/mouse
; IN: arg1 - addres to jump if timeout
; OUT: al - readed byte

%macro read60 0
    wait_ibf           ; wait for data
    in al, 60h         ; read it from port 60h
%endmacro

; ----------------------------------------------------------------------------
; write60 - write byte to port 60h
; IN: arg1 - byte to write
; OUT: nothing

%macro write60 1
    wait_obf          ; wait until output buffor empty
    mov al, %1    
    out  60h, al      ; write data to port 60h
%endmacro

%define MAX 0
%define MIN 2

%define LEFT 0
%define RIGHT 2

; ----------------------------
; mouse driver procedure
; ----------------------------

driver_memory_block_start:

packet_size dw 0

enabled db 0
installed db 0

mouse_data times 4 db 0     ; memory for data read from the mouse

old_int_74 dw 0,0           ; old interupt 74h handler address
old_int_10 dw 0,0           ; old interupt 10h handler address

show_cursor_flag db -1      ; flag holds hide/show cursor state

h_limit dw 0,0              ; max, min
v_limit dw 0,0              ; max, min

h_counter dw 0
v_counter dw 0

h_screen dw 0               ; number of column on the screen
v_screen dw 0               ; number of rows on the screen

counter db 0                ; uses to determine how many bytes read from mouse

x_pos dw 0                  ; cursor hot spot horizontal position
y_pos dw 0                  ; cursor hot spot vertical position

h_moved db 0                ; flag indicates move on x
v_moved db 0                ; flag indicates move on y

button_status dw 0          ; store info about clicked buttons

h_pos_last dw 0             ; holds last horizontal position of the hot spot  
v_pos_last dw 0             ; holds last vertical position of the hor spot

pos_last_lin_addr dw 0      ;  xy2flat(h_pos_last, and v_pos_last)
pos_lin_addr dw 0           ;  xy2flat(x_pos, and y_pos)

h_pos_last_press dw 0, 0    ; last horizontal position when LEFT/RIGHT button pressed
v_pos_last_press dw 0, 0    ; last vertical position when LEFT/RIGHT button pressed
h_pos_last_release dw 0, 0  ; last horizontal position when LEFT/RIGHT button released
v_pos_last_release dw 0, 0  ; last vertical position when LEFT/RIGHT button released

button_press_number dw 0, 0    ; number of presses LEFT/RIGHT button since int_33_5 (Get Mouse Button Press Information) called
button_release_number dw 0,0   ; number of releases LEFT/RIGHT button since int_33_5 (Get Mouse Button Press Information) called


cursor_type dw 0               ; cursor type hardware/software (TomOS supports only software)
cursor_mask_and dw 0           ; cursor mask to and with bakcground
cursor_mask_xor dw 0           ; cursor mask to xor with anded background
gcursor times 16 db 0          ; NOT USED YET - will hold grafics cursor

soft_character times 2 db  0   ; attribute and character under the cursor bring back on the screen when mpuse moved

h_ratio dw 0              ; horizontal factor between mouse and screen
v_ratio dw 0              ; vertical factor between mouse and screen 

user_sub_routine dw 0,0       ; user subroutine addres OFFSET:SEGMENT
user_sub_routine_mask dw 0    ; user subroutine mask (user subroutine will be called if at least on event occur)
user_sub_routine_event dw 0   ; used by driver to hold occured event, then compared with uer_sub_routine_mask

delta_x dw 0                 
delta_y dw 0

save_bkg dw 0               ; used by driver to hold subroutine address which saves background under the cursor
paint_bkg dw 0              ; used by driver to hold subroutine address which paints background under the cursor

make_cursor dw 0            ; used by driver to hold subroutine address which makes visible cursor
make_invisible_cursor dw 0  ; used by driver to hold subroutine address which make invisible cursor
paint_cursor dw 0           ; used by driver to hold subroutine address which paints cursor on the screen

hide_cursor dw 0            ; REMOVE IN THE NEXT RELEASE - used by driver to hold subroutine address which hides cursor
show_cursor dw 0            ; REMOVE IN THE NEXT RELEASE - used by driver to hold subroutine address which shows cursor

xy2flat dw 0                ; used by driver to hold subroutine addres which translates xy coordinates into flat memory

mouseID db 0                ; mouse id (supports only 0 which means 2 button mouse)

driver_memory_block_stop:


; ------------------------------------------------------------------------------------------
; __mouse_driver - int 12h handler
; IN: Data from a mouse.
; OUT: Nothing

__mouse_driver:

    pusha                  ; save register to the stack

    xor cx, cx
    mov cl, byte [counter]
    mov si, cx

    in al, 64h				; get status register
    test al, 00100000b			; check if data from mouse
    jz .exit				; if not exit
    in al, 60h                          ; fetch data from the mouse

    mov byte [mouse_data+si], al        ; save it
    inc byte [counter] 

    cmp si, [packet_size]               ; check if last byte in packet
    jb near .exit                       ; if no exit int routine;

    xor ax, ax
    mov byte [counter] , al                    ; reset the counter
    mov word [user_sub_routine_event], ax      ; reset user_sub_routine_event flag

    call __mouse_translate_2_y                 ; get y cursor position on the screen
 
    call __mouse_translate_2_x                 ; get x cursor position on the screen
 
    call __mouse_set_button_state              ; get information about button state

    push word [pos_lin_addr]                   ; mov [pos_lin_addr] to  [pos_last_lin_addr]
    pop word [pos_last_lin_addr]
 
    mov ax, word [x_pos]
    mov bx, word [y_pos]
 
    call near [xy2flat]         ; translate xy into flat address
 
    mov word [pos_lin_addr] , ax                  ; save the flat address
 
    xor dx, dx
    cmp byte [show_cursor_flag], dl            ; check if cursor hidden
    jnz near .emove                            ; if yes skip moving
        mov al, byte [h_moved]
	or al, byte [v_moved]
	test al, al              ; check if moved 
	jz .emove                ; if no move just exit routine
            xor al, al
	    mov byte [h_moved], al
	    mov byte [v_moved], al
	    call __mouse_move_cursor                ; move cursor (supports only text mode int 10h ah 3)
    .emove:
     
    call __mouse_call_user_sub_routine                 ; call user subroutine
     
    .exit:

    mov al, 20h                                ; end int routine
    out 0A0h, al
    out 020h, al
    popa                                       ; get registers from the stack

    iret                                       ; return

; ----------------------------------------------------
; __mouse_set_button_state - sets buttons flag
; IN: Nothing
; OUT: Nothing

__mouse_set_button_state:
    pusha
        xor dx, dx
        mov dl, byte [mouse_data+0]            ; mouse_data+0 - state byte
        mov cx, dx
        and dx, LEFT_BUTTON_PRESSED            ; get left button flag 
        mov bx, word [button_status]           
        and bx, LEFT_BUTTON_PRESSED            
        cmp bx, dx                             ; check if LB already pressed
        je .check_right_button                 ; if yes check RB
        jg .left_released                      ; if true means LB released
            inc word [button_press_number+LEFT]       ; inc LB click counter 
            push word [x_pos]
            pop word [h_pos_last_press+LEFT]          ; store x pos where clicked LB
            push word [y_pos]
            pop word [v_pos_last_press+LEFT]          ; store y pos where clicked LB
            or word [user_sub_routine_event], 0002h   ; set flag that LB clicked
            jmp near .check_right_button              ; check RB state
        .left_released:
            inc word [button_release_number+LEFT]     ; inc LB release counter
            push word [x_pos]
            pop word [h_pos_last_release+LEFT]        ; store x pos where released
            push word [y_pos]
            pop word [v_pos_last_release+LEFT]          ; store y pos where released
            or word [user_sub_routine_event],  0004h    ; set flag that LB released
        .check_right_button:
        and cx, 00000010b                             ; get right button flag
        mov bx, word [button_status]
        and bx, 00000010b
        cmp bx, cx                                    ; check if RB already pressed
        je .call_user_subrutine                       ; if yes exit routine
        jg .right_released                            ; if true means RB released
            inc word [button_press_number+RIGHT]      ; inc RB click counter
            push word [x_pos]
            pop word [h_pos_last_press+RIGHT]         ; store x pos where clicked RB
            push word [y_pos]
            pop word [v_pos_last_press+RIGHT]         ; store y pos where clicked RB
            or word [user_sub_routine_event], 0008h   ; set flag that RB clicked
            jmp near .call_user_subrutine             ; exit routine
        .right_released:
            inc word [button_release_number+RIGHT]    ; inc RB release counter
            push word [x_pos]
            pop word [h_pos_last_release+RIGHT]       ; store x pos where released RB
            push word [y_pos]
            pop word [v_pos_last_release+RIGHT]       ; store y pos where released RB
            or word [user_sub_routine_event], 0010h   ; set flag that RB released
        .call_user_subrutine:

        xor dx, dx
        mov dl, byte [mouse_data+0]
        and dx, 00000111b
        mov word [button_status], dx          ; store button state for future

    popa
    ret

; ----------------------------------------------------
; __mouse_translate_2_x - mouse coordinates to screen coordinates on x
; IN: Nothing
; OUT: Nothing

__mouse_translate_2_x:
    pusha
    xor ax, ax                            
    mov al, byte [mouse_data+1]               ; get delta x pos readed from mouse
    test byte [mouse_data+0], 00010000b       ; check sign
    jz .count_x                               ; if less than 0
        or ah, 0ffh                           ; then make it negative
    .count_x:
    mov word [v_counter], ax                  ; save delta x
    add ax, word [delta_x]                    ; add delta x from previous call
    mov bx, word [h_ratio]                    
    cwd
    idiv bx                                   ; div delta_x + x_counter by horozontal ratio
    mov word [delta_x], dx                    ; save remaining in delta_X (will be used in next call)
    test ax, ax                               ; if if delta_x + x_counter less than horozontal ratio (means no cursor move on y)
    jz .next3                                 ; then exit routine
        or word [user_sub_routine_event], 0001h      ; set flag that cursor moved
        push word [x_pos]                 
        pop word [h_pos_last]                        ; save old x pos for future use
        mov byte [h_moved], 1                        ; set flag that moved on x 
        cmp byte [v_moved], 0                        ; check if moved on y
        jnz .next34                                  ; if moved
        push word [y_pos]                            
        pop word [v_pos_last]                        ; save old y pos for future use
    .next34:
    add word [x_pos], ax                             ; save new x_pos
    call __mouse_set_h_limit                                 ; check x limit
    .next3:
    popa
    ret

; ----------------------------------------------------
; __mouse_set_h_limit - check if cursor inside limit rectangle
; IN: Nothing
; OUT: Nothing

__mouse_set_h_limit:
    pusha
    mov ax, word [h_limit+MAX]                   
    cmp ax, word [x_pos]
    jge .next1                                   ; if x_pos >= x_limit_max
        mov word [x_pos], ax                     ; then x_pos = x_limit_max
        jmp .next3
    .next1:
    mov ax, word [h_limit+MIN]                   
    cmp ax, word [x_pos]                         
    jle .next3                                    ; if x_pos <= x_limit_min
        mov word [x_pos], ax                      ; then x_pos = x_limit_min
    .next3:
    popa
    ret

; ----------------------------------------------------
; __mouse_translate_2_y - mouse coordinates to screen coordinates on y
; IN: Nothing
; OUT: Nothing

__mouse_translate_2_y:
    pusha
    xor ax, ax                            ; get y pos
    mov al, byte [mouse_data+2]
    test byte [mouse_data+0], 00100000b ; check sign
    jz .count_y                         ; if less than 0
        or ah, 0ffh                     ; then make it negative
    .count_y:
    mov word [v_counter], ax            ; save delta y
    sub ax, word [delta_y]                
    neg ax                                ; add delta y from previous call 
    mov bx, word [v_ratio]
    cwd
    idiv bx                             ; div delta_y + y_counter by horozontal ratio
    mov word [delta_y], dx              ;save remaining in delta_y
    test ax, ax
    jz near .next5                           ; if if delta_y + y_counter less than horozontal ratio (means no cursor move on y)
        or word [user_sub_routine_event], 0001h         ; set flag that cursor moved
        push word [y_pos]
        pop word [v_pos_last]                           ; save old y pos for future use
        mov byte [v_moved], 1                           ; set flag that moved on y
        cmp byte [h_moved], 0                           ; check if moved on x
        jnz .next33                                     ; if moved
        push word [x_pos]                               
        pop word [h_pos_last]                           ; save old y pos for future use
    .next33:
    add word [y_pos], ax                                ; save new x_pos
    call __mouse_set_v_limit                                    ; check y limit 
    .next5:
    popa
    ret

; ----------------------------------------------------
; __mouse_set_v_limit - check if cursor inside limit rectangle
; IN: Nothing
; OUT: Nothing

__mouse_set_v_limit:
    pusha
    mov ax, word [v_limit+MAX]
    cmp ax, word [y_pos]
    jge .next6                       ; if y_pos >= y_limit_max
        mov word [y_pos], ax         ; y_pos = y_limit_max
        jmp .next5
    .next6:
    mov ax, word [v_limit+MIN]
    cmp ax, word [y_pos]
    jle .next5                       ; if y_pos <= y_limit_min
        mov word [y_pos], ax         ; y_pos = y_limit_min
    .next5:
    popa
    ret

; -------------------------------------------------------------------------------
; __mouse_move_cursor - move cursor on the screen 
; IN: Nothing
; OUT: Nothing

__mouse_move_cursor:
    pusha

    call near [paint_bkg]         ; to erase old cursor position( write old background)

    call near [save_bkg]         ; it saves background under new cursor position

    call near [make_cursor]      ; makes new cursor

    call near [paint_cursor]      ; call show software cursor proc

    popa

    ret

; -------------------------------------------------------------------------
; make_cursor_text -  makes a new cursor in text mode mode 3h
; IN: Nothing
; OUT: Nothing
; Description:
;    None

make_cursor_text:
    mov ax, word [soft_character]         ; get background under the cursor

    and ax, word [cursor_mask_and]        ; and with cursor_mask_and
    
    xor ax, word [cursor_mask_xor]         ; xor with cursor_mask_xor        
    ret

make_invisible_cursor_text:
    mov ax, word [soft_character]         ; invisible cursor is background
    ret

; ----------------------------------------------------------------------
; hide_cursor_text - hides cursror on the scren in text mode mode 3h
; IN: Nothing
; OUT: Nothing

hide_cursor_text:
    pusha

    mov word [make_cursor], make_invisible_cursor_text   ; set [make_cursor] to make_invisible_cursor_text

    call near [make_cursor]                 ; make cursor invisible

    call near [paint_cursor]               ; paint invisible cursor

    xor ax, ax
    dec ax
    or byte [show_cursor_flag], al     ; set show flag
    popa
    ret

; ----------------------------------------------------------------------------
; shows_cursor_text - shows cursron on the scren in text mode mode 3h
; IN: Nothing
; OUT: Nothing

show_cursor_text:
    pusha

    push word [x_pos]          ; h_pos_last = x_pos
    pop word [h_pos_last]
    push word [y_pos]          ; v_pos_last = y_pos
    pop word [v_pos_last]

    call near [save_bkg]             ; save background under cursor

    mov word [make_cursor], make_cursor_text     ; set [make_cursor proc] to make_cursor_text

    call near [make_cursor]         ; make new visible cursor

    call near [paint_cursor]        ; paint new cursor on the screen

    xor ax, ax
    and byte [show_cursor_flag], al     ; set show flag
    popa
    ret


; ----------------------------------------------------------------------------
; paint_cursor_text - paint cursror on the scren in text mode mode 3h
; IN: Nothing
; OUT: Nothing

paint_cursor_text:
    push di
    mov dx, ax

    mov di, word [pos_lin_addr] ; move cursor position in a flat form to DI register

    push es
    push 0B800h                 ; move screen memory segment to es
    pop es   
    mov word [es:di], dx        ; put cursor on the screen
    pop es

    pop di
    ret

; -------------------------------------------------------------------------------
; paint_bkg_text - paint cursor background  on the scren in text mode mode 3h
; IN: Nothing
; OUT: Nothing

paint_bkg_text:
    push di

    mov di, word [pos_last_lin_addr]                  ; mov last cursor position in a flat form to DI register

    mov ax, word [ds:soft_character]                  ; move to ax bakcground

    push es
    push 0B800h                 ; move screen memory segment to es
    pop es
    mov word [es:di], ax        ; put background on the screen
    pop es

    pop di
    ret

; -------------------------------------------------------------------------------
; save_bkg_text - save cursor background  from the scren in text mode mode 3h
; IN: Nothing
; OUT: Nothing

save_bkg_text:
    push di

    mov di, word [pos_lin_addr]                   ; mov cursor position in a flat form to DI register

    push es
    push 0B800h                 ; move screen memory segment to es
    pop es
    mov ax, word [es:di]        ; save background from under the curson
    mov word [ds:soft_character], ax
    pop es

    pop di
    ret

; --------------------------------------------------------------------------------
; __mouse_call_user_sub_routine -  calls user subroutine if specified
; IN: Nothing
; OUT: Nothing

__mouse_call_user_sub_routine:
    pusha                                           ; save registers on the stack

    xor ax, ax
    cmp word [user_sub_routine], ax
    jnz .call_it
    cmp word [user_sub_routine+2], ax
    jz .no_call
    .call_it:

    mov ax, word [user_sub_routine_mask]            ; get user routine mask
    test ax, ax
    jz .no_call                                     ; if mask is empty exit routine
    mov bx,  word [user_sub_routine_event]
    and ax, bx                                      ; check if event from the mask occured
    test ax, ax
    jz .no_call                                     ; if no exit routine
        mov cx, word [x_pos]                        ; prepare arguments for user subroutine
        mov dx, word [y_pos]
        mov di, word [h_counter]
        mov si, word [v_counter]
        mov bx, word [button_status]
        call far [user_sub_routine]                 ; call user subroutine
        xor ax, ax
        mov word [user_sub_routine_event], ax        ; reset event flag
    .no_call:
    popa
    ret


; ------------------------------------------
; xy2flat_mode_3 - 
; IN: AX -  horisontal position
;     BX -  vertical position
; OUT: AX - linear offset
; NOTES:
;     y*64+y*16+x = 80*y + x

xy2flat_mode_3:
    shl bx, 4
    add ax, bx    ; x+y*16
    shl bx, 2
    add ax, bx    ; x+y*16+y*64
    shl ax, 1
    ret


; ---------------------------------------
; ---------------------------------------
; ----     init proc    -----------------
; ---------------------------------------
; ---------------------------------------

; ---------------------------------------
; __mouse_init_proc - init mouse
; IN: Nothing
; OUT: Nothing

__mouse_init_proc:
    push bx
    push cx
    push dx
    push si
    push di

    cli


    call __disable_kbd          ; disable keyboard jus in case

    cmp byte [installed],0      ; check if alrady installed
    jz .install_mouse           ; if no install
    cmp byte [enabled], 0       ; if installed check if enabled
    jz .install_mouse           ; if enabled just exit
        call int_33_2           ; if disabled show cursor 
        call int_33_1F          ; and enable
    .install_mouse:

    xor dx, dx
    mov word [user_sub_routine], dx      ; reset [user_sub_routine]
    mov word [user_sub_routine+2], dx


    xor ax, ax
    mov byte [counter], al              ; clear counter

    call __mouse_enable_data_raporting		; disable data raporting
    test ah, ah

    jnz .disable_ok                     ; if error exit 
        xor ax, ax
        jmp mouse_init_proc_error
    .disable_ok:

    call __mouse_disable_irq_10_74		; disable irq (irq controller)

    call __mouse_disable_irq		    ; disable irq (keyboard controler)


    call __mouse_reset                  ; reset mouse
    test ah, ah                         ; if error exit
    jnz .reset_ok
        xor ax, ax
	    jmp mouse_init_proc_error
    .reset_ok:

    test al, al		                    ; check if mouse id is 0
    jz .mouse_2bt                       ; if no exit (drived dont support non 2 button mouses)
        xor ax, ax
	jmp mouse_init_proc_error
    .mouse_2bt:

    push word 2
    pop word [packet_size]		; set pocket size to 3 bytes (standard 2 button mouse)


; --------------- place for init mouse

    xor ax, ax
    inc ax                                   ; enable data raporting
    call __mouse_enable_data_raporting
    test ah, ah

    jnz .enable_ok                           ; if error exit 
        xor ax, ax
	jmp mouse_init_proc_error
    .enable_ok:


    call __mouse_reset_software              ; reset mouse software (sets default state)


    call __mouse_enable_irq                  ; enable mouse irq (keyboard controller)


    call __mouse_set_irq_10_74               ; enable mouse irg (set handler, enable irq controller)

    xor ax, ax
    dec ax
    mov byte [installed] , al
    mov byte [enabled] , al
    mov bx, 2                                ; driver supports only 2 buttons mouses
    mouse_init_proc_error:
    mouse_init_exit:

    call __enable_kbd                        ; enable keyboard
    push ax


    xor ax, ax
    mov byte [counter], al                   ; clear counter
    
    pop ax
    
    sti

    pop di
    pop si
    pop dx
    pop cx
    pop bx
    ret

; -------------------------------------------------------------------------------
; __mouse_enable_irq - enable mouse irq in keyboard controler
; IN: Nothing
; OUT: Nothing

__mouse_enable_irq:
    push ax
    push cx

    cli

    write64 KC_CMD_READ_COMMAND_BYTE    ; get command byte
    read60
    or al, 00000010b                    ; enable irq12 (keyboard controler)
    mov cl, al
    write64 KC_CMD_WRITE_COMMAND_BYTE
    write60 cl                            ; set command byte
   
    sti

    pop cx
    pop ax
    ret

; -------------------------------------------------------------------------------
; __mouse_disable_irq - disable mouse irq in keyboard controler
; IN: Nothing
; OUT: Nothing

__mouse_disable_irq:
    push ax
    push cx

    cli

    write64 KC_CMD_READ_COMMAND_BYTE    ; get command byte
    read60
    and al, 11111101b                   ; disable irq12 (keyboard controler)
    mov cl, al
    write64 KC_CMD_WRITE_COMMAND_BYTE
    write60 cl                            ; set command byte
   
    sti

    pop cx
    pop ax
    ret

;--------------------------------------------------------------------------------
; __mouse_set_irq_10_74 - set interupt handlers used by driver
; IN: Nothing
; OUT: Nothing

__mouse_set_irq_10_74:
    pusha

    cli

    push es

    mov ax, 74h
    call _os_get_int_handler                ; get mouse interupt handler (irq 12)
    
    push es
    pop word [old_int_74+2]                ; save the handler
    mov word [old_int_74+0], bx


  ;  mov ax, 10h                             ; get 10h handler
  ;  call _os_get_int_handler

  ;  push es
  ;  pop word [old_int_10+2]                 ; save the 10h handler in the memory
  ;  mov word [old_int_10+0], bx


  ;  mov cx, 10h
  ;  mov si, int10h_handler
  ;  call _os_modify_int_handler             ; set new 10h handler

    mov cx, 74h                            
    mov si, __mouse_driver
    call _os_modify_int_handler             ; set irq 12 handler (PS2 mouse irq)

    pop es

    call __mouse_enable_irq_cascade        ; unmask slave interupt controller

    call __mouse_enable_irq_10_74          ; unmask irq 12h in second controller

    popa

    ret

; ----------------------------------------------------------------
; __mouse_enable_irq_cascade - enable second irq controler
; IN: Nothing
; OUT: Nothing

__mouse_enable_irq_cascade:
    push ax
    cli

    in al, 21h                      ; get interupt mask register (master)           
    and al, 11111011b               ; enable irq cascade IRA 8-15
    out 21h, al                     ; set interupt mask register (master)

    sti
    pop ax
    ret

; ----------------------------------------------------------------
; __mouse_disable_irq_cascade - disable second irq controler
; IN: Nothing
; OUT: Nothing

__mouse_disable_irq_cascade:
    push ax
    cli

    in al, 21h                       ; get interupt mask register (master)       
    or al,   00000100b               ; disable irq cascade IRA 8-15
    out 21h, al                      ; set interupt mask register (master)

    sti
    pop ax
    ret

; ----------------------------------------------------------------
; __mouse_enable_irq_10_74 - unmask(enable) irq12
; IN: Nothing
; OUT: Nothing

__mouse_enable_irq_10_74:
    push ax
    cli
    in al, 0A1h                    ; get interupt mask register (slave)
    and al, 11101111b              ; enable irq 12
    out 0A1h, al                   ; set interupt mask register (slave)
    sti
    pop ax
    ret

; ----------------------------------------------------------------
; __mouse_disable_irq_10_74 - mask(disable) irq12
; IN: Nothing
; OUT: Nothing

__mouse_disable_irq_10_74:
    push ax
    cli

    in al, 0A1h                   ; get interupt mask register (slave)                  
    or al, 00010000b              ; disable irq 12
    out 0A1h, al                  ; set interupt mask register (slave)
    
    sti
    pop ax
    ret




;--------------------------------------------------------------------------------
; __mouse_reset_irq_10_74 - reset interupt handlers used by driver
; IN: Nothing
; OUT: Nothing

__mouse_reset_irq_10_74:
    pusha
    cli

    call __mouse_disable_irq_10_74

    mov ax, 74h*4            ; Beginning address = base + 74 *4
    mov si, ax

    xor ax, ax               ; Interrupt table segment 0

    push es                  ; save ES on the stack

    mov es, ax               ; set ES to point interupt segment 

    cli                      ; disable interupts

    push word [old_int_74+0]
    pop word  [es:si]            ; Set the old interrupt handler offset for irq 12 (mouse)

    push word [old_int_74+2]
    pop word [es:si+2]               ; Set the old interrupt handler segment for irq 12(mouse)

;    mov ax, 10h*4                ; Beginning address = base + 10h *4
;    mov si, ax

;    push word [old_int_10+0]     ; Set the old interrupt handler offset for irq 10h
;    pop word [es:si]
  
;    push word [old_int_10+2]     ; Set the old interrupt handler segment for irq 10h
;    pop word [es:si+2]
  
    sti                       ; enable interupts
 
    pop es                    ; get ES from the stack

    popa

    sti

    ret


; -------------------------------------------------------------
; __mouse_reset_software - set default settings
; IN: Nothing
; OUT: Nothing

__mouse_reset_software:
    pusha
    mov word [h_ratio], MOUSE_DEFAULT_H_RATIO
    mov word [v_ratio], MOUSE_DEFAULT_V_RATIO

    mov word [show_cursor], show_cursor_text
    mov word [save_bkg], save_bkg_text
    mov word [paint_bkg] , paint_bkg_text
    mov word [make_cursor], make_cursor_text
    mov word [hide_cursor], hide_cursor_text
    mov word [paint_cursor], paint_cursor_text
    mov word [make_invisible_cursor], make_invisible_cursor_text
    mov word [xy2flat] , xy2flat_mode_3

; set limits 

    push es
    push 40h
    pop es

    mov ax, word [es:4Ah]        ; number of columns from bios memory
    dec ax
    mov word [h_screen], ax        ; save screen height
    shr ax, 1                    ; divide height screen by 2
    mov word [x_pos], ax        ; set init x pos in the middle of the screen
    xor ax, ax

    mov al, byte [es:84h]        ; number of rows
    mov byte [v_screen], al        ; save screen width
    shr ax, 1                    ; divide screen width by 2
    mov word [y_pos], ax        ; set init y positin middle of the screen

    push word [v_screen]
    pop word [v_limit+MAX]        ; set x max limit to screen size

    push word [h_screen]    
    pop word [h_limit+MAX]        ; set y max limit to screen size

    xor dx, dx
    mov word [h_limit+MIN], dx    ; set x and y min minit to 0
    mov word [v_limit+MIN], dx

    push word [y_pos]            
    pop word [v_pos_last]        ; set x last position to x position
    push word [x_pos]
    pop word [h_pos_last]        ; set y last position to y position


    mov ax, word [x_pos]
    mov bx, word [y_pos]
    call near [xy2flat]         ; translate xy into flat address
    mov word [pos_lin_addr] , ax                  ; save the address
    mov word [pos_last_lin_addr], ax
   
    
    mov word [cursor_mask_and], 0012h    ; default cursor and mask
    mov word [cursor_mask_xor], 3333h    ; default cursor xor mask
    
    xor ax, ax
    mov word [cursor_type], ax           ; default cursor type (software)
    dec ax
    mov byte [show_cursor_flag], al      ; cursor hidden by default

    pop es

    popa
    ret

; -------------------------------------------
; __mouse_set_sample_rate
; IN: al - new sample rate
; OUT: ah - 1 success, 0 error
;       al - last byte sent by mouse

__mouse_set_sample_rate:
    push ax                               ; push AX on the stack for future use
    write64 KC_CMD_WRITE_MOUSE_DEVICE     ; tell keyboard controller that next byte should go to mouse
    write60  M_CMD_SET_SAMPLE_RATE        ; set sample rate command for the mouse
    read60                                ; read return value
    cmp al, M_RET_ACKNOWLEGE              ; if return value!=M_RET_ACKNOWLEGE exit with error
    jz .next1
        sub sp, -2
	    xor ah, ah
	    ret
    .next1:
    pop cx                                ; get AX from the stack to CX (consists sample rate we want to set)
    write64 KC_CMD_WRITE_MOUSE_DEVICE     ; tell keyboard controller that next byte should go to mouse
    write60 cl                            ; sent new sample rate to the mouse
    read60                                ; read return value                    
    cmp al, M_RET_ACKNOWLEGE              ; if return value!=M_RET_ACKNOWLEGE exit with error         
    jz .next2
        xor ah, ah
	ret
    .next2:
    xor ah, ah                            ; success return ah = 1, al = M_RET_ACKNOWLEGE
    inc ah
    ret


; -------------------------------------------
; __mouse_set_scalling
; IN: al - 0 salling 1:1 
;     al - 1 scalling 2:1
; OUT: ah - 1 success, 0 error
;       al - last byte sent by mouse

__mouse_set_scalling:
    push cx                            ; push cx on the stack
    test ax, ax                        ; if AX != 0 set 2:1 scalling
    jz .scalling11
        mov cx,  M_CMD_SET_SCALLING_21
        jmp .next
    .scalling11:                       ; if AX == 0 set 1:1 scalling
        mov cx,  M_CMD_SET_SCALLING_11
    .next:
    write64 KC_CMD_WRITE_MOUSE_DEVICE    ; tell keyboard controller that next byte should go to mouse
    write60  cl                          ; write new scalling to the mouse
    read60                               ; read return value
    cmp al, M_RET_ACKNOWLEGE             ; if return value!=M_RET_ACKNOWLEGE exit with error
    jz .success
        pop cx
        xor ah, ah
        ret
    .success:
    pop cx
    xor ah, ah
    inc ah                               ; success return ah = 1, al = M_RET_ACKNOWLEGE
    ret

; -------------------------------------------
; __mouse_get_status
; IN: Nothing
; OUT: ah - 1 success, 0 error
;      al - last byte sent by mouse
;      bl - byte1
;      cl - byte2
;      dl - byte3

__mouse_get_status:
    xor ax, ax
    call __mouse_enable_data_raporting           ; disable data raporting
    test ax, ax
    jnz .get_status
        xor ah, ah
        ret
    .get_status:
    write64 KC_CMD_WRITE_MOUSE_DEVICE    ; tell keyboard controller that next byte should go to mouse
    write60  M_CMD_STATUS_REQUEST        ; sent commend to get mouse status
    read60                               ; read return value
    cmp al, M_RET_ACKNOWLEGE             ; if return value!=M_RET_ACKNOWLEGE exit with error
    jz .success
        xor ah, ah
        ret
    .success:
    read60                               ; read  button state
    mov bl, al
    read60                               ; read resolution
    mov cl, al
    read60                               ; read sample rate
    mov dl, al
    xor ax, ax
    inc ax
    call __mouse_enable_data_raporting           ; enable data raporting
    test ax, ax
    inc ax
    jnz .exit
        xor ah, ah
        ret
    .exit:
    RETURN_1


; -------------------------------------------
; __mouse_enable_data_raporting - enable data raporting (start sending mouse packets)
; IN: al - 0/1 = disable/enable
; OUT: ah - 1 success, 0 error
;      al - last byte sent by mouse

__mouse_enable_data_raporting:
    push cx
    test ax, ax                                ; if AX == 0 disable data raporting
    jz .disable                                
        mov cl, M_CMD_ENABLE_DATA_REPORTING    ; if AX != 0 enable data raporing
        jmp .cont
    .disable:
        mov cl, M_CMD_DISABLE_DATA_REPORTING
    .cont:
    write64 KC_CMD_WRITE_MOUSE_DEVICE          ; tell keyboard controller that next byte should go to mouse
    write60  cl                                ; send disable/enable data raporting
    read60                                     ; read return value
    cmp al, M_RET_ACKNOWLEGE                   ; if return value!=M_RET_ACKNOWLEGE exit with error 
    jz .success
        pop cx
	xor ah, ah
	ret
    .success:
    pop cx
    xor ah, ah                                 ; success return AH=1 and AL = M_RET_ACKNOWLEGE
    inc ah
    ret

; ----------------------------------------------
; __mouse_set_resolution
; IN: al - new resolution
; OUT: ah - 1 success, 0 error
;      al - last byte sent by mouse
; NOTE: valid values for al: M_RES_1,  M_RES_2, M_RES_4, M_RES_8

__mouse_set_resolution:
    push cx
    mov cx, ax                                 ; save AX in CX for future use
    write64 KC_CMD_WRITE_MOUSE_DEVICE          ; tell keyboard controller that next byte should go to mouse
    write60  M_CMD_SET_RESOLUTION              ; send command to mouse 'set resolution'
    read60                                     ; read return value
    cmp al, M_RET_ACKNOWLEGE                   ; if return value!=M_RET_ACKNOWLEGE exit with error 
    jz .ack1
        pop cx
        xor ah, ah
        ret
    .ack1:
    write64 KC_CMD_WRITE_MOUSE_DEVICE          ; tell keyboard controller that next byte should go to mouse
    write60 cl                                 ; send new resolution to mouse
    read60                                     ; read return value
    cmp al, M_RET_ACKNOWLEGE                   ; if return value!=M_RET_ACKNOWLEGE exit with error 
    jz .ack2
        pop cx
        xor ah, ah
        ret
    .ack2:
    pop cx
    xor ah, ah
    inc ah                                     ; success return AH = 1 and AL = M_RET_ACKNOWLEGE
    ret

; ----------------------------------------------------
; __mouse_reset - reset mouse and set default settings
; IN: Nothing
; OUT: ah = 1 success
;      al = mouse id
;
;      al = 0 error
;      al = last byte sent from mouse

__mouse_reset:
    write64 KC_CMD_WRITE_MOUSE_DEVICE          ; tell keyboard controller that next byte should go to mouse
    write60  M_CMD_RESET                       ; send 'reset' command to mouse
    read60                                     ; read return value
    cmp al, M_RET_ACKNOWLEGE                   ; if return value!=M_RET_ACKNOWLEGE exit with error 
    jz .ack1
        xor ah, ah
        ret
    .ack1:
    read60                                     ; read mouse reset return value
    cmp al, M_RET_RESET_OK                     ; if reset value != M_RET_RESET_OK then exit with error
    jz .ack2
        xor ah, ah
    ret
    .ack2:
    read60                                     ; read mouse id
    xor ah, ah                                 ; success return AH = 1 and AL = mouse id
    inc ah
    ret

; ----------------------------------------------------
; __mouse_get_id
; IN: Nothing
; OUT: ah = 1 success
;      al = mouse id
;
;      al = 0 error
;      al = last byte sent from mouse

__mouse_get_id:
    write64 KC_CMD_WRITE_MOUSE_DEVICE          ; tell keyboard controller that next byte should go to mouse
    write60  M_CMD_GET_DEVICE_ID               ; send 'get id' command to the mouse
    read60                                     ; get return value
    cmp al, M_RET_ACKNOWLEGE                   ; if reset value != M_RET_RESET_OK then exit with error 
    jz .ack1
        xor ah, ah
        ret
    .ack1:
    xor dx, dx
    read60                                     ; get mouse id
    xor ah, ah                                 ; return AH = 1 and AL = mouse id
    inc ah
    ret

; ----------------------------------------------------
; __set_stream_mode
; IN: Nothing
; OUT: ah = 1 success
;      al = mouse id
;
;      al = 0 error
;      al = last byte sent from mouse

__mouse_set_stream_mode:
    write64 KC_CMD_WRITE_MOUSE_DEVICE           ; tell keyboard controller that next byte should go to mouse
    write60  M_CMD_SET_STREAM_MODE              ; send 'set mouse stream mode' to the mouse
    read60                                      ; get return value
    cmp al, M_RET_ACKNOWLEGE                    ; if reset value != M_RET_RESET_OK then exit with error 
    jz .ack1
        xor ah, ah
        ret
    .ack1:
    xor ah, ah                                  ; return AH = 1 and AL = M_RET_ACKNOWLAGE
    inc ah
    ret


; ----------------------------------------------------------------
; __disable_kbd - disables keyboard
; IN: Nothing
; OUT: Nothing

__disable_kbd:
    push ax
    write64 KC_CMD_DISABLE_KEYBOARD_INTERFACE          ; send 'disable keyboard' to the keyboard controler
    pop ax
    ret

; ----------------------------------------------------------------
; __enable_kbd - enables keyboard
; IN: Nothing
; OUT: Nothing

__enable_kbd:
    push ax
    write64 KC_CMD_ENABLE_KEYBOARD_INTERFACE            ; send 'enable keyboard' to the keyboard controler
    pop ax
    ret

; ----------------------------------------------------------------
; disable_mouse - disables mouse
; IN: Nothing
; OUT: Nothing

__mouse_disable:
    push ax
    write64 KC_CMD_DISABLE_MOUSE_INTERFACE              ; send 'disable aux dev (mouse)' to the keyboard controler
    pop ax
    ret

; ----------------------------------------------------------------
; enable_mouse - enable mouse
; IN: Nothing
; OUT: Nothing

__mouse_enable:
    push ax
    write64 KC_CMD_ENABLE_MOUSE_INTERFACE                ; send 'enable aux dev (mouse)' to the keyboard controler
    pop ax
    ret


; ******************************************************************


; ------------------------------------------------------------
; int_33_1 - show cursor
; IN: Nothing
; OUT: Nothing

int_33_1:
    pusha
    cli                                            ; mask irq
    cmp byte [enabled], 0
    jz .exit
    mov al, byte [show_cursor_flag]                
    test al, al  
    jz .exit                                       ; check if cursor already shown
        call near [show_cursor]                         ; show cursor on the screen
    .exit:
    sti
    popa
    ret

; -------------------------------------------------
; int_33_2 - Hide Mouse Cursor
; IN: Nothing
; OUT: Nothing

int_33_2:
    pusha
    cli
    cmp byte [enabled], 0
    jz .exit
    mov al, byte [show_cursor_flag]
    test al, al                                  ; if mouse hidden exit
    jnz .exit
       call near [hide_cursor]                   ; hide cursor
    .exit:
    sti
    popa
    ret

; -------------------------------------------------
; int_33_3 - Get Mouse Position and Button Status
; IN: Nothing
; OUT: CX - row
;      DX - columnt
;      BX - button_status (#1)
; NOTE:
;   (#1) Button states:
;     bit 0 - left button pressed if 1
;     bit 1 - right button pressed if 1

int_33_3:
    cli
    cmp byte [enabled], 0
    jz .exit
        mov cx, word [x_pos]                
        mov dx, word [y_pos]
        mov bx, word [button_status]
    .exit:
    sti
    ret

; -------------------------------------------------
; int_33_4 - Set Mouse Position
; IN: CX - row
;     DC - column
; OUT: Nothing

int_33_4:
    cli
    cmp byte [enabled], 0
    jz .exit
        push word [y_pos]
        pop word [v_pos_last]       ; copy x position to x last position
        push word [x_pos]
        pop word [h_pos_last]       ; copy y position to y last position
        mov word [x_pos], cx        ; set new x position 
        mov word [y_pos], dx        ; set new y position

        push word [pos_lin_addr]
        pop word [pos_last_lin_addr]
    
        mov ax, word [x_pos]
        mov bx, word [y_pos]
        call near [xy2flat]         ; translate xy into flat address
        mov word [pos_lin_addr] , ax                  ; save the address

        call __mouse_move_cursor       ; move cursor

        mov word [user_sub_routine_event], 0001h      ; set flag that cursor moved
        call __mouse_call_user_sub_routine

    .exit:

    sti
    ret

; -------------------------------------------------
; int_33_5 - Get Mouse Button Press Information
; IN:  BX = button number (see #1)
; OUT: AX = button states (see #2)
;      BX = number of times specified button has been pressed since last call
;      CX = column at time specified button was last pressed
;      DX = row at time specified button was last pressed
; NOTE:
;   (#1) Button states:
;     bit 0 - left button pressed if 1
;     bit 1 - right button pressed if 1
;    
;   (#2) Values for mouse button number (BX):
;     0001h  left
;     0010h  right

int_33_5:
    cli
    cmp byte [enabled], 0
    jz .exit
        push si
        and bx, 1
        shl bx, 1                  ; get index 0 - left , 2 - right, 4 - middle(not supported)
        mov si, bx
        mov bx, word [button_press_number+si]  ; get number of presses from a last call
        xor ax, ax
        mov word [button_press_number+si], ax  ; reset number of presses
        mov cx, word [h_pos_last_press+si]     ; return last x position 
        mov dx, word [v_pos_last_press+si]     ; return last y position
        mov ax, word [button_status]           ; return button status
        pop si
    .exit:
    sti
    ret

; -------------------------------------------------
; int_33_6 - Get Mouse Button Release Information
; IN:  BX = button number (see #1)
; OUT: AX = button states (see #2)
;      BX = number of times specified button has been pressed since last call
;      CX = column at time specified button was last pressed
;      DX = row at time specified button was last pressed
; NOTE:
;   (#1) Button states:
;     bit 0 - left button pressed if 1
;     bit 1 - right button pressed if 1
;    
;   (#2) Values for mouse button number:
;     0000h  left
;     0001h  right
;     0002h  middle (Mouse Systems/Logitech/Genius mouse)

int_33_6:
    cli
    cmp byte [enabled], 0
    jz .exit
        push si
        and bx, 1
        shl bx, 1                              ; get index 0 - left , 2 - right, 4 - middle(not supported)
        mov si, bx
        mov bx, word [button_release_number+si]   ; get number of releases from a last call
        xor ax, ax
        mov word [button_release_number+si], ax   ; reset number of presses
        mov cx, word [h_pos_last_release+si]      ; return last x position
        mov dx, word [v_pos_last_release+si]      ; return last y position
        mov ax, word [button_status]              ; return button status
        pop si
    .exit:
    sti
    ret


; -------------------------------------------------
; int_33_7 - Set Mouse Horizontal Min/Max Position
; IN: CX - minimum column
;     DX - maximum column
; OUT: Nothing
; Note: 
;     When minimum is greater than maximum it swaps them

int_33_7:
    cli
    cmp byte [enabled], 0
    jz .exit
        cmp cx, dx                    
        jb .next                      ; if minimum is greater than maximum
            xchg cx, dx               ; swap them
        .next:
        mov word [h_limit+MIN], cx    ; set new minimum
        mov word [h_limit+MAX], dx    ; set new maximum
    .exit:
    sti
    ret

; -------------------------------------------------
; int_33_8 - Set Mouse Vertical Min/Max Position
; IN: CX - minimum row
;     DX - maximum row
; OUT: Nothing
; Note: 
;     When minimum is greater than maximum it swaps them

int_33_8:
    cli
    cmp byte [enabled], 0
    jz .exit
        cmp cx, dx
        jb .next                      ; if minimum is greater than maximum
            xchg cx, dx               ; swap them
        .next:
        mov word [v_limit+MIN], cx    ; set new minimum
        mov word [v_limit+MAX], dx    ; set new maximum
    .exit:
    sti
    ret


; -------------------------------------------------
; int_33_A - Set Mouse Text Cursor
; IN: CX - screen mask
;     DX - cursor mask;
; OUT: Nothing
; NOTE: Character/attribute data at the current screen position is ANDed with the screen mask 
;       and then XORed with the  cursor mask 

int_33_A:
    pusha
    cli
    cmp byte [enabled], 0
    jz .exit
        mov word [cursor_mask_and], cx       ; set new cursor masks
        mov word [cursor_mask_xor], dx       ; 
    .exit:
    sti
    popa
    ret

; -------------------------------------------------
; FIXME ------------ INT 33,B - Read Mouse Motion Counters
; -------------------------------------------------

int_33_B:
    cli
    cmp byte [enabled], 0
    jz .exit
        mov cx, word [h_counter]
        mov dx, word [v_counter]
    .exit:
    sti
    ret

; ------------------------------------------------------------
; int_33_C - Set Mouse User Defined Subroutine and Input Mask
; IN: CX - call mask (see table #4)
;     ES:DX - FAR routine (see table#5)
; OUT: Nothing
; NOTE:
;  Bitfields for mouse call mask: 
;    Bit(s) Description 
;    0 call if mouse moves 
;    1 call if left button pressed 
;    2 call if left button released 
;    3 call if right button pressed 
;    4 call if right button released 
;    5 call if middle button pressed (Mouse Systems/Logitech/Genius mouse) 
;    6 call if middle button released (Mouse Systems/Logitech/Genius mouse) 
;    7-15 unused
;
;  Values interrupt routine is called with:. 
;    AX = condition mask (same bit assignments as call mask). 
;    BX = button state. 
;    CX = cursor column. 
;    DX = cursor row. 
;    SI = horizontal mickey count. 
;    DI = vertical mickey count 

int_33_C:
    cli
    cmp byte [enabled], 0
    jz .exit
        mov word [user_sub_routine_mask], cx    ; save user mask
        mov word [user_sub_routine], dx         ; set user subroutine offset
        push es
        pop word [user_sub_routine+2]           ; set user subroutine segment
    .exit:
    sti
    ret

; ------------------------------------------------------------
; INT 33,F - Set Mouse Mickey Pixel Ratio
; ------------------------------------------------------------

int_33_F:
    cli
    cmp byte [enabled], 0
    jz .exit
        mov word [h_ratio], cx            ; set new horizontal ratio
        mov word [v_ratio], dx            ; set new vertical ratio
    .exit:
    sti
    ret

; -------------------------------------------------
; INT 33,13 - Set Mouse Double Speed Threshold
; -------------------------------------------------

res200 db '200',0
res100 db '100',0
res80  db '80',0
resu  db 'unknow',0

int_33_13:
    cli


    sti
    ret

; ------------------------------------------------------------
; INT 33,14 - Swap Interrupt Subroutines
; ------------------------------------------------------------

int_33_14:
    cmp byte [enabled], 0
    jz .exit
        push word [user_sub_routine_mask]         ; save on stack old values 
        push word [user_sub_routine]
        push word [user_sub_routine+2]
        call int_33_C                             ; set new interrupt subroutine
        pop es                                    ; return old vlues from the stack
        pop dx
        pop cx
    .exit:
    ret
; -------------------------------------------------------------------------------
; int_33_1F - Disable mouse driver
; IN: Nothing
; OUT: AX - status 001Fh successful
;      AX - status FFFFh unsuccessful
; NOTES: 
;      Restores vectors for INT 10 and INT 74 (286/386). 

int_33_1F:
    push ax

    cli

    cmp byte [enabled], 0
    jz .exit

        call __mouse_disable_irq                 ; disable irq (keyboard controller)

        call __mouse_reset_irq_10_74             ; set old int10h int12h handlers

        call int_33_2                            ; hide cursor

        xor ax, ax                                ; disable data raporting
        call __mouse_enable_data_raporting


        xor ax, ax
        mov byte [counter], al                 ; reset counter
    
        mov ax, 20h         ; success return code is 19h (see next instruction dec ax)

        dec ax              ; if not instaled error

	xor ax, ax
	mov byte [enabled], al

    .exit:

    sti

    pop ax

    ret

; -------------------------------------------------------------------------------
; int_33_20 - Enable mouse driver
; IN: Nothing
; OUT: AX - 0020h successful
;      AX - FFFFh unsuccesful
; NOTES: 
;      Restores vectors for INT 10h and INT 74h (286/386) 
;      which were removed by function 1Fh.

int_33_20:

    xor ax, ax

    cmp byte [enabled], al
    jnz .exit

        xor ax, ax                                ; disable data raporting
	call __mouse_enable_data_raporting

	xor ax, ax
	mov byte [counter], al                    ; reset mouse packet counter

	xor ax, ax
	cmp byte [installed], al                  ; if not installed exit
	jz .exit
	cmp byte [enabled], al                    ; if no enabled exit
	jnz .exit

	inc ax
	mov byte [enabled], al                   ; set enable flag

	cli

	call __mouse_reset_software              ; set mouse to default state

	call __mouse_reset                       ; reset mouse
	call __mouse_get_id                      ; get id

	xor ax, ax                                ; enable data raporting
	inc ax
	call __mouse_enable_data_raporting

	call __mouse_set_irq_10_74                ; set interupts
	call __mouse_enable_irq                   ; enable mouse interuprt in keyboard controller

	mov ax, 21h         ; success return code is 20h (see next instruction dec ax)
    .exit:
    dec ax

    ret
; ----------------------------------------------------------------
; int_33_21 - reset mouse software
; IN: Nothing
; OUT: Nothing

int_33_21:
    push cx
    mov cl, byte [installed]
    test cl, cl
    jz .not_installed                  ; if not installed exit

    cli

    call near [hide_cursor]
    call __mouse_reset_software

    mov word [user_sub_routine_event], 0001h      ; set flag that cursor moved
    call __mouse_call_user_sub_routine

    pop cx

    xor ax, ax
    mov bx, ax
    sub bx, -2

    dec ax

    sti
    ret

    .not_installed:            ; return 0021h
    pop cx
    ret




; ***********************************************************************
; ***********************************************************************
; ***********************************************************************

; --------------------------------------------------------------
; os_mouse_int33 - int 33 handler
; IN: Nothing
; OUT: Nothing

os_mouse_int33:            ; Mouse interrupt handler
    cmp ax, 0
    je near dos_mouse_init_driver
    cmp ax, 1
    je near dos_mouse_show_cursor
    cmp ax, 2
    je near dos_mouse_hide_cursor
    cmp ax, 3
    je near dos_mouse_get_pos_bt_info
    cmp ax, 4
    je near dos_mouse_set_pos
    cmp ax, 5
    je near dos_mouse_press_info
    cmp ax, 6
    je near dos_mouse_release_info
    cmp ax, 7
    je near dos_mouse_set_h_limit
    cmp ax, 8
    je near dos_mouse_set_v_limit
    cmp ax, 0Ah
    je near dos_mouse_set_text_cursor
    cmp ax, 0Bh
    je near dos_mouse_read_mouse_motion_counters
    cmp ax, 0Ch
    je near dos_mouse_set_user_defined_subroutine
    cmp ax, 0Fh
    je near dos_mouse_set_pixel_ratio
    cmp ax, 13h
    je near dos_mouse_double_speed_threshold
    cmp ax, 14h
    je near dos_mouse_swap_interrupt_subroutines
    cmp ax, 1Fh
    je near dos_mouse_disable_driver
    cmp ax, 20h
    je near dos_mouse_enable_driver
    cmp ax, 21h
    je near dos_mouse_reset_mouse_software

    mov si, .msg
    call _os_print_string
    call _os_kbd_wait_for_key
    jmp _os_int_reboot

    .msg db 10, 13, 10, 13, '>>> Unimplemented DOS mouse call - rebooting', 10, 13, 0

; -----------------------------------------------------------------
; DOS mouse initialisation (int 33h, AH = 0h)

dos_mouse_init_driver:
    call __mouse_init_proc
    iret

dos_mouse_show_cursor:
    call int_33_1
    iret

dos_mouse_hide_cursor:
    call int_33_2
    iret

dos_mouse_get_pos_bt_info:
    call int_33_3
    iret

dos_mouse_set_pos:
    call int_33_4
    iret

dos_mouse_press_info:
    call int_33_5
    iret

dos_mouse_release_info:
    call int_33_6
    iret

dos_mouse_set_h_limit:
    call int_33_7
    iret

dos_mouse_set_v_limit:
    call int_33_8
    iret

dos_mouse_set_text_cursor:
    call int_33_A
    iret

dos_mouse_read_mouse_motion_counters:
    call int_33_B
    iret

dos_mouse_set_user_defined_subroutine:
    call int_33_C
    iret

dos_mouse_set_pixel_ratio:
    call int_33_F
    iret

dos_mouse_double_speed_threshold:
    call int_33_13
    iret

dos_mouse_swap_interrupt_subroutines:
    call int_33_14
    iret

dos_mouse_disable_driver:
    call int_33_1F
    iret

dos_mouse_enable_driver:
    call int_33_20
    iret

dos_mouse_reset_mouse_software:
    call int_33_21
    iret


; ---------------------------------------------------------------------
; int10h_handler - new hadler for int 10h
; IN:
; OUT:
; NOTE: make the driver working with bios syscalls

int10h_handler:
    cmp ah, 6                   ; scroll up window
    jz near int10h_region

    cmp ah, 7                   ; scroll down window
    jz near int10h_region

    cmp ah, 9                   ; write char. and attr. at cursor position
    jz near int10h_hide_9

    cmp ah, 0Ah                 ; write char. only at cursor position
    jz near int10h_hide_A

    cmp ah, 0Bh                 ; set background/border color
    jz near int10h_hide

    cmp ah, 0Eh			; teletype output
    jz near int10h_point

    pushf
    call far [old_int_10]	; call orginal int10h handler 
    iret


int10h_hide:                    ; hide cursor
    call int_33_2

    pushf
    call far [old_int_10]       ; call int 10h 

    call int_33_1		; show cursor

    iret                        ; exit call

int10h_hide_A:
int10h_hide_9:
    push ax
    push bx
    push dx
    push cx
    push cx

    mov ah, 03h                ; get cursor position
    xor bh, bh
    int 10h

    xor ax, ax
    mov bx, bx
    mov bl, dh
    mov al, dl

    call xy2flat_mode_3        ; start display position (flat address)
	
    mov word [.start_display], ax    ; save it

    pop cx
    add ax, cx                 ; stop display position (flat address)

    mov word [.stop_display], ax   ; save it

    mov ax, word [x_pos]
    mov bx, word [y_pos]

    call xy2flat_mode_3			; get mouse cursor position (flat address)

    cmp ax, word [.start_display]       ; check if cursor inside area to paint if yes hide cursor
    jb .next
    cmp ax, word [.stop_display]
    ja .next

    call int_33_2              ; hide cursor

    .next:
    pop cx
    pop dx
    pop bx
    pop ax

    pushf
    call far [old_int_10]       ; call int10h

    call int_33_1               ; show cursor

    iret                        ; exit call

.start_display dw 0
.stop_display dw 0

int10h_point:
    push ax
    push bx
    push dx
    push cx

    mov ah, 03h                ; get cursor position
    xor bh, bh
    int 10h

    xor ax, ax
    mov bx, bx
    mov bl, dh
    mov al, dl

    call xy2flat_mode_3        ; start display position (flat address)

    mov word [.start_display], ax    ; save it

    mov ax, word [x_pos]
    mov bx, word [y_pos]

    call xy2flat_mode_3			; get mouse cursor position (flat address)

    cmp ax, word [.start_display]       ; check if cursor inside area to paint if yes hide cursor
    jnz .next

    call int_33_2              ; hide cursor

    .next:
    pop cx
    pop dx
    pop bx
    pop ax

    pushf
    call far [old_int_10]       ; call old int10h

    call int_33_1               ; show cursor

    iret                        ; exit call

.start_display dw 0

int10h_region:
    push ax
    mov ax, word [x_pos]    ; copy mouse horizontal position into AX

    cmp al, cl              ; check if AX inside horizontal region
    jb .next                ; if outside exit
    cmp al, dl
    ja .next

    mov ax, word [y_pos]    ; copy mouse vertical position into AX
    
    cmp al, ch              ; check if AX inside vertical region
    jb .next                ; if outside exit
    cmp al, dh
    ja .next
    
    call int_33_2 ; hide cursor

    .next:

    pop ax

    pushf
    call far [old_int_10]  ; call old interupt handler

    call int_33_1

    iret

%endif

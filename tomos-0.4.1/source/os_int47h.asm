;  @description File contains subroutines responisble for systemcalls.


%ifndef TOMOS_INT47H_ASM
%define TOMOS_INT47H_ASM

%include "tomos.inc"

%DEFINE TKS TOMOS_KERNEL_SEGMENT


%DEFINE INT47_SIZE  tomos_int47h_calls_end-tomos_int47h_calls
%DEFINE INT47_ELEMENTS (INT47_SIZE >> 2)

%include "syscalls.inc"



; -------------------------------------------------
; @proc os_int47h_setup
; Set TomOS system calls on int 47h. Cant be called by user.
; It is called by system during startup. 
; 
; @see os_main.asm
; @param_in Nothing 
; @param_out Nothing
; @section "tomos syscalls"

_os_int47h_setup:
    mov cx, 47h                   ; number of int (47h in our case)
    mov si, _os_int47h            ; handler address
    call _os_modify_int_handler   ; set new 47h handler
    ret

; -------------------------------------------------
; @proc os_int47h
; int47 handler.
; 
; @param_in  AX - id of syscall
; @param_out if ax out of bound return 0FFFFh
; @section "tomos syscalls"

_os_int47h:
    push bx                     ; save BX on the stack

    mov bx, ax
    cmp ax, INT47_ELEMENTS      ; check if out of bound
    ja .exit

    shl bx, 02h                 ; get index in tomos_int47h_calls
    mov  ax, word [tomos_int47h_calls + bx]   ; get AX element form the array
 
    pop bx
    call     ax                 ; call a syscall

    iret                        ; return

    .exit:
    xor ax, ax                  ; set AX = 0FFh
    dec ax

    pop bx                      ; restore BX from the stack
    iret                        ; return

; ----------------------------------------------
; @proc os_int47h_get_syscall
;  Get addres of syscall.
; 
; @param_in AX - id of syscall
; @param_out  DX:DI - syscall address
; @param_out if ax out of bound return 0FFFFh
; @section "tomos syscalls"

_os_int47h_get_syscall:
    push bx                 ; save BX on the stack

    mov bx, ax              ; save AX (id) into BX
    cmp ax, INT47_ELEMENTS  ; check if AX in out of bound
    jae .exit

    shl bx, 02h             ; get index in tomos_int47h_calls
    lea bx, [tomos_int47h_calls + bx]    ; get AX element form the array

    mov di, word [bx + far_address.offset]   ; copy syscall offset to DI
    mov dx, word [bx + far_address.segment]  ; copy syscall segment to DX

    pop bx                  ; restore BX from the stack
    ret                     ; return

    .exit:
    xor ax, ax        
    dec ax                  ; set AX = 0FFh

    pop bx                  ; restore BX from the stack
    ret                     ; return
    
; ----------------------------------------------
; @proc os_int47h_set_syscall
;  Set addres of syscall.
;
;  @param_in  AX - id of syscall
;  @param_in DX:DI - syscall address
;  @param_out AX - 0FF if id out of bound
;  @param_out if ax out of bound return 0FFFFh
;  @section "tomos syscalls"

_os_int47h_set_syscall:
    push bx                      ; save BX on the stack

    mov bx, ax                   ; save AX (id) into BX
    cmp ax, INT47_ELEMENTS       ; check if AX in out of bound
    jae .exit

    shl bx, 02h                  ; get index in tomos_int47h_calls
    lea bx, [tomos_int47h_calls + bx]      ; get AX element form the array

    mov word [bx + far_address.offset], di   ; set new syscall offset
    mov word [bx + far_address.segment], dx  ; set new syscall segment

    pop bx                       ; restore BX from the stack
    ret                          ; return

    .exit:
    xor ax, ax
    dec ax                       ; set AX = 0FFh

    pop bx                       ; restore BX from the stack
    ret                          ; return

; ----------------------------------------------
; @proc os_int47h_get_syscall_id
; Get id of syscall by address.
;
; @param_in  DX:DI - syscall address
; @param_out  AX - id syscall if found or 0FFFFh
; @section "tomos syscalls"

_os_int47h_get_syscall_id:
    push cx                      ; save CX on the stack
    push bx                      ; save BX on the stack

    xor cx, cx                   ; reset CX
    mov bx, tomos_int47h_calls   ; move offset of tomps_int47h_calls to BX 

    .loop:
    cmp cx, INT47_ELEMENTS       ; if CX >= INT47_ELEMENTS
    jae .exit_no_found           ; exit with AX = 0FFh (not found)

        cmp word [bx + far_address.offset], di    ; compare offsets
        jnz .next                                 ; if different next
        cmp word [bx + far_address.segment], dx   ; compare segments
        jz .exit_found                            ; if equal exit with ID (found)
        .next:

        sub bx, -4               ; set BX on the next element in tomos_int47h_calls
        inc cx                   ; increment CX (indicate id)
    jmp .loop                    ; execite loop

    .exit_found:
    mov ax, cx                   ; copy id to AX from CX

    pop bx                       ; restore BX from the stack
    pop cx                       ; restore CX from the stack
    ret                          ; return

    .exit_no_found:
    xor ax, ax
    dec ax                       ; set AX to 0FFh
    
    pop bx                       ; restore BX from the stack
    pop cx                       ; restore CX from the stack
    ret                          ; return

%endif

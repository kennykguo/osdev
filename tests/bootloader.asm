; Basic bootloader that prints "Hello, OS World!"
[bits 16]       ; Tell assembler we're using 16 bit mode
[org 0x7c00]    ; Bootloader offset

; Setup segments
mov ax, 0       ; Initialize segment registers
mov ds, ax
mov es, ax
mov ss, ax
mov sp, 0x7c00  ; Setup stack

; Print message
mov si, message
call print_string

; Infinite loop
jmp $

; Print string routine
print_string:
    mov ah, 0x0e    ; BIOS teletype output
.loop:
    lodsb           ; Load next character
    test al, al     ; Check if end of string
    jz .done        ; If zero, we're done
    int 0x10        ; Print character
    jmp .loop
.done:
    ret

message: db 'Hello, OS World!', 0

; Boot sector magic
times 510-($-$$) db 0   ; Pad to 510 bytes
dw 0xaa55              ; Boot signature
;
; A simple boot sector program that demonstrates addressing.
;

[org 0x7C00]              ; BIOS loads the boot sector at this address in memory.

mov ah, 0x0e        ; int 10/ah = 0x0e -> teletype output function

; First attempt
mov al, the_secret  ; Load the value of 'the_secret' into AL
int 0x10            ; Does this print an X?

; Second attempt
mov al, [the_secret] ; Load the byte at address 'the_secret' into AL
int 0x10            ; Does this print an X?

; Third attempt
mov bx, the_secret  ; Load the offset of 'the_secret' into BX
add bx, 0x7C00      ; Adjust for the segment base address
mov al, [bx]        ; Load the byte at address (BX) into AL
int 0x10            ; Does this print an X?

; Fourth attempt
mov al, [0x7C1E]    ; Load the byte at address 0x7C1E into AL
int 0x10            ; Does this print an X?

jmp $               ; Infinite loop (halt execution)

the_secret:
    db "D"              ; Define the secret value as 'X'

; Padding and magic number
times 510 - ($ - $$) db 0  ; Pad the boot sector to 510 bytes
dw 0xAA55                 ; Magic number at the end

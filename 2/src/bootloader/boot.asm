org 0x7C00        ; Set the origin to 0x7C00 (Boot sector starts at this address)
bits  16           ; Specify 16-bit mode (for x86 architecture)

%define ENDL 0x0D, 0x0A  ; Define the end-of-line (newline) as CRLF (0x0D 0x0A)

;
; FAT12 header
;
jmp short start
nop
bdb_oem: db 'MSWIN4.1' ; 8 bytes
dbd_bytes_per_sector: dw 512
bdb_sectors_per_custer db 1
bdb_reserved_sectors: dw 1
bdb_fat_count: db 2
bdb_dir_entries_count: dw 0E0h
bdb_total_sectors: dw 2880
bdb_media_descriptor_type: db 0F0h
bdb_sectors_per_fat: dw 9
bdb_sectors_per_track: dw 18
bdb_heads: dw 2
bdb_hidden_sectors: dd 0
bdb_large_sector_count: dd 0


; Extended boot record
ebr_drive_number: db 0
                  db 0
ebr_signature: db 29h
ebr_volume_id: db 12h, 34h, 56h, 78h
ebr_volume_label: db 'NANOBYTE OS  '
ebr_system_id: db 'FAT12    '




start: 
    jmp main          ; Jump to the main program execution



; Function to print a string (puts)
puts:
    push si           ; Save SI register (source index) on stack
    push ax           ; Save AX register on stack




.loop:
    lodsb             ; Load byte at DS:SI into AL (load string character)
    or al, al         ; Check if AL (character) is zero (null terminator)
    jz .done          ; If zero, we are at the end of the string, so jump to done

    mov ah, 0x0e      ; Set AH to 0x0e for teletype output function (BIOS interrupt)
    int 0x10          ; Call BIOS interrupt 0x10 to print the character in AL

    jmp .loop         ; Jump back to the start of the loop

.done:
    pop ax            ; Restore AX register from stack
    pop si            ; Restore SI register from stack
    ret               ; Return from the function

; Main program
main:
    hlt               ; Halt the CPU (this is a basic halt, but before that, set up the environment)
    mov ax, 0         ; Set AX register to 0 (initialize)
    mov ds, ax        ; Set the Data Segment (DS) register to 0 (point to 0x0000)
    mov es, ax        ; Set the Extra Segment (ES) register to 0 (point to 0x0000)

    mov ss, ax        ; Set the Stack Segment (SS) register to 0 (point to 0x0000)
    mov sp, 0x7C00    ; Set the Stack Pointer (SP) to 0x7C00 (top of the boot sector)

    mov si, msg_hello ; Load the address of the "Hello World!" message into SI
    call puts         ; Call puts function to display the message

.halt:
    jmp .halt         ; Infinite loop to halt the execution (after message is printed)

; The message to display
msg_hello: 
    db 'Hello world!', ENDL, 0  ; The message "Hello world!" followed by a newline and null terminator

times 510 - ($-$$) db 0  ; Fill the remaining space up to 510 bytes with 0 (for boot sector padding)
dw 0AA55h               ; Boot signature (0xAA55) – required to identify the boot sector

; Boot Sector (0x7C00): The code is designed to run as a bootloader, starting at address 0x7C00 in memory, which is the standard location for boot code.
; Interrupts: The program uses BIOS interrupt 0x10 with function 0x0e to print characters on the screen.
; Assembly Functions: The puts function is used to print the "Hello world!" message by looping through each character in the string until it encounters the null terminator.
; Infinite Loop for Halting: After printing the message, the program enters an infinite loop (.halt) to stop the CPU, as bootloaders typically do not continue beyond their initial task.
; Padding: The line times 510 - ($-$$) db 0 ensures that the boot sector is 512 bytes in total (a requirement for boot sectors), padding with zeros if necessary.
; Boot Signature: The dw 0AA55h is the boot sector signature that identifies the binary as a valid bootloader to the BIOS.
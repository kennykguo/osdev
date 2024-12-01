; Bootloader for a simple operating system
; This code represents a minimal boot sector that demonstrates basic system initialization
; and interaction with BIOS interrupts

; Origin and Bit Mode Configuration
org 0x7C00        ; Set the memory origin to 0x7C00 
                  ; This is the standard memory address where BIOS loads the first sector of a bootable device
                  ; Ensures that all memory references are relative to this address

bits  16          ; Specify 16-bit real mode 
                  ; x86 systems start in 16-bit mode when booting
                  ; This mode allows direct hardware access and compatibility with older systems

; Preprocessor Macro for End of Line
%define ENDL 0x0D, 0x0A  ; Define a macro for end-of-line (newline)
                         ; 0x0D is carriage return, 0x0A is line feed
                         ; Used to create cross-platform compatible line breaks

;
; FAT12 File System Boot Block Header
; These fields are required by the FAT12 file system specification
;
jmp short start   ; Short jump to skip over the header 
                  ; Ensures that the header data isn't interpreted as executable code
nop               ; No operation - used for alignment and compatibility with some BIOSes

; OEM and Disk Parameters (Bios Parameter Block)
bdb_oem: db 'MSWIN4.1'  ; OEM identifier (8 bytes)
                        ; Typically indicates the operating system or tool that formatted the disk

; Disk geometry and file system parameters
dbd_bytes_per_sector: dw 512     ; Number of bytes in each sector (standard for most floppies/early hard drives)
bdb_sectors_per_cluster: db 1    ; Number of sectors per allocation unit
bdb_reserved_sectors: dw 1       ; Number of reserved sectors (boot sector is typically reserved)
bdb_fat_count: db 2              ; Number of File Allocation Tables (FAT) on the disk
bdb_dir_entries_count: dw 0E0h   ; Maximum number of directory entries in the root directory
bdb_total_sectors: dw 2880       ; Total number of sectors on the disk (for a standard 1.44MB floppy)
bdb_media_descriptor_type: db 0F0h  ; Media type identifier (0xF0 typically represents a removable media)
bdb_sectors_per_fat: dw 9        ; Number of sectors occupied by one FAT
bdb_sectors_per_track: dw 18     ; Number of sectors per track
bdb_heads: dw 2                  ; Number of read/write heads
bdb_hidden_sectors: dd 0          ; Number of hidden sectors before the partition
bdb_large_sector_count: dd 0      ; Total number of sectors for large volumes

; Extended Boot Record
ebr_drive_number: db 0            ; BIOS drive number (will be set by BIOS)
                db 0              ; Reserved byte
ebr_signature: db 29h             ; Extended boot signature
ebr_volume_id: db 12h, 34h, 56h, 78h  ; Volume serial number
ebr_volume_label: db 'NANOBYTE OS  '  ; Volume label (11 bytes)
ebr_system_id: db 'FAT12    '     ; File system type identifier

; Entry Point
start: 
    jmp main      ; Jump to the main program execution
                  ; Separates initialization code from the main program logic

; Function: puts
; Purpose: Print a null-terminated string to the screen
; Input: SI register points to the start of the string
; Uses: BIOS interrupt 0x10 for character output
puts:
    push si       ; Save source index register (contains string pointer)
    push ax       ; Save accumulator register to preserve its current value

.loop:
    lodsb         ; Load the byte at DS:SI into AL and increment SI
                  ; Effectively moves through the string one character at a time
    or al, al     ; Bitwise OR to set zero flag if AL is zero
                  ; Checks if we've reached the end of the string (null terminator)
    jz .done      ; If zero flag is set, we've reached the end of the string

    mov ah, 0x0e  ; BIOS teletype output function
                  ; Tells interrupt 0x10 to print the character in AL
    int 0x10      ; Call BIOS video interrupt to print the character

    jmp .loop     ; Continue processing next character

.done:
    pop bx        ; Restore registers (bx used as placeholder)
    pop ax        
    pop si        ; Restore registers to their original state
    ret           ; Return from the function

; Main Program Initialization
main:
    ; Initialize segment registers
    mov ax, 0     ; Load 0 into accumulator
    mov ds, ax    ; Set Data Segment to 0
    mov es, ax    ; Set Extra Segment to 0
                  ; These ensure we're working with a clean, predictable memory state

    mov ss, ax    ; Set Stack Segment to 0
    mov sp, 0x7C00 ; Set Stack Pointer to the top of the boot sector
                   ; This creates a stack in a safe, known location

    ; Store the drive number passed by BIOS
    mov [ebr_drive_number], dl  ; DL contains the boot drive number

    ; Attempt to read a sector from the disk
    mov ax, 1     ; LBA (Logical Block Address) of the sector to read
    mov cl, 1     ; Number of sectors to read
    mov bx, 0x7E00 ; Memory address to store the read data (just after boot sector)
    call disk_read ; Call disk read routine

    ; Display welcome message
    mov si, msg_hello ; Load address of hello message into source index
    call puts         ; Call puts to display the message

    ; Halt the system
    cli             ; Clear interrupt flag (disable interrupts)
    hlt             ; Halt the CPU

; Error Handling: Floppy Disk Read Failure
floppy_error:
    mov si, msg_read_failed  ; Load error message address
    call puts                ; Display error message
    jmp wait_key_and_reboot  ; Wait for keypress and reboot

; Wait for Keypress and Reboot
wait_key_and_reboot:
    mov ah, 0      ; BIOS function to wait for keypress
    int 16h        ; Call keyboard interrupt
    jmp 0FFFFh:0   ; Jump to reboot vector (forces system reset)

.halt:
    cli            ; Clear interrupts
    hlt            ; Halt CPU (final fallback)

; Convert Logical Block Address (LBA) to CHS (Cylinder-Head-Sector)
; This is necessary because older disk systems use CHS addressing
lba_to_chs:
    push ax
    push dx

    xor dx, dx     ; Clear dx for division
    div word [bdb_sectors_per_track]  ; Divide LBA by sectors per track

    inc dx         ; Increment to get sector number (1-based)
    mov cx, dx     ; Store sector number in cx

    xor dx, dx     ; Clear dx again for next division
    div word [bdb_heads]  ; Divide by number of heads

    mov dh, dl     ; Head number
    mov ch, al     ; Cylinder number
    shl ah, 6      ; Shift high bits of cylinder to correct position
    or cl, ah      ; Combine cylinder and sector information

    pop ax
    mov dl, al     ; Restore drive number
    pop ax
    ret

; Read sectors from disk
disk_read:
    push ax
    push bx
    push cx
    push dx
    push di

    push cx
    call lba_to_chs  ; Convert LBA to CHS addressing
    pop ax

    mov ah, 02h      ; BIOS function for disk read
    mov di, 3        ; Number of read attempt retries

.retry:
    pusha            ; Save all registers
    stc              ; Set carry flag (required for some BIOS versions)
    int 13h          ; Call BIOS disk interrupt
    jnc .done        ; If read successful (carry not set), exit

    popa             ; Restore registers
    call disk_reset  ; Reset disk controller

    dec di           ; Decrement retry counter
    test di, di      ; Check if we've exhausted retries
    jnz .retry       ; If retries remain, try again

.fail:
    jmp floppy_error ; Jump to error handling if all retries fail

.done:
    popa             ; Restore registers
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; Reset Disk Controller
disk_reset:
    pusha            ; Save all registers
    mov ah, 0        ; BIOS function to reset disk system
    stc              ; Set carry flag
    int 13h          ; Call disk interrupt
    jc floppy_error  ; If reset fails, jump to error handler
    popa             ; Restore registers
    ret

; Message Strings
msg_hello: 
    db 'Hello world!', ENDL, 0  ; Null-terminated string with newline

msg_read_failed:
    db 'Read from disk failed', ENDL, 0

; Boot Sector Padding and Signature
times 510 - ($-$$) db 0  ; Pad remaining bytes to make sector exactly 512 bytes
dw 0AA55h               ; Boot sector signature (magic number that tells BIOS this is bootable)
; Bootloader for a simple operating system
; This code represents a minimal boot sector that demonstrates basic system initialization
; and interaction with BIOS interrupts

; Origin and Bit Mode Configuration
; Set the memory origin to 0x7C00 -> this is where the bootloader is loaded into RAM
org 0x7C00        
; This is the standard memory address where BIOS loads the first sector of a bootable device
; Ensures that all memory references are relative to this address

; Specify 16-bit real mode 
; x86 systems start in 16-bit mode when booting
; This mode allows direct hardware access and compatibility with older systems
bits  16

; Preprocessor Macro for End of Line
%define ENDL 0x0D, 0x0A  ; Define a macro for end-of-line (newline)
; FAT12 File System Boot Block Header
; These fields are required by the FAT12 file system specification
jmp short start   ; Short jump to skip over the header 
                  ; Ensures that the header data isn't interpreted as executable code
nop               ; No operation - used for alignment and compatibility with some BIOSes

; OEM and Disk Parameters (Bios Parameter Block)
bdb_oem: db 'MSWIN4.1'  ; OEM identifier (8 bytes)
                        ; Typically indicates the operating system or tool that formatted the disk

; Disk geometry and file system parameters
dbd_bytes_per_sector: dw 512     ; Number of bytes in each sector (standard for most floppies/early hard drives)
bdb_sectors_per_cluster: db 1    ; Number of sectors per allocation unit
bdb_reserved_sectors: dw 1       ; Number of reserved sectors (boot sector is reserved)
bdb_fat_count: db 2              ; Number of File Allocation Tables (FAT) on the disk
bdb_dir_entries_count: dw 0E0h   ; Maximum number of directory entries in the root directory
bdb_total_sectors: dw 2880       ; Total number of sectors on the disk (for a standard 1.44MB floppy)
bdb_media_descriptor_type: db 0F0h  ; Media type identifier (0xF0 represents a removable media)
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


main:
    mov ax, 0     ; Set up segment registers
    mov ds, ax    
    mov es, ax    
    mov ss, ax    
    mov sp, 0x7C00 ; Stack starts at the end of the bootloader

    mov ax, 1     ; LBA sector to read
    mov bx, 0x7E00 ; Load sector at memory address 0x7E00
    mov cl, 1     ; Read one sector
    mov [ebr_drive_number], dl  ; Store boot drive number
    call disk_read  ; Read the sector

    mov si, msg_hello  ; Load address of "Hello World" message
    call puts         ; Print message

    cli            ; Disable interrupts
    hlt            ; Halt execution

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


; C = LBA ÷ (HPC × SPT)
; H = (LBA ÷ SPT) mod HPC
; S = (LBA mod SPT) + 1
; C, H and S are the cylinder number, the head number, and the sector number
; LBA is the logical block address
; HPC is the maximum number of heads per cylinder (reported by disk drive, typically 16 for 28-bit LBA)
; SPT is the maximum number of sectors per track (reported by disk drive, typically 63 for 28-bit LBA)
; Convert Logical Block Address (LBA) to CHS (Cylinder-Head-Sector)
; This is necessary because older disk systems use CHS addressing
lba_to_chs:
    push ax       ; Save LBA value in AX (LBA will be used for calculations)
    push dx       ; Save DX register (which will be modified during the divisions)
    xor dx, dx    ; Clear DX (it will store remainders in later division steps)
    
    ; Step 1: Calculate the sector number (1-based)
    div word [bdb_sectors_per_track]  ; AX = LBA / sectors_per_track, DX = LBA % sectors_per_track
    
    inc dx        ; Increment DX to make sector 1-based (sectors start from 1, not 0)
    mov cx, dx    ; Store the sector number in CX (CX will hold the final result for sector)
    
    xor dx, dx    ; Clear DX again for the next calculation

    ; Step 2: Calculate the head number (part of the cylinder, within the cylinder's tracks)
    div word [bdb_heads]  ; AX = Cylinder number, DX = Head (LBA / SPT) % heads
    ; Explanation:
    ; Now we divide the quotient from the previous division (AX) by `bdb_heads` (2). 
    ; This gives us the head number. AX will contain the cylinder number, and DX (now in DL) will hold the head number.
    mov dh, dl    ; Move the head number (from DL) into DH (part of the result)
    ; The head number is stored in DL, but we need to place it in DH because we are going to pack the result into CX and DH.

    ; Step 3: Calculate the cylinder number
    mov ch, al    ; Move the low byte of the cylinder number into CH (AX contains the cylinder now)
    shl ah, 6     ; Shift the high byte of the cylinder into the correct position (shift left by 6 bits)
    or cl, ah     ; Combine the high 2 bits of the cylinder (in AH) with the sector number (in CL)
    ; Cylinder is stored in AX after dividing by `bdb_heads`. 
    ; The lower 8 bits of the cylinder are placed in CH (low byte of cylinder), and the higher 2 bits of the cylinder are moved into CL, combining it with the sector number.
    ; Final Result:
    ; - CX will contain the combined cylinder and sector (in a packed form).
    ; - DH contains the head number.
    
    pop ax        ; Restore the original LBA value back into AX
    pop dx        ; Restore the original DX register
    ret           ; Return from the function

disk_read:
    push ax       ; Save LBA sector number (AX will contain the LBA of the sector to read)
    push bx       ; Save memory address for read (BX contains the destination buffer address)
    push cx       ; Save CX (sector number)
    push dx       ; Save DX (used for LBA-to-CHS calculation)
    push di       ; Save DI (for retry counter)

    push cx       ; Save sector count (for later usage)
    call lba_to_chs  ; Convert LBA to CHS (Cylinder, Head, Sector)
    ; After the function call, the CHS values are packed in CX, and the head number is in DH.
    
    pop ax        ; Restore sector count (likely expected in AX after LBA-to-CHS conversion)
    
    mov ah, 0x02  ; BIOS read function (0x02 is BIOS function to read sectors)
    mov di, 3     ; Retry counter initialized to 3 (attempt to retry 3 times in case of failure)

.retry:
    pusha         ; Save all registers before BIOS call (necessary for proper register preservation)
    stc           ; Set carry flag (required for some BIOS versions to indicate a read operation)
    int 13h       ; Call BIOS disk interrupt (interrupt 13h reads from disk)
    jnc .done     ; If no error (Carry flag not set), jump to done

    popa          ; Restore registers if failed
    call disk_reset ; Reset disk controller if read failed
    dec di        ; Decrement retry counter
    test di, di   ; Test if retries remain
    jnz .retry    ; If retries remain, retry reading from disk

.fail:
    jmp floppy_error  ; If all retries fail, jump to error handler

.done:
    popa          
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
    int 13h          ; Call BIOS disk interrupt
    jc floppy_error  ; If reset fails (Carry flag set), jump to error handler
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
%ifndef TOMOSOS_MACROS
%define TOMOSOS_MACROS 1

BIOS_SEGMENT equ 0x0040

BIOS_KEYBOARD_READ equ 0x001A
BIOS_KEYBOARD_WRITE equ 0x001C

BIOS_KEYBOARD_TAIL equ 0x001A
BIOS_KEYBOARD_HEAD equ 0x001C

BIOS_KEYBOARD_BUFOR_START equ 0x0080
BIOS_KEYBOARD_BUFOR_STOP equ 0x0082

BUFOR_SIZE equ 512

%macro RETURN_1 0
    xor ax, ax
    inc ax
    ret
%endmacro


; -----------------------------------------------------
; ------    misc macros     ---------------------------
; -----------------------------------------------------

struc far_address
    .offset resw 1
    .segment resw 1
endstruc

%macro m2m 2
    push %2
    pop %1
%endmacro

%macro m2mb 3
    mov  %1, %3
    mov %2, %1
%endmacro

%macro ENUM 2-*
    %assign i %1
    %rep %0-1
        %xdefine %2 i
        %rotate 1
	%assign i i+1
    %endrep
%endmacro

; -----------------------------------------------------
; -----     int47h          ---------------------------
; -----------------------------------------------------

%macro syscall 1
    mov ax, ID_%1
    int 47h
%endmacro

; ----------------------------------------------------
; ----       syscalls       --------------------------
; ----------------------------------------------------


%ifndef TKS
%DEFINE TKS TOMOS_KERNEL_SEGMENT
%endif

%macro DECLARE_SYSCALLS 1-*
    %assign id 0
    %assign loop %0/2
    %rep loop
        %ifdef ID%1
	    %defstr %%STR %1
	    %strlen %%LEN %%STR
	    %substr %%STR %%STR 2, %%LEN
	    %strcat %%ERROR_MSG "syscall os_", %%STR, " already declared."
	    %error %%ERROR_MSG
	%else
            %xdefine ID_%1  id
	%endif
	%rotate 2
	%assign id id+1
    %endrep

    SYSCALLS_ADDRS:
    %rep loop
        dw _%1 , %2
        %rotate 2
    %endrep
%endmacro



; -----------------------------------------------------
; -----     mm               --------------------------
; -----------------------------------------------------

%ifmacro MM_FAR_PTR
    %error "macro FAR_PTR exists already.
%else
    %macro FAR_PTR 1
        %1 dw 0,0
    %endmacro
%endif

%ifmacro SAVE_FAR_PTR
    %error "macro SAV_EFAR_PTR exists already.

%else
    %macro SAVE_FAR_PTR 1
        push es
        pop word [%1 + far_address.segment]
        mov word [%1 + far_address.offset], di
    %endmacro
%endif

%ifmacro LOAD_FAR_PTR
    %error "macro LOAD_FAR_PTR exists already.
%else
    %macro LOAD_FAR_PTR 1
        push word [%1 + far_address.segment]
        mov di, word [%1 + far_address.offset]
        pop es
    %endmacro
%endif

; -------------------------------------------------------
; -----   sring macros       ----------------------------
; ------------------------------------------------------- 

; --------------------------------------------------------
; @macro m_os_print_string_nl
; print string and then go to a new line
;
; @param_in string to print. ex m_os_print_string_nl 'Hello World'

%macro m_os_print_string_nl 1		; print string with new line
    push si
jmp %%l1
    %%string db %1,10,13,0
%%l1:
    mov si, %%string
    syscall os_print_string
    pop si
%endmacro

; --------------------------------------------------------
; @macro m_os_print_string
; print string
;
; @param_in string to print. ex m_os_print_string 'Hello World'

%macro m_os_print_string 1		;print string without new line
jmp %%l1
    %%string db %1,0
%%l1:
    mov si, %%string
    call os_print_string
%endmacro

; --------------------------------------------------------
; @macro m_create_string
; create string and return addres in si
;
; @param_in string to create. ex m_create_string 'Hello World'

%macro  m_create_string 1
jmp %%l1
    %%string db %1,0
%%l1:
    mov si, %%string
%endmacro


struc FAT12_TABLE

    .Bootjmp:               resb 3     ; 3 bytes for jump over the structure
    .OEMLabel:              resb 8     ; Disk label
    .BytesPerSector:        resw 1     ; Bytes per sector
    .SectorsPerCluster:     resb 1     ; Sectors per cluster
    .ReservedForBoot:       resw 1     ; Reserved sectors for boot record
    .NumberOfFats:          resb 1     ; Number of copies of the FAT
    .RootDirEntries:        resw 1     ; Number of entries in root dir
    .LogicalSectors:        resw 1     ; Number of logical sectors
    .MediumByte:            resb 1     ; Medium descriptor byte
    .SectorsPerFat:         resw 1     ; Sectors per FAT
    .SectorsPerTrack:       resw 1     ; Sectors per track (36/cylinder)
    .Sides:                 resw 1     ; Number of sides/heads
    .HiddenSectors:         resd 1     ; Number of hidden sectors
    .LargeSectors:          resd 1     ; Number of LBA sectors
    .DriveNo:               resw 1     ; Drive No: 0
    .Signature:             resb 1     ; Drive signature: 41 for floppy
    .VolumeID:              resd 1     ; Volume ID: any number
    .VolumeLabel:           resb 11    ; Volume Label: any 11 chars
    .FileSystem:            resb 8     ; File system type: don't change!
endstruc

struc BOOT_RECORD_STRUC

    .Bootjmp:               resb 3     ; 3 bytes for jump over the structure
    .OEMLabel:              resb 8     ; Disk label
    .BytesPerSector:        resw 1     ; Bytes per sector
    .SectorsPerCluster:     resb 1     ; Sectors per cluster
    .ReservedForBoot:       resw 1     ; Reserved sectors for boot record
    .NumberOfFats:          resb 1     ; Number of copies of the FAT
    .RootDirEntries:        resw 1     ; Number of entries in root dir
    .LogicalSectors:        resw 1     ; Number of logical sectors
    .MediumByte:            resb 1     ; Medium descriptor byte
    .SectorsPerFat:         resw 1     ; Sectors per FAT
    .SectorsPerTrack:       resw 1     ; Sectors per track (36/cylinder)
    .Sides:                 resw 1     ; Number of sides/heads
    .HiddenSectors:         resd 1     ; Number of hidden sectors
    .LargeSectors:          resd 1     ; Number of LBA sectors
    .DriveNo:               resw 1     ; Drive No: 0
    .Signature:             resb 1     ; Drive signature: 41 for floppy
    .VolumeID:              resd 1     ; Volume ID: any number
    .VolumeLabel:           resb 11    ; Volume Label: any 11 chars
    .FileSystem:            resb 8     ; File system type: don't change!
endstruc

%define SIZE_OF_DISK_DESCRIPTION_TABLE 62
%define SIZE_OF_BOOT_RECORD_STRUC 62

struc FAT12_83FILEFORMAT
    .FileName              resb 11        ; File name
    .Attributes            resb 1         ; Attributes of dile
    .Reserved              resb 1         ; Reserved
    .CreationTime10sec     resb 1         ; Creation time in tents of a second
    .CreationTime          resw 1         ; Creation time
    .CreationDate          resw 1         ; Creation date
    .LastAccessDate        resw 1         ; Date of the last access
    .ReservedFAT32         resw 1         ; Reserved for FAT32
    .LastModificationTime  resw 1         ; Time of last modification
    .LastModificationDate  resw 1         ; Date of last modification
    .FirstClusterLow       resw 1         ; Entrys first cluster (lower 16 bits)
    .Size                  resd 1         ; Size of the file in bytes
endstruc

%define SIZE_OF_FAT12_83FILEFORMAT 32

struc FAT12_ENTITY
    .FileName              resb 11        ; File name
    .Attributes            resb 1         ; Attributes of dile
    .Reserved              resb 1         ; Reserved
    .CreationTime10sec     resb 1         ; Creation time in tents of a second
    .CreationTime          resw 1         ; Creation time
    .CreationDate          resw 1         ; Creation date
    .LastAccessDate        resw 1         ; Date of the last access
    .Reserved1             resw 1         ; Reserved
    .LastModificationTime  resw 1         ; Time of last modification
    .LastModificationDate  resw 1         ; Date of last modification
    .Reserved2             resw 1         ; Reserved
    .Size                  resd 1         ; Size of the file in bytes
endstruc

%define SIZE_OF_FAT12_ENTITY 32

struc ADIR
    .index resw 1
    .e_fat resb SIZE_OF_FAT12_83FILEFORMAT
endstruc

%define SIZE_OF_ADIR 2 + SIZE_OF_FAT12_83FILEFORMAT

struc HDIR_STRUC
    .index resw 1
    .e_fat resb SIZE_OF_FAT12_ENTITY 
    .index_start resw 1
    .index_end resw 1
    .num_ent resw 1
    .buffer resw 2
    .buffer_size resw 1
endstruc

%define SIZE_OF_HDIR_STRUC (SIZE_OF_FAT12_ENTITY + 14)


%ifndef MAX_OPENED_FILES
    %define MAX_OPENED_FILES 6
%endif

struc FILE_DESC
    .e_fat12 resw 1
    .file_pos resw 1
    .permissions resw 1
endstruc

%define SIZE_FILE_DESC 6

%define FREE_DESC 0FFFFh

ENUM 1, FP_READ, FP_WRITE, FP_CREATE


%define IS_DIRECTORY(fat12) cmp word [fat12 + FAT12_83FILEFORMAT.Attributes], 10h

%endif


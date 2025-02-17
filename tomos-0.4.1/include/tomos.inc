; ---------------------------------------------------------------
; tomos.inc defines usefull macros which might be
; uses by applications. 
; %include "../include/tomos.inc" 
; assuming the application source code is in the programs
; directory.
; ---------------------------------------------------------------

%ifndef TOMOSOS_MACROS     ; allows to include the file more than once
%define TOMOSOS_MACROS 1

%define OSINT 47h

; --------------------------------------------------------------------
; Bitfields for a mouse call mask uses by os_mouse_set_user_subroutine
; see documantation doc/index.html

%define MOUSE_EVENT_MOVED          00001b
%define MOUSE_EVENT_LEFT_PRESSED   00010b
%define MOUSE_EVENT_LEFT_RELEASED  00100b
%define MOUSE_EVENT_RIGHT_PRESSED  01000b
%define MOUSE_EVENT_RIGHT_RELEASED 10000b

; --------------------------------------------------------------------
; uses by os_mouse_get_position
; see documantation doc/index.html

%define MOUSE_FLAG_LEFT_PRESSED    0b
%define MOUSE_FLAG_RIGHT_PRESSED   1b

%define MOUSE_LEFT_BUTTON          0
%define MOUSE_RIGHT_BUTTON         1

%define MOUSE_STATE_LEFT_BUTTON          0001b
%define MOUSE_STATE_RIGHT_BUTTON         0010b
; ----------------------------------------------------
; @struc far_address
; structute describes far pointer for example in 
; the interupt table

struc far_address
    .offset resw 1        ;<<!* offets
    .segment resw 1       ;<<!* segment
endstruc

; ----------------------------------------------------
; @macro m2m
; copy a word from memory to memory using push pop
; 
; @param_in destination
; @oaram_in source

%ifmacro m2m
    %error "macro m2m exists already."
%else
%macro m2m 2
    push %2
    pop %1
%endmacro
%endif

; -----------------------------------------------------
; @macro ENUM
; simple version of ENUM known from C language
;
; @param_in 1st parameter is a value assigned to the first element.
; @param_in list of elements, value if first declared as a first parametr of the macro.

%ifmacro ENUM
    %error "macro ENUM exists already."
%else
%macro ENUM 2-*
    %assign i %1
    %rep %0-1
        %xdefine %2 i
        %rotate 1
	%assign i i+1
    %endrep
%endmacro
%endif

; -----------------------------------------------------
; @macro FAR_PTR
; declare a far pointer
;
; @param_in name of declared variables

%ifmacro FAR_PTR
    %error "macro FAR_PTR exists already.
%else
    %macro FAR_PTR 1
        %1 dw 0,0
    %endmacro
%endif

; -----------------------------------------------------
; @macro SAVE_FAR_PTR
; copy value of ES and DI to varialbel declared by FAR_PTR.
;
; @param_in name of variable declared by FAR_PTR.

%ifmacro SAVE_FAR_PTR
    %error "macro SAV_FAR_PTR exists already.
%else
    %macro SAVE_FAR_PTR 1
        push es
        pop word [%1 + far_address.segment]
        mov word [%1 + far_address.offset], di
    %endmacro
%endif

; -----------------------------------------------------
; @macro LOAD_FAR_PTR
; copy value of declared FAR_PTR to ES and DI.
;
; @param_in name of variable declared by FAR_PTR

%ifmacro LOAD_FAR_PTR
    %error "macro LOAD_FAR_PTR exists already.
%else
    %macro LOAD_FAR_PTR 1
        push word [%1 + far_address.segment]
        mov di, word [%1 + far_address.offset]
        pop es
    %endmacro
%endif

; --------------------------------------------------------
; @macro m_os_print_string_nl
; print string and then go to a new line
;
; @param_in string to print. 

%ifmacro m_os_print_string_nl
    %error macro m_os_print_string_nl exists already.
%else
%macro m_os_print_string_nl 1
    push si
jmp %%l1
    %%string db %1,10,13,0
%%l1:
    mov si, %%string
    syscall os_print_string
    pop si
%endmacro
%endif

; --------------------------------------------------------
; @macro m_os_print_string
; print string
;
; @param_in string to print. ex m_os_print_string 'Hello World'

%ifmacro m_os_print_string
    %error macro m_os_print_string exists already.
%else
%macro m_os_print_string 1
jmp %%l1
    %%string db %1,0
%%l1:
    mov si, %%string
    syscall os_print_string
%endmacro
%endif

; --------------------------------------------------------
; @macro m_create_string
; create string and return addres in si
;
; @param_in string to create. ex m_create_string 'Hello World'

%ifmacro m_create_string 
    %error macro m_os_create_string exists already.
%else
%macro  m_create_string 1
jmp %%l1
    %%string db %1,0
%%l1:
    mov si, %%string
%endmacro
%endif

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

%define IS_DIRECTORY(fat12) cmp word [fat12 + FAT12_ENTITY.Attributes], 10h


%macro PRINT_ES_DI 0
    pusha

    push es
    pop dx
    syscall os_print_4hex
    syscall os_print_space
    mov dx, di
    syscall os_print_4hex
    syscall os_print_newline

    popa
%endmacro

%endif


; -------------------------------------------------------------
; -------------------------------------------------------------
; -------------------------------------------------------------        simple memory manager
; -------------------------------------------------------------
; -------------------------------------------------------------

%ifndef MEMORY_ASM
%define MEMORY_ASM

%INCLUDE "tomos.inc"

; @description File contains subroutines responsible for memory managment.
; Memory uses by memory manager starts at 2000:E000.
; Maximum block allocated is 8190 bytes. Size of blocks allocated by
; os_mm_alloc are aligned to power of 2.



mem_start dw mm_buffer, 0x2000       ; start of memory to manage


; -------------------------------------------------------------
; structure describing memory each field
; consists a list of free blocks of indicaded size

struc mem_array
    size resw 1
    array_8kb times 2 resw 1
    array_4kb times 2 resw 1
    array_2kb times 2 resw 1
    array_1kb times 2 resw 1
    array_512 times 2 resw 1
    array_256 times 2 resw 1
    array_128 times 2 resw 1
    array_64 times 2 resw 1
    array_32 times 2 resw 1
    array_16 times 2 resw 1
    array_8 times 2 resw 1
    array_4 times 2 resw 1
    array_2 times 2 resw 1
    array_1 times 2 resw 1
endstruc

%DEFINE NUMBER_OF_BLOCKS 14

; ---------------------------------------------------------------
; first 8 bytes of every allocated block
; basicly it creates a list

struc mem_alloc
    .next_offset resw 1
    .next_segment resw 1
    .prev_offset resw 1
    .prev_segment resw 1
endstruc

%DEFINE sizeof_mem_alloc 2+2+2+22

;struc far_address
;    _offset resw 1
;    _segment resw 1
;endstruc

; ---------------------------------------------------------------
; create a memory structure fo the memory manager

mem_free_array:
    istruc mem_array

        at size, dw 14
        at array_8kb, dw 0,0     ; list of 8KB blocks 
        at array_4kb, dw 0,0     ; list of 4KB blocks
        at array_2kb, dw 0,0     ; list of 2KB blocks
        at array_1kb, dw 0,0     ; list of 1KB blocks 
        at array_512, dw 0,0     ; list of 512B blocks 
        at array_256, dw 0,0     ; list of 256B blocks 
        at array_128, dw 0,0     ; list of 128B blocks 
        at array_64, dw 0,0      ; list of 64B blocks 
        at array_32, dw 0,0      ; list of 32B blocks 
        at array_16, dw 0,0      ; list of 16B blocks 
        at array_8, dw 0,0       ; list of 8B blocks 
        at array_4, dw 0,0       ; list of 4B blocks 
        at array_2, dw 0,0       ; list of 2B blocks 
        at array_1, dw 0,0       ; list of 1B blocks 

    iend
mem_free_array_end:

mm_flag dw 0       ; flag indicates if mm was initialized, it is set by _mm_init and clear by _mm_release

; -------------------------------------------------------------
; @proc os_mm_init
; Init memory manager.
;
; @param_in  Nothing
; @param_out Nothing
; @section "memory"
; @note Called by system. No need to call by user

_os_mm_init:
    cmp word [mm_flag], 0
    jnz .exit

        push es
        mov di, mem_free_array                           ; ES:DI address of mem_free_array
        push cs
        pop es
        xor dx, dx                                       ; write 0
        mov cx, mem_free_array_end - mem_free_array      ; size of mem_free_array
        call _os_memory_set                                     ; clear mem_free_array
        pop es

        mov word [mem_free_array + mem_array + size], 14 ; set size of memory struct

        push es                                          ; save ES:DI on the stack
	push di
	push word [mem_start + far_address.offset]
	pop word di
	push word [mem_start + far_address.segment]
	pop word es                                      ; move mem_start far addres to ES:DI

	xor ax, ax
	mov word [es:di+mem_alloc.next_offset] , ax      ; reset
	mov word [es:di+mem_alloc.next_segment] , ax     ; reset
	mov word [es:di+mem_alloc.prev_offset] , ax      ; reset
	mov word [es:di+mem_alloc.prev_segment] , ax     ; reset

	xor ax, ax
	call _mm_push                                    ; set first block (it is the biggest block, here 8KB then it is splited)

        mov word [mm_flag], 1                            ; set flag to idicate that mm was initialized

	pop di
	pop es
    .exit:
    mov ax, word [mm_flag]                               ; return mm_flag
    ret

; -------------------------------------------------------------
; @proc os_mm_release
; Resets memory manager.
;
; @param_in Nothing
; @param_out Nothing
; @section "memory"
; @note Called by system. No need to call by user

_os_mm_release:
    mov word [mm_flag], 0      ; reset flag to indicate that mm is released
    ret

; -------------------------------------------------------------
; @proc os_mm_alloc
; Allocate memory block.
;
; @param_in  DX - number of bytes to alloc
; @param_out ES:DI - address of allocated memory
; @param_out AX - 1 ok , 0 not ok
; @section "memory"

_os_mm_alloc:
    push cx                             ; save CX, BX on the stack
    push bx
    push dx

    mov ax, dx                          ; copy size of memory block to AX

    cmp ax, 6                           ; if user wants less than 7
    ja .l1
        mov ax, 6
    .l1:
    add ax, 2                           ; size of mem_alloc struct
    call _mm_get_free_index             ; get the lowest index in mem_free_array

    inc ax
    test ax, ax                         ; if 0FFh means block too big
    jz near .error                      ; and exit with AX = 00h

    dec ax

    mov word [ds:.index], ax            ; otherwise index in mem_free_array returned, save it
    mov cx, ax                          ; copy CX the lowest index
    inc cx

    
    .next_free:
        dec cx
        cmp cx, -1
        jz near .error                 ; exit loop if CX == -1, means memory full

        mov ax, cx

        call _mm_get_free              ; check if index has free blocks

        test ax, ax                    ; if doesnt try the bigger one
    jz .next_free

    cmp word [.index], cx              ; if index of free block equal to expected return the index
    je .exit


    push es                            ; higher divide it into smaller block and return as free blocks
    pop word [ds:.free_segment]        ; save segment of returned block
    mov word [ds:.free_offset], di     ; save offset of returned block

    mov bx, cx                         ; save CX in BX
    sub cx, NUMBER_OF_BLOCKS - 1       ; translate index
    neg cx

    mov ax, 1
    shl ax, cl                         ; 1^cl - size of returned free block

    mov cx, bx                         ; restore CX
   
    .division_loop:

        mov bx, ax                     
        inc cx
        shr bx, 1                      ; divide free block by 2
        mov ax, cx                     ; index of flield in free_mem_array

        mov di, word [ds:.free_offset]     ; restore ES:DI
        push word [ds:.free_segment]
        pop es
        call _mm_push                      ; free block (size divided by 2, index in AX)

        add  word [ds:.free_offset], bx    ; get another block (simply add a size of block devided by 2)

        mov ax, bx

        cmp cx, word [.index]              ; when reach index he needs exit the loop
 
    jb .division_loop

    mov di , word [ds:.free_offset]        ; save the index in DI
    .exit:

    m2m word [es:di], word [.index]        ; save index of the block in firest 2 bytes of allocated memory
                                           ; is needed to release (indicates a size of the block)
    xor ax, ax
    inc ax 

    add di, 2                              ; skip 2 bytes 

    pop dx
    pop bx                                 ; restore BX and CX from the stack
    pop cx

    ret                                    ; return the procedure

    .error:
    xor ax, ax                             ; return AX = 0

    pop dx
    pop bx                                 ; restore BX ans CX from the stack
    pop cx
    ret

; variables needeb by procedure

.index dw 1    
.free_segment dw 1
.free_offset dw 1
.free_segment_tmp dw 1
.free_offset_tmp dw 1

; -------------------------------------------------------------
; @proc os_mm_realloc
; Reallocate memory block allocated by os_mm_alloc.
;
; @param_in ES:DI - memory to realloc
; @param_in DX - new size in bytes
; @param_out ES:DI - pointer to memory block
; @param_out AX - 1 ok, 0 fails
; @section "memory"
; @note Memory addres returned might me different than 
;       memory addres send to the subroutine. 
;       If reallocation fails ES:DI contains ES:DI send
;       to the procedure

_os_mm_realloc:
    push dx
    push ds

    mov ax, dx

    mov cx, word [es:di-2]
    sub cx, NUMBER_OF_BLOCKS - 1       ; translate index
    neg cx
    mov dx, 1
    shl dx, cl
    add dx, -2
    cmp ax, dx
    jbe .no_alloc

        mov bx, di
        push es
        pop dx

        mov word [._segment], dx
        mov word [._offset], bx

        mov word [._size], ax

        mov dx, ax
        
        call _os_mm_alloc
       
        test ax, ax
        jz .error 

        push es
        push di

        mov cx, word [._size]

        push word [._segment]
        pop ds
        mov si, bx

        call _os_memory_copy

        push word [._segment]
        mov di, word [._offset]
        pop es
        
        call _os_mm_free

        pop di
        pop es
        
    .no_alloc:
     xor ax, ax
     inc ax

     pop ds
     pop dx
     ret

    .error:
    xor ax, ax

    pop ds
    pop dx
    ret

._size dw 1
._offset dw 1
._segment dw 1


; -------------------------------------------------------------
; _mm_push - push on the top of the list
; IN: AX - index in mem_free_array
;     ES:DI - element to add
; OUT: AX - 1 ok, 0 bad

_mm_push:
    push es
    push di

    push es
    pop word [.e_segment]
    mov word [.e_offset], di


    call _mm_get_last
    test ax, ax
    jnz .list_push
        push word [.e_segment]
        pop word [di + far_address.segment]
        push word [.e_offset]
        pop word [di + far_address.offset]

        jmp .exit 
    .list_push:
        push word [.e_segment]
        pop word [es:di + mem_alloc.next_segment]
        
        push word [.e_offset]
        pop word [es:di + mem_alloc.next_offset]

        push word [.e_offset]
        push word [.e_segment]

        mov word [.e_offset], di
        push es    
        pop word [.e_segment]

        pop es
        pop di

        mov word [es:di + mem_alloc.next_offset], 0
        mov word [es:di + mem_alloc.next_segment], 0

        push word [.e_offset]
        pop word [es:di + mem_alloc.prev_offset]

        push word [.e_segment]
        pop word [es:di + mem_alloc.prev_segment]
    .exit:
    
    pop di
    pop es
    ret

.e_segment dw 1
.e_offset dw 1

    

; -------------------------------------------------------------
; _get_last - get last element from the list
; IN: AX - index
; OUT: ES:DI - segment:offset
;      AX - true if success

_mm_get_last:
    push bx

    mov bx, ax
    mov word [.index], ax

    shl bx, 2
    mov di, word [mem_free_array + mem_array + array_8kb + bx + far_address.offset]
    mov ax, word [mem_free_array + mem_array + array_8kb + bx + far_address.segment]
    mov es, ax

    test ax, ax
    jz .empty

    .list_jump_loop:

        mov bx, word [es:di + mem_alloc.next_offset]
        mov ax, word [es:di + mem_alloc.next_segment]
        
        test ax, ax
        jz .found_last

        mov di, bx
        push ax
        pop es
    jmp .list_jump_loop

    .found_last:

    xor ax, ax
    inc ax

    pop bx
    ret

    .empty:

    ; BX contains index * 4 

    lea di , [mem_free_array + mem_array + array_8kb + bx]
    push  2000h
    pop es

    xor ax, ax
    
    pop bx
    ret

.index dw 1

; -------------------------------------------------------------
; _mm_get_free_index - assiociate size of block with index in mem_free_array
; IN: AX - number of bytes to alloc
; OUT: ES - segment of allocated memory
;      DI - offset of allocated memory

_mm_get_free_index:
    push bx
    push dx
    push cx
   
    mov bx, 10000000000000b
    mov dx, ax

    xor cx, cx
    dec cx
    
    .get_free_memory_index_loop:
        inc cx

        test bx, bx
        jz .error

        mov ax, dx

        and ax, bx
        shr bx, 1

        test ax, ax
    jz .get_free_memory_index_loop

    mov ax, dx

    shl bx, 1
    not bx

    and ax, bx
    test ax, ax
    jz .no_increment
        dec cx
    .no_increment:

    mov ax, cx

    pop cx
    pop dx
    pop bx
    ret
    
.error:
    xor ax, ax
    dec ax

    pop cx
    pop dx
    pop bx
    ret


; -------------------------------------------------------------
; _free_block - add a block as a free block
; IN: DI - offset
;     ES - segment

_mm_free_block:
    push bx
   
    xor dx, dx

    mov word [di], dx
    mov word [di+2], dx
    mov word [di+4], dx
    mov word [di+6], dx

    push es
    pop word [.t_segment]

    add di, -2

    mov word [.t_offset], di
    mov word [.t_offset_end], di

    mov bx, word [es:di]

    mov cx, bx
    mov word [.t_index], bx

    shl bx, 2
    add bx, 2

    mov di, word [mem_free_array + bx + far_address.offset]
    mov ax, word [mem_free_array + bx + far_address.segment]
    push ax
    pop es

    mov dx, 1
    sub cx, 13
    neg cx 
    shl dx, cl

    .list_jump_loop:

        test ax, ax
	jz .make_it_free

        cmp ax, word [.t_segment]
        jnz .next_loop

        mov cx, word [.t_offset_end]
        cmp di, cx
        jz .connect_block_12

        add di, dx
        cmp di, word [.t_offset]
        jz .connect_block_21

        .next_loop:

        mov ax, word [es:di + mem_alloc.next_segment]
        mov di, word [es:di + mem_alloc.next_offset]

        push ax
        pop es
    jmp .list_jump_loop

    .make_it_free:

    mov di, word [.t_offset]
    mov ax, word [.t_segment]
    mov es, ax
    mov ax, word [.t_index]
    call _mm_push

    xor ax, ax
    inc ax

    pop bx
    ret

.connect_block_12:

    mov ax, word [.t_index]
    call _mm_delete_from_list

    mov ax, word [.t_segment]
    mov es, ax
    mov di, word [.t_offset]

    mov ax, word [.t_index]
    dec ax
    mov word [es:di], ax
    sub di, -2
    call _mm_free_block

    xor ax, ax
    inc ax

    pop bx
    ret

.connect_block_21:

    sub di, dx
    
    mov ax, word [.t_index]
    call _mm_delete_from_list

    mov ax, word [.t_index]
    dec ax
    mov word [es:di], ax
    sub di, -2
    call _mm_free_block

    xor ax, ax
    inc ax

    pop bx
    ret

.t_segment dw 1
.t_offset dw 1
.t_offset_end dw 1
.t_index dw 1
        
; -------------------------------------------------------------
; _mm_delete_from_list - delete block from the free array list
; IN: ES_DI - block to remove
;     AX - index
; OUT: Nothing

_mm_delete_from_list: 
     push di
     push es

     mov word [.d_index], ax

     mov ax, word [es:di + mem_alloc.next_segment]
     test ax, ax
     jz .check_prev

         push word [es:di + mem_alloc.next_offset]
	 push word [es:di + mem_alloc.next_segment]

	 push word [es:di + mem_alloc.prev_offset]
	 push word [es:di + mem_alloc.prev_segment]

         mov di, [es:di + mem_alloc.next_offset]
         push ax
         pop es

	 pop word [es:di + mem_alloc.prev_segment]
	 pop word [es:di + mem_alloc.prev_offset]

	 mov ax, word [es:di + mem_alloc.prev_segment]
         test ax, ax
         jz .no_prev

	 push word [es:di + mem_alloc.prev_offset]
	 mov es, ax
	 pop di

	 pop word [es:di + mem_alloc.next_segment]
	 pop word [es:di + mem_alloc.next_offset]
         jz .exit

     .check_prev:
         push word [es:di + mem_alloc.next_offset]
	 push word [es:di + mem_alloc.next_segment]

	 mov ax, word [es:di + mem_alloc.prev_segment]
         test ax, ax
         jz .no_prev

	 push word [es:di + mem_alloc.prev_offset]
	 mov es, ax
	 pop di

	 pop word [es:di + mem_alloc.next_segment]
	 pop word [es:di + mem_alloc.next_offset]
     .exit:

     pop es
     pop di
     ret
    

.no_prev:
    sub sp, -4

    mov bx, word [.d_index]
    shl bx, 2
    xor dx, dx
    mov word [mem_free_array + mem_array + array_8kb + bx + far_address.offset], dx
    mov word [mem_free_array + mem_array + array_8kb + bx + far_address.segment], dx
    pop es
    pop di
    ret
    
.d_index dw 1


; -------------------------------------------------------------
; _get_free_block - get free block
; IN:  AX - index
; OUT: DI - offset
;      ES - segment
;      AX - true if success, false if full

_mm_get_free:
    push bx
    push dx

    mov bx, ax
    shl bx, 2

    mov ax, word [mem_free_array + mem_array + array_8kb + bx + far_address.segment]

    test ax, ax
    jz .no_free

    mov di, word [mem_free_array+mem_array + array_8kb + bx + far_address.offset]
    push ax
    pop es

    m2m word [mem_free_array + mem_array + array_8kb + bx + far_address.offset] , word [es:di + mem_alloc.next_offset]   ; copy mem to mem (m2m defined in macro.mac)

    mov dx, word [es:di + mem_alloc.next_segment]
    mov word [mem_free_array + mem_array + array_8kb + bx + far_address.segment] , dx

    test dx, dx
    jz .exit

    push di
    push ax

    push dx
    pop es

    mov di, word [es:di + mem_alloc.next_offset]

    xor ax, ax
    mov word [es:di + mem_alloc.prev_offset], ax
    mov word [es:di + mem_alloc.prev_segment], ax

    pop es
    pop di

    .exit:

    xor ax, ax
    inc ax
   
    pop dx 
    pop bx
    ret

    .no_free:
    xor ax, ax
    
    pop dx
    pop bx
    ret

; -------------------------------------------------------------
; @proc os_mm_free
; Free allocated memory.
;
; @param_in  ES:DI memory block allocated by os_mm_alloc or os_mm_realloc
; @param_out  Nothing
; @section "memory"
_os_mm_free:
    pusha
    call _mm_free_block
    popa
    ret


; -------------------------------------------------------------
; @proc os_mm_get_size
; Get size of alloced memory block.
; Returned value might vary from a size value sent to os_mm_alloc. 
; (grater or equal than sent to os_mm_alloc).
;
; @param_in  ES:DI - poineter to the block
; @param_out AX - size of the block
; @section "memory"
; @note returned value is an align to the nearest power of 2

_os_mm_get_size:
    push cx
    mov cx, word [es:di-2]
    sub cx, NUMBER_OF_BLOCKS - 1       ; translate index
    neg cx
    mov ax, 1
    shl ax, cl
    add ax, -2
    pop cx
    ret


_mm_print_free_mem:
    pusha
    pushf

    xor ax, ax
    xor bx, bx

    
    mov si , ._menu
    call _os_print_string
    
    .loop:
        mov cx, bx
	sub cx, NUMBER_OF_BLOCKS - 1       ; translate index
	neg cx

        cmp cx, 0
        jl .exit

        mov dx, bx
        shl bx, 1
        lea si, [.a_strings+bx]
        mov si , [si]
        call _os_print_string
        mov bx, dx

        mov ax, bx
    
        call _number_of_free_blocks
        mov dx, ax

        call _os_print_4hex
        call _os_print_space

        mov ax, dx
        shl dx, 1

        shl ax, cl
        sub ax, dx
        mov dx, ax

        call _os_print_4hex
        call _os_print_newline
         
        inc bx
    jmp .loop
    .exit:

    popf
    popa
    ret 

._menu db 'SIZE NBLK FMEM',10,13,0

._dw db ':',0 
._8k dw  '8K   ',0
._4k dw  '4K   ',0
._2k dw  '2K   ',0
._1k dw  '1K   ',0
._512 dw '512  ',0
._256 dw '256  ',0
._128 dw '128  ',0
._64 dw  '64   ',0
._32 dw  '32   ',0
._16 dw  '16   ',0
._8 dw   '8    ',0
._4 dw   '4    ',0
._2 dw   '2    ',0
._1 dw   '1    ',0

.a_strings dw ._8k, ._4k, ._2k, ._1k, ._512, ._256, ._128, ._64, ._32, ._16, ._8, ._4, ._2, ._1

; -----------------------------------------------
; _number_of_free_blocks - get number of free blocks 
; IN: AX - index
; OUT: AX - number of blocks

_number_of_free_blocks:
    push cx
    push bx
    push dx
    push ds
    
    push TOMOS_KERNEL_SEGMENT
    pop ds

    mov bx, ax
    shl bx, 2
    add bx, 2
    
    lea bx, [mem_free_array + bx]

    mov dx, [bx + far_address.segment]
    mov ax, [bx + far_address.offset]

    xor cx, cx
    
    .loop:
        test dx, dx
        jz .exit

        inc cx

        mov bx, ax
        push dx
        pop ds

        mov dx, [bx + far_address.segment]
        mov ax, [bx + far_address.offset]

    jmp .loop

    .exit:
    mov ax, cx

    pop ds
    pop dx
    pop bx
    pop cx
    ret        

; ------------------------------------------------------------
; _mm_get_free_memory_size - get available memory to allocate
; IN: Nothing
; OUT: Nothing

_os_mm_get_free_mem_size:
 
_mm_get_free_memory_size:
    push cx
    push dx

    xor ax, ax
    xor bx, bx
    
    mov word [.free_memory_size], ax   


    .loop:
        mov cx, bx
	sub cx, NUMBER_OF_BLOCKS - 1       ; translate index
	neg cx

        cmp cx, 0
        jl .exit

        mov ax, bx
    
        call _number_of_free_blocks
        mov dx, ax
        shl dx, 1

        shl ax, cl
        sub ax, dx
        add word [.free_memory_size], ax
         
        inc bx
    jmp .loop
    .exit:

    mov ax, word [.free_memory_size]

    pop dx
    pop cx
    
    ret 
  
.free_memory_size dw 0     


%endif

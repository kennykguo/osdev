.section .data
#######GLOBAL VARIABLES########
heap_begin:     .long 0      # Start of managed memory
current_break:  .long 0      # End of managed memory

######STRUCTURE INFORMATION####
.equ HEADER_SIZE,       8    # Size of memory region header
.equ HDR_AVAIL_OFFSET, 0    # Offset for availability flag
.equ HDR_SIZE_OFFSET,  4    # Offset for size field

###########CONSTANTS ###########
.equ UNAVAILABLE,    0       # Memory in use
.equ AVAILABLE,      1       # Memory is free
.equ SYS_BRK,        45      # Break system call number
.equ LINUX_SYSCALL,  0x80    # Linux syscall interrupt

.section .text
##########FUNCTIONS############

# allocate_init - Initialize heap management
# No parameters, no return value
.globl allocate_init
.type allocate_init,@function
allocate_init:
    pushl %ebp
    movl %esp, %ebp

    # Get break point
    movl $SYS_BRK, %eax
    movl $0, %ebx
    int $LINUX_SYSCALL

    incl %eax                  # Next available address
    movl %eax, current_break
    movl %eax, heap_begin

    movl %ebp, %esp
    popl %ebp
    ret

# allocate - Allocate memory block
# Parameter: size of requested memory (at 8(%ebp))
# Returns: memory address in %eax or 0 if error
.globl allocate
.type allocate,@function
.equ ST_MEM_SIZE, 8          # Stack position of size parameter

allocate:
    pushl %ebp
    movl %esp, %ebp
    
    movl ST_MEM_SIZE(%ebp), %ecx  # Get requested size
    movl heap_begin, %eax         # Start of search
    movl current_break, %ebx      # End of search

alloc_loop_begin:
    cmpl %ebx, %eax              # Check if at end
    je move_break

    movl HDR_SIZE_OFFSET(%eax), %edx       # Get block size
    cmpl $UNAVAILABLE, HDR_AVAIL_OFFSET(%eax)
    je next_location

    cmpl %edx, %ecx              # Check if block big enough
    jle allocate_here

next_location:
    addl $HEADER_SIZE, %eax      # Move to next block
    addl %edx, %eax
    jmp alloc_loop_begin

allocate_here:
    movl $UNAVAILABLE, HDR_AVAIL_OFFSET(%eax)
    addl $HEADER_SIZE, %eax      # Point to usable memory
    movl %ebp, %esp
    popl %ebp
    ret

move_break:
    addl $HEADER_SIZE, %ebx      # Space for header
    addl %ecx, %ebx              # Space for data

    pushl %eax                   # Save registers
    pushl %ecx
    pushl %ebx

    movl $SYS_BRK, %eax         # Request memory
    int $LINUX_SYSCALL

    cmpl $0, %eax               # Check for error
    je error

    popl %ebx                   # Restore registers
    popl %ecx
    popl %eax

    movl $UNAVAILABLE, HDR_AVAIL_OFFSET(%eax)
    movl %ecx, HDR_SIZE_OFFSET(%eax)
    addl $HEADER_SIZE, %eax
    movl %ebx, current_break

    movl %ebp, %esp
    popl %ebp
    ret

error:
    movl $0, %eax              # Return 0 on error
    movl %ebp, %esp
    popl %ebp
    ret

# deallocate - Free memory block
# Parameter: address to free (at 4(%esp))
# No return value
.globl deallocate
.type deallocate,@function
.equ ST_MEMORY_SEG, 4         # Stack position of address

deallocate:
    movl ST_MEMORY_SEG(%esp), %eax
    subl $HEADER_SIZE, %eax        # Get to header
    movl $AVAILABLE, HDR_AVAIL_OFFSET(%eax)
    ret
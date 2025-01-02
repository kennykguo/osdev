.section .data
    hex_chars_lower: .ascii "0123456789abcdef"
    hex_chars_upper: .ascii "0123456789ABCDEF"
    buffer: .space 1024    # Output buffer

.section .text
    .globl pprintf
    .type pprintf, @function

# System call numbers
.equ SYS_WRITE, 4
.equ STDOUT, 1

########################
# pprintf function
# Stack frame:
#   8(%ebp)  - format string pointer
#   12(%ebp) - first variable argument
#   16(%ebp) - second variable argument
#   ...and so on
########################
pprintf:
    pushl %ebp
    movl %esp, %ebp
    pushl %ebx
    pushl %esi
    pushl %edi

    movl 8(%ebp), %esi       # format string pointer
    movl $buffer, %edi       # output buffer pointer
    movl $12, %ebx          # current argument offset from ebp

    # Main processing loop
process_char:
    movb (%esi), %al        # Get next character
    testb %al, %al
    jz done                 # If null terminator, we're done

    cmpb $'%', %al          # Check for format specifier
    je format_char

    # Regular character - copy to buffer
    movb %al, (%edi)
    incl %edi
    incl %esi
    jmp process_char

format_char:
    incl %esi               # Skip '%'
    movb (%esi), %al        # Get format character

    cmpb $'d', %al
    je handle_int
    cmpb $'s', %al
    je handle_string
    cmpb $'x', %al
    je handle_hex_lower
    cmpb $'X', %al
    je handle_hex_upper
    cmpb $'c', %al
    je handle_char
    cmpb $'%', %al
    je handle_percent

    # Unknown format - output % and character
    movb $'%', (%edi)
    incl %edi
    movb %al, (%edi)
    incl %edi
    incl %esi
    jmp process_char

handle_int:
    movl (%ebp,%ebx,1), %eax    # Get integer argument
    addl $4, %ebx               # Move to next argument
    
    # Handle negative numbers
    testl %eax, %eax
    jns positive_int
    negl %eax
    movb $'-', (%edi)
    incl %edi

positive_int:
    pushl %edi                  # Save buffer position
    call convert_int
    popl %edi                  # Restore and update buffer position
    addl %eax, %edi
    incl %esi
    jmp process_char

handle_string:
    movl (%ebp,%ebx,1), %eax    # Get string pointer
    addl $4, %ebx

copy_string:
    movb (%eax), %cl
    testb %cl, %cl
    jz string_done
    movb %cl, (%edi)
    incl %eax
    incl %edi
    jmp copy_string

string_done:
    incl %esi
    jmp process_char

handle_hex_lower:
    movl (%ebp,%ebx,1), %eax
    addl $4, %ebx
    movl $hex_chars_lower, %ecx
    call convert_hex
    incl %esi
    jmp process_char

handle_hex_upper:
    movl (%ebp,%ebx,1), %eax
    addl $4, %ebx
    movl $hex_chars_upper, %ecx
    call convert_hex
    incl %esi
    jmp process_char

handle_char:
    movl (%ebp,%ebx,1), %eax
    addl $4, %ebx
    movb %al, (%edi)
    incl %edi
    incl %esi
    jmp process_char

handle_percent:
    movb $'%', (%edi)
    incl %edi
    incl %esi
    jmp process_char

done:
    # Write the buffer
    movl $SYS_WRITE, %eax
    movl $STDOUT, %ebx
    movl $buffer, %ecx
    subl $buffer, %edi        # Calculate length
    movl %edi, %edx
    int $0x80

    # Restore registers and return
    popl %edi
    popl %esi
    popl %ebx
    movl %ebp, %esp
    popl %ebp
    ret

########################
# Integer to string conversion
# Input: %eax = number
# Output: string at (%edi), length in %eax
########################
convert_int:
    pushl %esi
    pushl %edi
    pushl %ebx
    
    movl $10, %ebx           # Divisor
    movl %edi, %esi          # Save start of buffer
    
convert_loop:
    movl $0, %edx           # Clear for division
    divl %ebx
    addb $'0', %dl          # Convert remainder to ASCII
    movb %dl, (%edi)        # Store digit
    incl %edi
    testl %eax, %eax
    jnz convert_loop
    
    # Reverse the string
    movl %edi, %eax         # End pointer
    decl %eax
    movl %esi, %edi         # Start pointer
    
reverse_loop:
    cmpl %eax, %edi
    jae reverse_done
    movb (%edi), %bl
    movb (%eax), %bh
    movb %bh, (%edi)
    movb %bl, (%eax)
    incl %edi
    decl %eax
    jmp reverse_loop
    
reverse_done:
    movl %esi, %edi         # Restore buffer pointer
    subl %esi, %eax         # Calculate length
    incl %eax
    
    popl %ebx
    popl %edi
    popl %esi
    ret

########################
# Hex conversion
# Input: %eax = number, %ecx = hex chars table
# Uses: %edi = buffer pointer
########################
convert_hex:
    pushl %esi
    pushl %edi
    pushl %ebx
    
    movl %edi, %esi          # Save buffer start
    movl $8, %ebx            # Process 8 nibbles (32 bits)
    
hex_loop:
    roll $4, %eax            # Get next nibble
    movl %eax, %edx
    andl $0xF, %edx          # Mask to 4 bits
    movb (%ecx,%edx,1), %dl  # Get hex character
    movb %dl, (%edi)         # Store in buffer
    incl %edi
    decl %ebx
    jnz hex_loop
    
    movl %edi, %eax          # Calculate length
    subl %esi, %eax
    
    popl %ebx
    popl %edi
    popl %esi
    ret
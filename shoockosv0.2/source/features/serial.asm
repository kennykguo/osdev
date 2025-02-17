; ==================================================================
; MikeOS -- The Mike Operating System kernel
; Copyright (C) 2006 - 2009 MikeOS Developers -- see doc/LICENSE.TXT
;
; SERIAL PORT ROUTINES
; ==================================================================

; ------------------------------------------------------------------
; os_serial_port_enable -- Set up the serial port for transmitting data
; IN/OUT: Nothing

os_serial_port_enable:
	pusha
	mov dx, 0			; Configure serial port 1
	mov al, 11100011b		; 9600 baud, no parity, 8 data bits, 1 stop bit
	mov ah, 0
	int 14h
	popa
	ret


; ------------------------------------------------------------------
; os_send_via_serial -- Send a byte via the serial port
; IN: AL = byte to send via serial; OUT: AH = Bit 7 clear on success

os_send_via_serial:
	pusha

	mov ah, 01h
	mov dx, 0			; COM1

	int 14h

	mov [.tmp], ax

	popa

	mov ax, [.tmp]

	ret


	.tmp dw 0


; ------------------------------------------------------------------
; os_get_via_serial -- Get a byte from the serial port
; OUT: AL = byte that was received; OUT: AH = Bit 7 clear on success

os_get_via_serial:
	pusha

	mov ah, 02h
	mov dx, 0			; COM1

	int 14h

	mov [.tmp], ax

	popa

	mov ax, [.tmp]

	ret


	.tmp dw 0


; ==================================================================


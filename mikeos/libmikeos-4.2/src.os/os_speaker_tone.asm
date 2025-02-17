
; @@@ void mikeos_speaker_tone(unsigned int divisor);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_speaker_tone

_mikeos_speaker_tone:
	mov	bx, sp
	mov	ax, [ss:bx + 2]

	mov	bx, os_speaker_tone
	call	bx

	ret


; @@@ void mikeos_speaker_off(void);

%include "os_vector.inc"

	section .text
	use16
	global	_mikeos_speaker_off

_mikeos_speaker_off:
	mov	bx, os_speaker_off
	call	bx

	ret

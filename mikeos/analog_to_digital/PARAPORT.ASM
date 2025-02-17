;
;            PARALLEL PORT ACCESS FOR ~IBM~ PC TYPE COMPUTERS.
;            -------------------------------------------------
;
;      (C)1998  B.Walker, (G0LCU). Email to:-  wisecracker@tesco.net
;      -------------------------------------------------------------
;
;----------------------------------------------------------------------------
;
; This is a subroutine only and the MS-DOS (TM) ~DEBUG~ command is used to
; check that it works properly.
;
; It was compiled using the A86/D86 (C) combination and is only 82 Bytes
; long. See ~(C)-AD86.TXT~ for further information.
;
;----------------------------------------------------------------------------
;
; NOTE:- This source code is FREE and can be used by anyone who needs it.
; This is issued under PD 2009, for the MikeOS 16 bit operating system.
;
;----------------------------------------------------------------------------
;
; Enter the subroutine here.
; subroutinestart:
        pushf                           ;Push used registers.
        push    dx                      ;Note that the flags are ignored.
        push    cx                      ;
        push    ax                      ;
        mov     dx,037ah                ;Point to parallel port control bits.
        mov     ax,00ah                 ;Set -STROBE high & clear other bits.
        out     dx,al                   ;Do it.
        mov     al,00bh                 ;Now trigger the -STROBE bit.
        out     dx,al                   ;Do it.
        mov     al,8                    ;Set my board to 4 highest bits.
        out     dx,al                   ;Do it.
        call    timedelay               ;Hold the port pins for a while.
        mov     dx,0379h                ;Point to port status bits.
        in      al,dx                   ;Read 4 bit data, high nibble.
        shl     al,1                    ;Shift into top 4 bits.
        and     al,0f0h                 ;Ensure 4 high bits only.
        mov     ah,al                   ;Store data into ah register.
        mov     dx,037ah                ;Point to port control bits again.
        mov     al,00ah                 ;Keep -STROBE high & clear other bits.
        out     dx,al                   ;Do it.
        mov     al,2                    ;Now set my board to 4 lowest bits.
        out     dx,al                   ;Do it.
        call    timedelay               ;Hold these port pins for a while.
        mov     dx,0379h                ;Point to port status bits.
        in      al,dx                   ;Read 4 bit data, low nibble.
        shr     al,1                    ;Shift to the right 3 times.
        shr     al,1                    ;
        shr     al,1                    ;
        and     al,00fh                 ;Ensure low 4 bits only.
        add     al,ah                   ;Add high nibble to low nibble.
        xor     ah,ah                   ;Clear ah register.
        mov     store,al                ;We now have the byte we want.
        mov     dx,037ah                ;Point to control bits again.
        mov     al,00ah                 ;Keep -STROBE high & clear other bits.
        out     dx,al                   ;Do it.
        pop     ax                      ;Pop the registers back.
        pop     cx                      ;
        pop     dx                      ;
        popf                            ;
        ret                             ;Retrun back to main calling routine.
timedelay:
        mov     cx,0ffffh               ;Load the cx register as a counter.
timerhold:
        sub     cx,1                    ;Count down by 1
        jnz     timerhold               ;Subtract 1 from counter until finshed.
        ret                             ;Return to calling routine.
store   db      0c3h                    ;My A-D Converter byte saved.

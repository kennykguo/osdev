; ---------------------------------------------------------------
; Hello World program for TomOS
;
; To find out more about writing applications for TomOS
; see doc/examples.html
; ---------------------------------------------------------------

CPU 386
BITS 16
ORG 100h

%include '../include/tomos.inc'

; The macro m_os_print_string_nl declared in the include/tomos.inc file.

start:
    m_os_print_string_nl 'Hello World!'    ; print the string on the screen
    ret

; end of program

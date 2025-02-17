; ---------------------------------------------------------
; syscalls.inc is automaticly included during a compilation
; by the ./build-linuc.sh script to allow invoking system 
; calls.

%ifndef SYSCALLS_INC
%DEFINE SYSCALLS_INC

%macro DECLARE_SYSCALLS_ID 1-*
    %assign id 0
    %assign loop %0
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
	%rotate 1
	%assign id id+1
    %endrep
%endmacro

; -----------------------------------------------------
; @macro syscall
; uses to call system routines
; 
; @param_in systemcall ID declared my DECLARE_SYSCALLS_ID (see doc/example)
; @param_out depends on called system routine

%macro syscall 1
    mov ax, ID_%1
    int 47h
%endmacro

%macro call 1
    %ifdef ID_%1
    syscall %1
    %else
    call %1
    %endif
%endmacro

; ------------------------------------------------
; declaration of all systemcalls provided by TomOS
; new systemcalls should be appended to the list to 
; keep compatibility with earlier API versions.
; MUST BE CHANGED TOGETHER WITH source/syscalls.asm

DECLARE_SYSCALLS_ID  os_mm_init, \
                  os_mm_release, \
                  os_mm_alloc, \
                  os_mm_realloc, \
                  os_mm_free, \
                  os_mm_get_size, \
                  os_int47h_get_syscall, \
                  os_int47h_set_syscall, \
                  os_int47h_get_syscall_id,  \
		  os_mouse_init, \
                  os_mouse_show , \
                  os_mouse_hide , \
                  os_mouse_get_position , \
                  os_mouse_set_position , \
                  os_mouse_get_bt_press_info , \
                  os_mouse_get_bt_release_info , \
                  os_mouse_set_h_limit , \
                  os_mouse_set_v_limit , \
                  os_mouse_set_text_cursor , \
                  os_mouse_set_user_subroutine , \
                  os_mouse_enable , \
                  os_mouse_disable , \
                  os_mouse_wait_for_l_click , \
                  os_speaker_tone, \
                  os_speaker_off , \
                  os_cursor_move, \
                  os_cursor_get_pos, \
                  os_cursor_show, \
                  os_serial_send, \
                  os_serial_get, \
                  os_date_get_string, \
                  os_date_set_fmt, \
                  os_dump_registers, \
                  os_file_selector, \
                  os_list_dialog, \
                  os_input_dialog, \
                  os_dialog_box, \
                  os_draw_block, \
                  os_draw_background, \
                  os_fatal_error, \
                  os_get_file_list, \
                  os_filename_convert, \
                  os_bcd_to_int, \
                  os_kbd_wait_for_key, \
                  os_kbd_check_for_key, \
                  os_kbd_flush_buffer,  \
                  os_kbd_check_buffer, \
                  os_kbd_get_char,  \
                  os_input_string, \
                  os_long_int_to_string, \
                  os_long_int_negate, \
                  os_pause, \
                  os_print_string, \
                  os_print_horiz_line, \
                  os_print_newline, \
                  os_print_space, \
                  os_dump_string, \
                  os_print_digit, \
                  os_print_1hex, \
                  os_print_2hex, \
                  os_print_4hex, \
                  os_clear_screen, \
                  os_int_to_string, \
                  os_string_length, \
                  os_string_find_char, \
                  os_string_uppercase, \
                  os_string_lowercase, \
                  os_string_copy, \
                  os_string_truncate, \
                  os_string_join, \
                  os_string_chomp, \
                  os_string_strip, \
                  os_string_compare, \
                  os_string_strincmp, \
                  os_get_api_version, \
                  os_get_int_handler, \
                  os_modify_int_handler, \
                  os_int_reboot ,\
		  os_memory_copy, \
		  os_memory_set, \
		  os_print_char, \
		  os_file_write, \
		  os_dir_open, \
		  os_dir_next_file,  \
		  os_dir_close, \
		  os_dir_change, \
                  os_file_convert_d2f ,\
                  os_file_convert_f2d , \
		  os_mm_get_free_mem_size, \
		  os_string_not_find_char , \
		  os_command_line_get, \
		  os_char_uppercase,\
		  os_char_lowwercase




%endif

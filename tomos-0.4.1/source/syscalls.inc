%ifndef SYSCALLS_INC
%DEFINE SYSCALLS_INC
tomos_int47h_calls:
; ------------------------------------------------
; declaration of all systemcalls provided by TomOS
; new systemcalls should be appended to the list to 
; keep compatibility with earlier API versions.

DECLARE_SYSCALLS  os_mm_init,TKS, \
                  os_mm_release, TKS, \
                  os_mm_alloc, TKS, \
                  os_mm_realloc, TKS, \
                  os_mm_free, TKS, \
                  os_mm_get_size, TKS, \
                  os_int47h_get_syscall, TKS, \
                  os_int47h_set_syscall, TKS, \
                  os_int47h_get_syscall_id, TKS,  \
		  os_mouse_init, TKS, \
                  os_mouse_show , TKS, \
                  os_mouse_hide , TKS, \
                  os_mouse_get_position , TKS, \
                  os_mouse_set_position , TKS, \
                  os_mouse_get_bt_press_info , TKS, \
                  os_mouse_get_bt_release_info , TKS, \
                  os_mouse_set_h_limit , TKS, \
                  os_mouse_set_v_limit , TKS, \
                  os_mouse_set_text_cursor , TKS, \
                  os_mouse_set_user_subroutine , TKS, \
                  os_mouse_enable , TKS, \
                  os_mouse_disable , TKS, \
                  os_mouse_wait_for_l_click , TKS, \
                  os_speaker_tone, TKS , \
                  os_speaker_off , TKS , \
                  os_cursor_move, TKS , \
                  os_cursor_get_pos, TKS, \
                  os_cursor_show, TKS, \
                  os_serial_send, TKS  , \
                  os_serial_get, TKS , \
                  os_date_get_string, TKS , \
                  os_date_set_fmt, TKS , \
                  os_dump_registers, TKS , \
                  os_file_selector, TKS , \
                  os_list_dialog, TKS , \
                  os_input_dialog, TKS , \
                  os_dialog_box, TKS , \
                  os_draw_block, TKS , \
                  os_draw_background, TKS , \
                  os_fatal_error, TKS , \
                  os_get_file_list, TKS , \
                  os_filename_convert, TKS , \
                  os_bcd_to_int, TKS , \
                  os_kbd_wait_for_key, TKS , \
                  os_kbd_check_for_key, TKS , \
                  os_kbd_flush_buffer, TKS , \
                  os_kbd_check_buffer, TKS , \
                  os_kbd_get_char, TKS , \
                  os_input_string, TKS , \
                  os_long_int_to_string, TKS , \
                  os_long_int_negate, TKS , \
                  os_pause, TKS , \
                  os_print_string, TKS , \
                  os_print_horiz_line, TKS , \
                  os_print_newline, TKS , \
                  os_print_space, TKS , \
                  os_dump_string, TKS , \
                  os_print_digit, TKS , \
                  os_print_1hex, TKS , \
                  os_print_2hex, TKS , \
                  os_print_4hex, TKS , \
                  os_clear_screen, TKS , \
                  os_int_to_string, TKS , \
                  os_string_length, TKS , \
                  os_string_find_char, TKS , \
                  os_string_uppercase, TKS , \
                  os_string_lowercase, TKS , \
                  os_string_copy, TKS , \
                  os_string_truncate, TKS , \
                  os_string_join, TKS , \
                  os_string_chomp, TKS , \
                  os_string_strip, TKS , \
                  os_string_compare, TKS , \
                  os_string_strincmp, TKS , \
                  os_get_api_version, TKS , \
                  os_get_int_handler, TKS , \
                  os_modify_int_handler, TKS , \
                  os_int_reboot, TKS, \
		  os_memory_copy, TKS, \
		  os_memory_set, TKS, \
		  os_print_char, TKS, \
		  os_file_write, TKS, \
		  os_dir_open, TKS, \
		  os_dir_next_file, TKS, \
		  os_dir_close, TKS, \
		  os_dir_change, TKS ,\
                  os_file_convert_d2f , TKS ,\
                  os_file_convert_f2d , TKS, \
		  os_mm_get_free_mem_size , TKS, \
		  os_string_not_find_char , TKS, \
		  os_command_line_get, TKS, \
		  os_char_uppercase, TKS, \
		  os_char_lowwercase, TKS

tomos_int47h_calls_end:

%endif

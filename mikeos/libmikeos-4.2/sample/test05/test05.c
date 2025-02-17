/* test05.c - misc functions (1) */

#include <mikeos.h>

/*
	used MikeOS API :-

	mikeos_bcd_to_int()
	mikeos_check_for_key()
	mikeos_clear_screen()
	mikeos_dialog_box()
	mikeos_draw_background()
	mikeos_draw_block()
	mikeos_dump_registers()
	mikeos_dump_string()
	mikeos_fatal_error()
	mikeos_get_api_version()
	mikeos_get_file_list()
	mikeos_get_random()
	mikeos_print_1hex()
	mikeos_print_2hex()
	mikeos_print_4hex()
	mikeos_wait_for_key()
	mikeos_input_dialog()
	mikeos_list_dialog()

	mikeos_print_newline()
	mikeos_print_space()
	mikeos_print_string()
	mikeos_pause()
*/

static	char	StringOK[] = "OK";
static	char	StringNG[] = "NG";
static	char	Buf[4096];

/* fill with garbage to check zero-terminated string */
static	void	fill_buffer(char *buf, int size)
{
	int	i;

	for (i = 0; i < size; i++) buf[i] = 'X';
	return;
}

/* test os_print_Nhex */
static	void	test_os_print_Nhex(void)
{
	static	char	str0[] = "mikeos_print_1hex";
	static	char	str1[] = "mikeos_print_2hex";
	static	char	str2[] = "mikeos_print_4hex";

	/* 1hex */
	mikeos_print_string(str0);
	mikeos_print_space();
	mikeos_print_1hex(0x1111);	/* 1 */
	mikeos_print_space();
	mikeos_print_1hex(0x2222);	/* 2 */
	mikeos_print_space();
	mikeos_print_1hex(0xaaaa);	/* A */
	mikeos_print_space();
	mikeos_print_1hex(0xbbbb);	/* B */
	mikeos_print_newline();

	/* 2hex */
	mikeos_print_string(str1);
	mikeos_print_space();
	mikeos_print_2hex(0x1111);	/* 11 */
	mikeos_print_space();
	mikeos_print_2hex(0x2222);	/* 22 */
	mikeos_print_space();
	mikeos_print_2hex(0xaaaa);	/* AA */
	mikeos_print_space();
	mikeos_print_2hex(0xbbbb);	/* BB */
	mikeos_print_newline();

	/* 4hex */
	mikeos_print_string(str2);
	mikeos_print_space();
	mikeos_print_4hex(0x1111);	/* 1111 */
	mikeos_print_space();
	mikeos_print_4hex(0x2222);	/* 2222 */
	mikeos_print_space();
	mikeos_print_4hex(0xaaaa);	/* AAAA */
	mikeos_print_space();
	mikeos_print_4hex(0xbbbb);	/* BBBB */
	mikeos_print_newline();

	return;
}

/* test os_get_api_version */
static	void	test_os_get_api_version(void)
{
	static	char	str0[] = "mikeos_get_api_version";

	mikeos_print_string(str0);
	mikeos_print_space();

	/* MIKEOS_API_VER defined at source/os_main.asm */
	mikeos_print_4hex(mikeos_get_api_version());
	mikeos_print_newline();

	return;
}

/* test os_dump_registers */
static	void	test_os_dump_registers(void)
{
	static	char	str0[] = "mikeos_dump_registers";

	mikeos_print_string(str0);
	mikeos_print_newline();

#asm
	! use in-line assembler to set register
	mov	ax, #0x1234
	mov	bx, #0x5678
	mov	cx, #0x9abc
	mov	dx, #0xdef0
#endasm

	mikeos_dump_registers();
	mikeos_print_newline();

	return;
}

/* test os_dump_string */
static	void	test_os_dump_string(void)
{
        /* mikeos_dump_string() requires the pointer to static region */

	static	char	str0[] = "mikeos_dump_string";

	mikeos_print_string(str0);
	mikeos_print_newline();

	mikeos_dump_string(str0);
	mikeos_print_newline();

	return;
}

/* test os_wait_for_key */
static	void	test_os_wait_for_key(void)
{
	static	char	str0[] = "mikeos_wait_for_key";
	static	char	str1[] = "Press [Enter] to continue...";

	mikeos_print_string(str0);
	mikeos_print_newline();

	mikeos_print_string(str1);
	mikeos_print_newline();

	/* scan code(upper 8bit) is igonred */
	while ((mikeos_wait_for_key() & 0x00ff) != 0x0d);

	return;
}

/* test os_clear_screen */
static	void	test_os_clear_screen(void)
{
	static	char	str0[] = "mikeos_clear_screen";

	/* clear screen first! */
	mikeos_clear_screen();

	mikeos_print_string(str0);
	mikeos_print_newline();

	return;
}

/* test os_get_file_list */
static	void	test_os_get_file_list(void)
{
	/* mikeos_get_file_list requires the pointer to static region */

	static	char	str0[] = "mikeos_get_file_list";

	mikeos_print_string(str0);
	mikeos_print_newline();

	fill_buffer(Buf, sizeof(Buf));
	mikeos_get_file_list(Buf);

	mikeos_print_string(Buf);
	mikeos_print_newline();

	return;
}

/* test os_bcd_to_int */
static	void	test_os_bcd_to_int(void)
{
	int	i;
	static	struct _target {
		unsigned char	value;
		int		result;
	} target[] = {
		0x01,	 1,
		0x12,	12,
		0x23,	23,
		0x34,	34,
		0x45,	45,
		0x56,	56,
		0x67,	67,
		0x78,	78,
		0x89,	89,
		0x90,	90,
	};
	static	char	str0[] = "mikeos_bcd_to_int";

	mikeos_print_string(str0);
	mikeos_print_space();

	for (i = 0; i < sizeof(target) / sizeof(struct _target); i++) {
		mikeos_print_string((target[i].result == 
				     mikeos_bcd_to_int(target[i].value)) ?
				    StringOK : StringNG);
		mikeos_print_space();
	}

	mikeos_print_newline();

	return;
}

/* test os_check_for_key */
static	void	test_os_check_for_key(void)
{
	int	i;
	static	struct _target {
		char	*message;
		int	character;
	} target[] = {
		{"Input 'A' to continue...", 'A'},
		{"Input '1' to continue...", '1'},
		{"Input 'b' to continue...", 'b'},
		{"Input '#' to continue...", '#'},
	};
	static	char	str0[] = "mikeos_check_for_key";

	mikeos_print_string(str0);
	mikeos_print_newline();

	for (i = 0; i < sizeof(target) / sizeof(struct _target); i++) {
		mikeos_print_string(target[i].message);
		mikeos_print_newline();
		while (mikeos_check_for_key() != target[i].character);
	}

	return;
}

/* test os_draw_background */
static	void	test_os_draw_background(void)
{
	/* mikeos_draw_background() requires the pointer to static region */

	static	char	str0[] = "mikeos_draw_background (top)";
	static	char	str1[] = "mikeos_draw_backbround (bottom)";

	/* the colour parameter of mikeos_draw_background is :-
		bit[7:4] as background colour
		bit[3:0] as foreground colour

		IRGB
	   0	0000	black
	   1	0001	blue
	   2	0010	green
	   3	0011	cyan
	   4	0100	red
	   5	0101	purple
	   6	0110	orange
	   7	0111	grey
	   8	1000	dark grey
	   9	1001	light blue
	   A	1010	light green
	   B	1011	light cyan
	   C	1100	light red
	   D	1101	light purple
	   E	1110	yellow
	   F	1111	white
	*/	   

	/* this test changes white(fg)/blue(bg) */
	mikeos_draw_background(str0, str1, 0x001f);

	return;
}

/* test os_dialog_box */
static	void	test_os_dialog_box(void)
{
	static	char	str0[] = "mikeos_dialog_box (test)";
	static	char	str1[] = "press OK to continue...";
	static	char	str2[] = "press Cancel to continue...";

	/* wait for Cancel/2-button dialog */
	while (mikeos_dialog_box(0, str0, str2, 1) == 0);

	/* wait for OK/2-button dialog */
	while (mikeos_dialog_box(str0, str1, 0, 1) == 1);

	/* wait for OK/1-button dialog */
	mikeos_dialog_box(str0, 0, str1, 0);

	return;
}

/* test os_fatal_error */
static	void	test_os_fatal_error(void)
{
        /* mikeos_fatal_error() requires the pointer to static region */

	static	char	str0[] = "mikeos_fatal_error (test)";

	/* program will abort, do not return */
	mikeos_fatal_error(str0);

	return;
}

/* test os_input_dialog */
static	void	test_os_input_dialog(void)
{
	/* mikeos_input_dialog requires the pointer to static region */

	static	char	str0[] = "mikeos_list_dialog";
	static	char	str1[] = "Input any string, and press [Enter]";

	mikeos_clear_screen();
	mikeos_print_string(str0);
	mikeos_print_newline();

	fill_buffer(Buf, sizeof(Buf));
	mikeos_input_dialog(Buf, str1);

	mikeos_print_newline();		/* get out from dialog box */
	mikeos_print_newline();
	mikeos_print_newline();
	mikeos_print_string(Buf);
	mikeos_print_newline();

	test_os_wait_for_key();

	return;
}

/* test os_list_dialog */
static	void	test_os_list_dialog(void)
{
	static	char	str0[] = "mikeos_list_dialog";
	static	char	str1[] = "select target, and press [Enter]";
	static	char	str2[] = ",,,Here!";
	static	char	str3[] = "Here!,,,";
	static	char	str4[] = ",Here!,,";
	static	char	str5[] = "press [Esc] to continue";

	mikeos_clear_screen();

	while (mikeos_list_dialog(str2, str0, str1) != 4);
	while (mikeos_list_dialog(str3, str0, str1) != 1);
	while (mikeos_list_dialog(str4, str0, str1) != 2);
	while (mikeos_list_dialog(str2, str0, str5) >= 0);

	return;
}

/* test os_get_random */
static	void	test_os_get_random()
{
	static	char	str0[] = "mikeos_get_random";
	int	i;

	mikeos_clear_screen();
	mikeos_print_string(str0);
	mikeos_print_newline();

	for (i = 0; i < 16; i++) {
		mikeos_print_4hex(mikeos_get_random(0x0000, 0xfffe));
		mikeos_print_space();
	}

	mikeos_print_newline();

	test_os_wait_for_key();

	return;
}

/* test os_draw_block */
static	void	test_os_draw_block()
{
	static	char	str0[] = "mikeos_draw_block";
	int	i;

	mikeos_clear_screen();
	mikeos_print_string(str0);
	mikeos_print_newline();


	mikeos_draw_block(0, 4, 8, 4, 0x47); // (0, 4) - (7, 3) red
	mikeos_draw_block(4, 6, 8, 4, 0x27); // (4, 6) - (11, 9) green
	mikeos_draw_block(8, 8, 8, 4, 0x17); // (8, 8) - (15, 11) blue

	mikeos_move_cursor(0, 16);
	test_os_wait_for_key();

	return;
}

int	MikeMain(void *arg)
{
	test_os_print_Nhex();
	test_os_get_api_version();
	test_os_dump_registers();
	test_os_dump_string();
	test_os_wait_for_key();

	test_os_clear_screen();
	test_os_get_file_list();
	test_os_wait_for_key();

	test_os_clear_screen();
	test_os_bcd_to_int();
	test_os_check_for_key();
	test_os_wait_for_key();

	test_os_draw_background();
	test_os_dialog_box();

	test_os_input_dialog();
	test_os_list_dialog();

	test_os_get_random();
	test_os_draw_block();

	test_os_fatal_error();

	return 0;
}

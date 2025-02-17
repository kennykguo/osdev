/* test06.c - move cursor */

#include <mikeos.h>

/*
	used MikeOS API :-

	mikeos_get_cursor_pos()
	mikeos_move_cursor()
	mikeos_hide_cursor()
	mikeos_show_cursor()

	mikeos_wait_for_key()
	mikeos_clear_screen()
	mikeos_draw_background()
	mikeos_print_1hex()
	mikeos_print_2hex()
	mikeos_print_space()
*/

/* region: (0,1) - (79,23) */
#define	LEFT_LIMIT	0x00
#define	RIGHT_LIMIT	0x4f
#define	UP_LIMIT	0x01
#define	DOWN_LIMIT	0x17

/* index (to switch static/stack region) */
static	char	Index = 0;

/* test os_move_cursor and os_get_cursor_pos */
int	test_os_move_cursor(void)
{
	unsigned int	key, scan, ascii;
	static	char	x_static, y_static;
	char		x_stack,  y_stack;
	char		x, y;

	key = mikeos_wait_for_key();
	ascii = key & 0x00ff;
	scan  = key & 0xff00;

	/* mikeos_get_cursor() accepts both pointer to 
	   static and stack region */
	if (Index++ & 0x01) {
		mikeos_get_cursor_pos(&x_static, &y_static);
		x = x_static;
		y = y_static;
	} else {
		mikeos_get_cursor_pos(&x_stack,  &y_stack);
		x = x_stack;
		y = y_stack;
	}

	/* determine next cursor position */
	switch (scan) {
	case	0x4800:	/* up */
		if (y > UP_LIMIT) y--;
		break;
	case	0x5000:	/* down */
		if (y < DOWN_LIMIT) y++;
		break;
	case	0x4b00:	/* left */
		if (x > LEFT_LIMIT) x--;
		break;
	case	0x4d00:	/* right */
		if (x < RIGHT_LIMIT) x++;
		break;
	default:	/* do nothing */
		break;
	}

	/* show current status, index, X and Y */
	mikeos_hide_cursor();
	mikeos_move_cursor(RIGHT_LIMIT - 8, DOWN_LIMIT + 1);
	mikeos_print_1hex(Index & 0x01);
	mikeos_print_space();
	mikeos_print_2hex(x);
	mikeos_print_space();
	mikeos_print_2hex(y);

	/* move cursor at new position */
	mikeos_move_cursor(x, y);
	mikeos_show_cursor();

	/* check [Esc] is not pressed */
	return ascii != 0x1b;
}

int	MikeMain(void *arg)
{
	static	char	str0[] = "mikeos_move_cursor()/mikeos_get_cursor_pos() test";
	static	char	str1[] = "[Up] [Down] [Left] [Right] to move cursor, [Esc] to quit";

	/* this test changes white(fg)/blue(bg) */
	mikeos_draw_background(str0, str1, 0x001f);
	mikeos_show_cursor();

	/* cursor position at start */
	mikeos_move_cursor(LEFT_LIMIT, UP_LIMIT);

	/* do test */
	while (test_os_move_cursor());

	/* restore CLI default state */
	mikeos_draw_background(str0, str1, 0x0007);
	mikeos_clear_screen();

	return 0;
}

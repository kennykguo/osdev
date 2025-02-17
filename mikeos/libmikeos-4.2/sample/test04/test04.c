/* test04.c - string input, show/hide cursor */

#include <mikeos.h>

/*
	used MikeOS API :-

	mikeos_show_cursor()
	mikeos_hide_cursor();
	mikeos_input_string()

	mikeos_print_newline()
	mikeos_print_string()
*/


/* fill with garbage to check zero-terminated string */
static	void	fill_buffer(char *buf, int size)
{
	int	i;

	for (i = 0; i < size; i++) buf[i] = 'X';
	return;
}

int	MikeMain(void *arg)
{
	int	i;

	/* mikeos_input_string() and mikeos_print_string()
	   requires the pointer to static region */

	static	char	buf[256];
	static	char	str0[] = "Type string, and hit enter (cursor is off)";
	static	char	str1[] = "Type string, and hit enter (cursor is on)";

	fill_buffer(buf, sizeof(buf));
	mikeos_print_string(str0);
	mikeos_print_newline();
	mikeos_hide_cursor();
	mikeos_input_string(buf);
	mikeos_print_newline();
	mikeos_print_string(buf);
	mikeos_print_newline();

	fill_buffer(buf, sizeof(buf));
	mikeos_print_string(str1);
	mikeos_print_newline();
	mikeos_show_cursor();
	mikeos_input_string(buf);
	mikeos_print_newline();
	mikeos_print_string(buf);
	mikeos_print_newline();

	return 0;
}

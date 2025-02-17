/* test02.c - get date/time string */

#include <mikeos.h>

/*
	used MikeOS API :-

	mikeos_set_date_fmt()
	mikeos_get_date_string()

	mikeos_set_time_fmt()
	mikeos_get_time_string()
	
	mikeos_print_horiz_line()
	mikeos_print_string()
	mikeos_print_newline()
	mikeos_print_4hex()
	mikeos_print_space()
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

	/* mikeos_get_date_string(), mikeos_get_time_string(), and
	   mikeos_print_string() requires the pointer to static region */

	static	char	buf[256];
	static	unsigned int	format[] = {
#if 0
		0x0000,	/* (not recommended) same as 0x0080 */
		0x0001,	/* (not recommended) same as 0x0081 */
		0x0002,	/* (not recommended) same as 0x0082 */
#endif
		0x0080,	/* format #0 without separator, "Sept 15, 2008" */
		0x0081,	/* format #1 without separator, "15 Sept, 2008" */
		0x0082,	/* format #2 without separator, "2008/09/15" */
		0x2d00,	/* format #0 with separator, "9-15-2000" */
		0x2d01,	/* format #1 with separator, "15-9-2000" */
		0x2d02,	/* format #2 with separator, "2008/09/15" */
#if 0
		0x2d80,	/* (not recommended) same as 0x0080 */
		0x2d81,	/* (not recommended) same as 0x0081 */
		0x2d82,	/* (not recommended) same as 0x0082 */
#endif
	};

	/* date */
	for (i = 0; i < sizeof(format) / sizeof(unsigned int); i++) {
		mikeos_print_4hex(format[i]);
		mikeos_print_space();

		fill_buffer(buf, sizeof(buf));
		mikeos_set_date_fmt(format[i]);
		mikeos_get_date_string(buf);
		mikeos_print_string(buf);
		mikeos_print_newline();
	}

	mikeos_print_horiz_line(0);	/* horiz line "--------" */

	/* time */
	fill_buffer(buf, sizeof(buf));
	mikeos_set_time_fmt(0);		/* format #0, "2:04 PM" */
	mikeos_get_time_string(buf);
	mikeos_print_string(buf);
	mikeos_print_newline();

	fill_buffer(buf, sizeof(buf));
	mikeos_set_time_fmt(1);		/* format #1, "1404 hours" */
	mikeos_get_time_string(buf);
	mikeos_print_string(buf);
	mikeos_print_newline();

	mikeos_print_horiz_line(1);	/* horiz line "========" */

	return 0;
}

/* test0d.c - C argument */

#include <mikeos.h>

/*
	used MikeOS API :-

	mikeos_print_4hex()
	mikeos_print_newline()
	mikeos_print_space()
	mikeos_print_string()
*/

int	MikeMain(char *argument)
{
	int	i;

	mikeos_print_4hex(argument);
	if (argument != NULL) {
		mikeos_print_space();
		mikeos_print_string(argument);
	}
	mikeos_print_newline();

	return 0;
}

/* test00.c - well-known "Hello, World!" */

#include <mikeos.h>

/*
	used MikeOS API :-

	mikeos_print_string()
	mikeos_print_newline()
*/

/*
	Standard C uses main() as entry point,
	but libmikeos uses this function :-

	int MikeMain(void *argument)

	Currently the value of argument is NULL,
	and the return value of MikeMain() must be zero.
*/

int	MikeMain(void *argument)
{
	static	char	message[] = "Hello, MikeOS!";

	/* mikeos_print_string() requires the pointer to static region */

	mikeos_print_string(message);
	mikeos_print_newline();

	return 0;
}

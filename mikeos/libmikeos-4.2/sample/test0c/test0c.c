/* test0c.c - launch EXAMPLE.BAS from C */

#include <mikeos.h>

/*
	used MikeOS API :-

	mikeos_get_file_size
	mikeos_load_file
	mikeos_run_basic
*/

/* filename and buffer must be on the static region */
static	char	FileName[] = "EXAMPLE.BAS";
static	char	Buf[8192];

/* test os_run_basic */
static	void	test_run_basic(void)
{
	int	size;
	static	char	str0[] = "mikeos_run_basic OK";
	static	char	str1[] = "mikeos_run_basic NG";

	/* get file size for os_run_basic */
	size = mikeos_get_file_size(FileName);
	if (size < 0 || size > sizeof(Buf)) goto error;

	/* load BASIC file into buffer */
	if (mikeos_load_file(Buf, FileName) != size) goto error;

	/* run it */
	mikeos_run_basic(Buf, size);

success:
	mikeos_print_string(str0);
	mikeos_print_newline();
	return;

error:
	mikeos_print_string(str1);
	mikeos_print_newline();
	return;
}	

int	MikeMain(void *arg)
{
	test_run_basic();

	return 0;
}

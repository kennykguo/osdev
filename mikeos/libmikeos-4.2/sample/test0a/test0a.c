/* test0a.c - file operation (2) rename file */

#include <mikeos.h>

/*
	used MikeOS API :-

	mikeos_file_exists()
	mikeos_rename_file()
*/


/*
	filename will be :-

	test0a.bin --> poco.bin --> paco.bin
	                   ^            |
	                   |            V
	               pico.bin <-- peco.bin

	and, one of above file must be existed.
*/

#define	FILE_NAMES	5

static	char	*FileName[FILE_NAMES] = {
	"PICO.BIN",
	"POCO.BIN",
	"PACO.BIN",
	"PECO.BIN",
	"TEST0A.BIN",
};

int	find_filename(void)
{
	int	i;

	for (i = 0; i < FILE_NAMES; i++) {
		if (mikeos_file_exists(FileName[i]) >= 0) return i;
	}

	return -1;
}

int	MikeMain(void *arg)
{
	static	char	str0[] = "something goes wrong...";
	int	ix;

	if ((ix = find_filename()) < 0 ||
	    mikeos_rename_file(FileName[(ix + 1) % (FILE_NAMES - 1)],
			       FileName[ix]) < 0) {
		mikeos_print_string(str0);
		mikeos_print_newline();
	}

 fin0:
	return 0;
}

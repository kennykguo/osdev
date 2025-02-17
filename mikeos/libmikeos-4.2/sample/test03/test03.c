/* test03.c - test string operations */

#include <mikeos.h>

/*
	used MikeOS API :-

	mikeos_find_char_in_string()
	mikeos_int_to_string()
	mikeos_long_int_to_string()
	mikeos_sint_to_string()
	mikeos_string_chomp()
	mikeos_string_compare()
	mikeos_string_copy()
	mikeos_string_join()
	mikeos_string_lowercase()
	mikeos_string_reverse()
	mikeos_string_strincmp()
	mikeos_string_strip()
	mikeos_string_tokinize()
	mikeos_string_truncate()
	mikeos_string_uppercase()

	mikeos_print_string()
	mikeos_print_newline()
	mikeos_print_space()
*/

static	char	StringOK[] = "OK";
static	char	StringNG[] = "NG";
static	char	Buf[256];

/* fill with garbage to check zero-terminated string */
static	void	fill_buffer(char *buf)
{
	int	i;

	for (i = 0; i < sizeof(buf); i++) buf[i] = 'X';
	return;
}

/* test os_string_compare */
static	void	test_string_compare(void)
{
        /* mikeos_string_compare() requires the pointer to static region */

	static	char	str0[] = "mikeos_string_compare";
	static	char	str1[] = "mikeos_string_compare";
	static	char	str2[] = "mikeos_string_compare?";

	mikeos_print_string(str0);
	mikeos_print_space();

	/* same location, same string */
	mikeos_print_string(mikeos_string_compare(str0, str0) ?
			    StringOK : StringNG);
	mikeos_print_space();

	/* different location, same string */
	mikeos_print_string(mikeos_string_compare(str0, str1) ?
			    StringOK : StringNG);
	mikeos_print_space();

	/* different location, different string (1) */
	mikeos_print_string(mikeos_string_compare(str0, str2) ?
			    StringNG : StringOK);
	mikeos_print_space();

	/* different location, different string (2) */
	mikeos_print_string(mikeos_string_compare(str2, str0) ?
			    StringNG : StringOK);
	mikeos_print_newline();

	return;
}

/* test os_string_copy */
static	void	test_string_copy(void)
{
	/* mikeos_string_copy() requires the pointer to static region */

	static	char	str0[] = "mikeos_string_copy";

	mikeos_print_string(str0);
	mikeos_print_space();

	/* do copy */
	fill_buffer(Buf);
	mikeos_string_copy(Buf, str0);

	/* check result */
	mikeos_print_string(mikeos_string_compare(Buf, str0) ?
			    StringOK : StringNG);
	mikeos_print_newline();

	return;
}

/* test os_string_join */
static	void	test_string_join(void)
{
        /* mikeos_string_join() requires the pointer to static region */

	static	char	str0[] = "mikeos_string_join";
	static	char	str1[] = "mikeos_st";
	static	char	str2[] = "ring_join";

	mikeos_print_string(str0);
	mikeos_print_space();

	/* do join */
	fill_buffer(Buf);
	mikeos_string_join(Buf, str1, str2);

	/* check result */
	mikeos_print_string(mikeos_string_compare(Buf, str0) ?
			    StringOK : StringNG);
	mikeos_print_newline();

	return;
}

/* test os_string_chomp */
static	void	test_string_chomp(void)
{
        /* mikeos_string_chomp() requires the pointer to static region */

	static	char	str0[] = "mikeos_string_chomp";
	static	char	str1[] = "        mikeos_string_chomp        ";

	mikeos_print_string(str0);
	mikeos_print_space();

	/* do chomp */
	mikeos_string_chomp(str1);

	/* check result */
	mikeos_print_string(mikeos_string_compare(str0, str1) ?
			    StringOK : StringNG);
	mikeos_print_newline();

	return;
}

/* test os_string_length */
static	void	test_string_length(void)
{
        /* mikeos_string_length() requires the pointer to static region */

	static	char	str0[] = "mikeos_string_length";
	static	char	str1[] = "";

	mikeos_print_string(str0);
	mikeos_print_space();

	/* string */
	mikeos_print_string((mikeos_string_length(str0) == sizeof(str0) - 1) ?
			    StringOK : StringNG);
	mikeos_print_space();

	/* no character */
	mikeos_print_string((mikeos_string_length(str1) == 0) ?
			    StringOK : StringNG);
	mikeos_print_newline();

	return;
}

/* test os_string_lowercase */
static	void	test_string_lowercase(void)
{
	/* mikeos_string_lowercase() requires the pointer to static region */

	static	char	str0[] = "mikeos_string_lowercase";
	static	char	str1[] = "MIKEOS_STRING_LOWERCASE";

	mikeos_print_string(str0);
	mikeos_print_space();

	/* do convert */
	mikeos_string_lowercase(str1);

	/* check result */
	mikeos_print_string(mikeos_string_compare(str0, str1) ?
			    StringOK : StringNG);
	mikeos_print_newline();

	return;
}

/* test os_string_uppercase */
static	void	test_string_uppercase(void)
{
	/* mikeos_string_uppercase() requires the pointer to static region */

	static	char	str0[] = "mikeos_string_uppercase";
	static	char	str1[] = "MIKEOS_STRING_UPPERCASE";

	mikeos_print_string(str0);
	mikeos_print_space();

	/* do convert */
	mikeos_string_uppercase(str0);

	/* check result */
	mikeos_print_string(mikeos_string_compare(str0, str1) ?
			    StringOK : StringNG);
	mikeos_print_newline();

	return;
}

/* test os_string_strip */
static	void	test_string_strip(void)
{
	/* mikeos_string_strip() requires the pointer to static region */

	static	char	str0[] = "mikeos_string_strip";
	static	char	str1[] = "@m@i@k@e@o@s@_@s@t@r@i@n@g@_@s@t@r@i@p@";

	mikeos_print_string(str0);
	mikeos_print_space();

	/* do strip */
	mikeos_string_strip(str1, '@');

	/* check result */
	mikeos_print_string(mikeos_string_compare(str0, str1) ?
			    StringOK : StringNG);
	mikeos_print_newline();

	return;
}

/* test_os_string_truncate */
static	void	test_string_truncate(void)
{
	/* mikeos_string_truncate() requires the pointer to static region */

	static	char	str0[] = "mikeos_string_truncate";
	static	char	str1[] = "mikeos_string_truncate@@@@@@@@";

	mikeos_print_string(str0);
	mikeos_print_space();

	/* do truncate */
	mikeos_string_truncate(str1, sizeof(str0) - 1);

	/* check result */
	mikeos_print_string(mikeos_string_compare(str0, str1) ?
			    StringOK : StringNG);
	mikeos_print_newline();

	return;
}

/* test os_find_char_in_string(void) */
static	void	test_find_char_in_string(void)
{
	/* mikeos_find_char_in_string() requires
	   the pointer to static region */

	static	char	str0[] = "mikeos_string_find_char_in_string";
	static	char	str1[] = "mikeos_string_find_char_in_string@#$";

	mikeos_print_string(str0);
	mikeos_print_space();

	/* target not found */
	mikeos_print_string((mikeos_find_char_in_string(str1, '!') == 0) ?
			    StringOK : StringNG);
	mikeos_print_space();

	/* do truncate */
	mikeos_string_truncate(str1,
			       mikeos_find_char_in_string(str1, '@') - 1);

	/* check result */
	mikeos_print_string(mikeos_string_compare(str0, str1) ?
			    StringOK : StringNG);
	mikeos_print_newline();

	return;
}

/* test os_int_to_string */
static	void	test_int_to_string(void)
{
	/* mikeos_int_to_string() requires the pointer to static region */

	int	i;
	static	struct _target {
		char		*str;
		unsigned int	value;
	} target[] = {
		{    "0",     0},
		{    "1",     1},
		{   "10",    10},
		{  "100",   100},
		{ "1000",  1000},
		{"10000", 10000},
		{"65535", 65535},
	};
	static	char	str0[] = "mikeos_int_to_string";

	mikeos_print_string(str0);
	mikeos_print_space();

	for (i = 0; i < sizeof(target) / sizeof(struct _target); i++) {
		/* do convert */
		fill_buffer(Buf);
		mikeos_int_to_string(Buf, target[i].value);

		/* check result */
		mikeos_print_string(mikeos_string_compare(Buf, target[i].str) ?
				    StringOK : StringNG);
		mikeos_print_space();
	}

	mikeos_print_newline();

	return;
}

/* test os_long_int_to_string */
static	void	test_long_int_to_string(void)
{
	/* mikeos_long_int_to_string() requires the pointer to static region */

	int	i;
	static	struct _target {
		char		*str;
		unsigned long	value;
		unsigned int	base;
	} target[] = {
		/* there is too many cases, check hex/dec only */
		{       "0", 0x00000000UL, 16},
		{       "1", 0x00000001UL, 16},
		{      "10", 0x00000010UL, 16},
		{     "100", 0x00000100UL, 16},
		{    "1000", 0x00001000UL, 16},
		{   "10000", 0x00010000UL, 16},
		{  "100000", 0x00100000UL, 16},
		{ "1000000", 0x01000000UL, 16},
		{"10000000", 0x10000000UL, 16},
		{"FFFFFFFF", 0xffffffffUL, 16},
		{         "0",          0UL, 10},
		{         "1",          1UL, 10},
		{        "10",         10UL, 10},
		{       "100",        100UL, 10},
		{      "1000",       1000UL, 10},
		{     "10000",      10000UL, 10},
		{    "100000",     100000UL, 10},
		{   "1000000",    1000000UL, 10},
		{  "10000000",   10000000UL, 10},
		{ "100000000",  100000000UL, 10},
		{"1000000000", 1000000000UL, 10},
		{"4294967295", 0xffffffffUL, 10},
	};

	static	char	str0[] = "mikeos_long_int_to_string";

	mikeos_print_string(str0);
	mikeos_print_space();

	for (i = 0; i < sizeof(target) / sizeof(struct _target); i++) {
		/* do convert */
		fill_buffer(Buf);
		mikeos_long_int_to_string(Buf,
					  target[i].value, target[i].base);

		/* check result */
		mikeos_print_string(mikeos_string_compare(Buf, target[i].str) ?
				    StringOK : StringNG);
		mikeos_print_space();
	}

	mikeos_print_newline();

	return;
}

/* test os_string_strincmp */
static	void	test_string_strincmp(void)
{
        /* mikeos_string_strincmp() requires the pointer to static region */

	static	char	str0[] = "mikeos_string_strincmp";
	static	char	str1[] = "mikeos_string_strincmp";
	static	char	str2[] = "mikeos_string_strincmp?";

	mikeos_print_string(str0);
	mikeos_print_space();

	/* same location, same string */
	mikeos_print_string(mikeos_string_strincmp(str0, str0,
						   sizeof(str0) - 1) ?
			    StringOK : StringNG);
	mikeos_print_space();

	/* different location, same string */
	mikeos_print_string(mikeos_string_strincmp(str0, str1,
						   sizeof(str0) - 1) ?
			    StringOK : StringNG);
	mikeos_print_space();

	/* different location, different string (1) */
	mikeos_print_string(mikeos_string_strincmp(str0, str2,
						   sizeof(str2) - 1) ?
			    StringNG : StringOK);
	mikeos_print_space();

	/* different location, different string (2) */
	mikeos_print_string(mikeos_string_strincmp(str2, str0,
						   sizeof(str2) - 1) ?
			    StringNG : StringOK);
	mikeos_print_space();

	/* different location, different string, partially matched (1) */
	mikeos_print_string(mikeos_string_strincmp(str0, str2,
						   sizeof(str0) - 1) ?
			    StringOK : StringNG);
	mikeos_print_space();

	/* different location, different string, partially matched (2) */
	mikeos_print_string(mikeos_string_strincmp(str2, str0,
						   sizeof(str0) - 1) ?
			    StringOK : StringNG);
	mikeos_print_newline();

	return;
}

/* test os_string_charchange */
static	void	test_string_charchange(void)
{
	/* mikeos_string_charchange() requires
	   the pointer to static region */

	static	char	str0[] = "mikeos_string_charchange";
	static	char	str1[] = "mIkeoS#StrIng#charchange";

	mikeos_print_string(str0);
	mikeos_print_space();

	/* do replace */
	mikeos_string_charchange(str0, 'I', 'i');
	mikeos_string_charchange(str0, 'S', 's');
	mikeos_string_charchange(str0, '#', '_');

	/* check result */
	mikeos_print_string(mikeos_string_compare(str0, str1) ?
			    StringOK : StringNG);
	mikeos_print_newline();

	return;
}

/* test os_string_reverse */
static	void	test_string_reverse(void)
{
	/* mikeos_string_reverse() requires
	   the pointer to static region */
	static	char	str0[] = "mikeos_string_reverse";
	static	char	str1[] = "esrever_gnirts_soekim";

	mikeos_print_string(str0);
	mikeos_print_space();

	/* do reverse */
	mikeos_string_reverse(str1);

	/* check result */
	mikeos_print_string(mikeos_string_compare(str0, str1) ?
			    StringOK : StringNG);
	mikeos_print_newline();

	return;
}


/* test os_sint_to_string */
static	void	test_sint_to_string(void)
{
	/* mikeos_sint_to_string() requires the pointer to static region */

	int	i;
	static	struct _target {
		char	*str;
		int	value;
	} target[] = {
		{     "0",       0},
		{     "1",       1},
		{    "10",      10},
		{   "100",     100},
		{  "1000",    1000},
		{ "10000",   10000},
		{ "32767",   32767},
		{    "-1",      -1},
		{   "-10",     -10},
		{  "-100",    -100},
		{ "-1000",   -1000},
		{"-10000",  -10000},
		{"-32768",  -32768},
	};
	static	char	str0[] = "mikeos_sint_to_string";

	mikeos_print_string(str0);
	mikeos_print_space();

	for (i = 0; i < sizeof(target) / sizeof(struct _target); i++) {
		/* do convert */
		fill_buffer(Buf);
		mikeos_sint_to_string(Buf, target[i].value);

		/* check result */
		mikeos_print_string(mikeos_string_compare(Buf, target[i].str) ?
				    StringOK : StringNG);
		mikeos_print_space();
	}

	mikeos_print_newline();

	return;
}

/* test os_string_parse */
static	void	test_string_parse(void)
{
	/* mikeos_string_parse(),
	   the string pointer points static region */
	static	char	str0[] = "mikeos_string_parse ";
	static	char	str1[] = "hoge fuga piyo hogera ";
	static	char	stra[] = "hoge";
	static	char	strb[] = "fuga";
	static	char	strc[] = "piyo";
	static	char	strd[] = "hogera ";
	char	*a, *b, *c, *d;

	mikeos_print_string(str0);

	/* do parse */
	mikeos_string_parse(str1, &a, &b, &c, &d);

	mikeos_print_string(mikeos_string_compare(stra, a) ?
			    StringOK : StringNG);
	mikeos_print_space();

	mikeos_print_string(mikeos_string_compare(strb, b) ?
			    StringOK : StringNG);
	mikeos_print_space();

	mikeos_print_string(mikeos_string_compare(strc, c) ?
			    StringOK : StringNG);
	mikeos_print_space();

	mikeos_print_string(mikeos_string_compare(strd, d) ?
			    StringOK : StringNG);
	mikeos_print_space();

	mikeos_print_newline();

	return;
}

/* test os_string_tokenize */
static	void	test_string_tokenize(void)
{
	/* mikeos_string_tokenize(),
	   the string pointer points static region */
	static	char	str0[] = "mikeos_string_tokenize ";
	static	char	stra[] = "mikeos";
	static	char	strb[] = "string";
	static	char	strc[] = "tokenize ";
	char	*n, *t;

	mikeos_print_string(str0);

	n = mikeos_string_tokenize(t = str0, '_');
	mikeos_print_string(mikeos_string_compare(t, stra) ?
			    StringOK : StringNG);
	mikeos_print_space();

	n = mikeos_string_tokenize(t = n, '_');
	mikeos_print_string(mikeos_string_compare(t, strb) ?
			    StringOK : StringNG);
	mikeos_print_space();

	n = mikeos_string_tokenize(t = n, '_');
	mikeos_print_string(mikeos_string_compare(t, strc) ?
			    StringOK : StringNG);
	mikeos_print_space();

	mikeos_print_string(n == NULL ? StringOK : StringNG);
	mikeos_print_newline();

	return;
}

int	MikeMain(void *arg)
{
	test_string_compare();
	test_string_copy();
	test_string_join();
	test_string_chomp();
	test_string_length();
	test_string_lowercase();
	test_string_uppercase();
	test_string_strip();
	test_string_truncate();
	test_find_char_in_string();
	test_int_to_string();
	test_long_int_to_string();
	test_string_strincmp();
	test_string_charchange();
	test_string_reverse();
	test_sint_to_string();
	test_string_parse();
	test_string_tokenize();

	return 0;
}

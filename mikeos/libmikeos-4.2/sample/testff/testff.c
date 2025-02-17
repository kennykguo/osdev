/* testff.c - long int operation (bcc stuff) */

#define	TEST_BCC_MIKEOS

#if defined(TEST_BCC_MIKEOS)
#include <mikeos.h>

#else
#include <stdio.h>

#define	mikeos_print_string(x)		printf("%s", (x))
#define	mikeos_print_4hex(x)		printf("%04x", (x))
#define	mikeos_long_int_negate(x)	(-(x))
#define	mikeos_wait_for_key		getchar
static	void	mikeos_print_space(void) {printf(" ");}	
static	void	mikeos_print_newline(void) {printf("\n");}
#endif

static	char	StringOK[] = "OK ";
static	char	StringNG[] = "NG ";
static	int	NGCount = 0;

#define	OK	{mikeos_print_string(StringOK);}
#define	NG	{mikeos_print_string(StringNG); NGCount++;}
#define	P(x)	{mikeos_print_4hex(&(x));mikeos_print_space();}

/* AMD64 must be int, not long */
#if 1
typedef	long		X;
typedef	unsigned long	UX;
#else
typedef	int		X;
typedef	unsigned int	UX;
#endif

/* fundamental operations */
#define	SET1(var1, num1)		{(var1) = (num1);}	
#define	SET2(var1, num1, var2, num2)	{(var1) = (num1); (var2) = (num2);}

#define	ADD(var1, var2)			{(var1) += (var2);} 
#define	SUB(var1, var2)			{(var1) -= (var2);}
#define	MUL(var1, var2)			{(var1) *= (var2);}
#define	DIV(var1, var2)			{(var1) /= (var2);}
#define	MOD(var1, var2)			{(var1) %= (var2);}

#define	AND(var1, var2)			{(var1) &= (var2);}
#define	OR(var1, var2)			{(var1) |= (var2);}
#define	XOR(var1, var2)			{(var1) ^= (var2);}
#define	NOT(var1)			{(var1) = ~(var1);}

#define	SHL(var1, var2)			{(var1) <<= (var2);}
#define	SHR(var1, var2)			{(var1) >>= (var2);}

#define	EQ(var1, var2)			((var1) == (var2))
#define	LT(var1, var2)			((var1) <  (var2))
#define	GT(var1, var2)			((var1) >  (var2))
#define	LEQ(var1, var2)			((var1) <= (var2))
#define	GEQ(var1, var2)			((var1) >= (var2))

static	void	check_eq(void)
{
	X		x0a, x1a;
	UX		ux0a, ux1a;
	static	X	x0s, x1s;
	static	UX	ux0s, ux1s;
	static	char	str[] = "check_eq ";

	mikeos_print_string(str);
	P(x0a); P(x1a); P(ux0a); P(ux1a);
	P(x0s); P(x1s); P(ux0s); P(ux1s);
	mikeos_print_newline();

	/* lcmpl */
	SET2(x0a, (X)0x01234567, x1a, (X)0x01234567);		if (EQ(x0a, x1a)) {OK;} else {NG;}
	SET2(x0a, (X)0x12345678, x1s, (X)0x12345678);		if (EQ(x0a, x1s)) {OK;} else {NG;}
	SET2(x0s, (X)0x23456789, x1a, (X)0x23456789);		if (EQ(x0s, x1a)) {OK;} else {NG;}
	SET2(x0s, (X)0x3456789a, x1s, (X)0x3456789a);		if (EQ(x0s, x1s)) {OK;} else {NG;}

	SET2(x0a, (X)0x01234567, x1a, (X)0x12345678);		if (EQ(x0a, x1a)) {NG;} else {OK;}
	SET2(x0a, (X)0x12345678, x1s, (X)0x23456789);		if (EQ(x0a, x1s)) {NG;} else {OK;}
	SET2(x0s, (X)0x23456789, x1a, (X)0x3456789a);		if (EQ(x0s, x1a)) {NG;} else {OK;}
	SET2(x0s, (X)0x3456789a, x1s, (X)0x01234567);		if (EQ(x0s, x1s)) {NG;} else {OK;}

	/* lcmpul */
	SET2(ux0a, (UX)0x01234567, ux1a, (UX)0x01234567);	if (EQ(ux0a, ux1a)) {OK;} else {NG;}
	SET2(ux0a, (UX)0x12345678, ux1s, (UX)0x12345678);	if (EQ(ux0a, ux1s)) {OK;} else {NG;}
	SET2(ux0s, (UX)0x23456789, ux1a, (UX)0x23456789);	if (EQ(ux0s, ux1a)) {OK;} else {NG;}
	SET2(ux0s, (UX)0x3456789a, ux1s, (UX)0x3456789a);	if (EQ(ux0s, ux1s)) {OK;} else {NG;}

	SET2(ux0a, (UX)0x01234567, ux1a, (UX)0x12345678);	if (EQ(ux0a, ux1a)) {NG;} else {OK;}
	SET2(ux0a, (UX)0x12345678, ux1s, (UX)0x23456789);	if (EQ(ux0a, ux1s)) {NG;} else {OK;}
	SET2(ux0s, (UX)0x23456789, ux1a, (UX)0x3456789a);	if (EQ(ux0s, ux1a)) {NG;} else {OK;}
	SET2(ux0s, (UX)0x3456789a, ux1s, (UX)0x01234567);	if (EQ(ux0s, ux1s)) {NG;} else {OK;}

	mikeos_print_newline();

	return;
}

static	void	check_and(void)
{	
	X		x0a, x1a;
	UX		ux0a, ux1a;
	static	X	x0s, x1s;
	static	UX	ux0s, ux1s;
	static	char	str[] = "check_and ";

	mikeos_print_string(str);
	P(x0a); P(x1a); P(ux0a); P(ux1a);
	P(x0s); P(x1s); P(ux0s); P(ux1s);
	mikeos_print_newline();

	/* landl */
	SET2(x0a, (X)0x01234567, x1a, (X)0x12345678);		AND(x0a, x1a);		if (EQ(x0a, (X)0x00204460)) {OK;} else {NG;}
	SET2(x0a, (X)0x12345678, x1s, (X)0x23456789);		AND(x0a, x1s);		if (EQ(x0a, (X)0x02044608)) {OK;} else {NG;}
	SET2(x0s, (X)0x23456789, x1a, (X)0x3456789a);		AND(x0s, x1a);		if (EQ(x0s, (X)0x20446088)) {OK;} else {NG;}
	SET2(x0s, (X)0x3456789a, x1s, (X)0x01234567);		AND(x0s, x1s);		if (EQ(x0s, (X)0x00024002)) {OK;} else {NG;}

	/* landul */
	SET2(ux0a, (UX)0x01234567, ux1a, (UX)0x12345678);	AND(ux0a, ux1a);	if (EQ(ux0a, (UX)0x00204460)) {OK;} else {NG;}
	SET2(ux0a, (UX)0x12345678, ux1s, (UX)0x23456789);	AND(ux0a, ux1s);	if (EQ(ux0a, (UX)0x02044608)) {OK;} else {NG;}
	SET2(ux0s, (UX)0x23456789, ux1a, (UX)0x3456789a);	AND(ux0s, ux1a);	if (EQ(ux0s, (UX)0x20446088)) {OK;} else {NG;}
	SET2(ux0s, (UX)0x3456789a, ux1s, (UX)0x01234567);	AND(ux0s, ux1s);	if (EQ(ux0s, (UX)0x00024002)) {OK;} else {NG;}

	mikeos_print_newline();

	return;
}

static	void	check_or(void)
{	
	X		x0a, x1a;
	UX		ux0a, ux1a;
	static	X	x0s, x1s;
	static	UX	ux0s, ux1s;
	static	char	str[] = "check_or ";

	mikeos_print_string(str);
	P(x0a); P(x1a); P(ux0a); P(ux1a);
	P(x0s); P(x1s); P(ux0s); P(ux1s);
	mikeos_print_newline();

	/* lorl */
	SET2(x0a, (X)0x01234567, x1a, (X)0x12345678);		OR(x0a, x1a);	if (EQ(x0a, (X)0x1337577f)) {OK;} else {NG;}
	SET2(x0a, (X)0x12345678, x1s, (X)0x23456789);		OR(x0a, x1s);	if (EQ(x0a, (X)0x337577f9)) {OK;} else {NG;}
	SET2(x0s, (X)0x23456789, x1a, (X)0x3456789a);		OR(x0s, x1a);	if (EQ(x0s, (X)0x37577f9b)) {OK;} else {NG;}
	SET2(x0s, (X)0x3456789a, x1s, (X)0x01234567);		OR(x0s, x1s);	if (EQ(x0s, (X)0x35777dff)) {OK;} else {NG;}

	/* lorul */
	SET2(ux0a, (UX)0x01234567, ux1a, (UX)0x12345678);	OR(ux0a, ux1a);	if (EQ(ux0a, (UX)0x1337577f)) {OK;} else {NG;}
	SET2(ux0a, (UX)0x12345678, ux1s, (UX)0x23456789);	OR(ux0a, ux1s);	if (EQ(ux0a, (UX)0x337577f9)) {OK;} else {NG;}
	SET2(ux0s, (UX)0x23456789, ux1a, (UX)0x3456789a);	OR(ux0s, ux1a);	if (EQ(ux0s, (UX)0x37577f9b)) {OK;} else {NG;}
	SET2(ux0s, (UX)0x3456789a, ux1s, (UX)0x01234567);	OR(ux0s, ux1s);	if (EQ(ux0s, (UX)0x35777dff)) {OK;} else {NG;}

	mikeos_print_newline();

	return;
}

static	void	check_xor(void)
{	
	X		x0a, x1a;
	UX		ux0a, ux1a;
	static	X	x0s, x1s;
	static	UX	ux0s, ux1s;
	static	char	str[] = "check_xor ";

	mikeos_print_string(str);
	P(x0a); P(x1a); P(ux0a); P(ux1a);
	P(x0s); P(x1s); P(ux0s); P(ux1s);
	mikeos_print_newline();

	/* leorl */
	SET2(x0a, (X)0x01234567, x1a, (X)0x12345678);		XOR(x0a, x1a);		if (EQ(x0a, (X)0x1317131f)) {OK;} else {NG;}
	SET2(x0a, (X)0x12345678, x1s, (X)0x23456789);		XOR(x0a, x1s);		if (EQ(x0a, (X)0x317131f1)) {OK;} else {NG;}
	SET2(x0s, (X)0x23456789, x1a, (X)0x3456789a);		XOR(x0s, x1a);		if (EQ(x0s, (X)0x17131f13)) {OK;} else {NG;}
	SET2(x0s, (X)0x3456789a, x1s, (X)0x01234567);		XOR(x0s, x1s);		if (EQ(x0s, (X)0x35753dfd)) {OK;} else {NG;}

	/* leorul */
	SET2(ux0a, (UX)0x01234567, ux1a, (UX)0x12345678);	XOR(ux0a, ux1a);	if (EQ(ux0a, (UX)0x1317131f)) {OK;} else {NG;}
	SET2(ux0a, (UX)0x12345678, ux1s, (UX)0x23456789);	XOR(ux0a, ux1s);	if (EQ(ux0a, (UX)0x317131f1)) {OK;} else {NG;}
	SET2(ux0s, (UX)0x23456789, ux1a, (UX)0x3456789a);	XOR(ux0s, ux1a);	if (EQ(ux0s, (UX)0x17131f13)) {OK;} else {NG;}
	SET2(ux0s, (UX)0x3456789a, ux1s, (UX)0x01234567);	XOR(ux0s, ux1s);	if (EQ(ux0s, (UX)0x35753dfd)) {OK;} else {NG;}

	mikeos_print_newline();

	return;
}

static	void	check_not(void)
{	
	X		x0a;
	UX		ux0a;
	static	X	x0s;
	static	UX	ux0s;
	static	char	str[] = "check_not ";

	mikeos_print_string(str);
	P(x0a); P(ux0a);
	P(x0s); P(ux0s);
	mikeos_print_newline();

	/* lcoml */
	SET1(x0a, (X)0x01234567);	NOT(x0a);	if (EQ(x0a, (X)0xfedcba98)) {OK;} else {NG;}
	SET1(x0s, (X)0x23456789);	NOT(x0s);	if (EQ(x0s, (X)0xdcba9876)) {OK;} else {NG;}

	SET1(ux0a, (UX)0x01234567);	NOT(ux0a);	if (EQ(ux0a, (UX)0xfedcba98)) {OK;} else {NG;}
	SET1(ux0s, (UX)0x23456789);	NOT(ux0s);	if (EQ(ux0s, (UX)0xdcba9876)) {OK;} else {NG;}

	mikeos_print_newline();

	return;
}

static	void	check_shl(void)
{	
	X		x0a, x1a;
	UX		ux0a, ux1a;
	static	X	x0s, x1s;
	static	UX	ux0s, ux1s;
	static	char	str[] = "check_shl ";

	mikeos_print_string(str);
	P(x0a); P(x1a); P(ux0a); P(ux1a);
	P(x0s); P(x1s); P(ux0s); P(ux1s);
	mikeos_print_newline();

	/* lsll */
	SET2(x0a, (X)0x6789abcd, x1a, (X)1);		SHL(x0a, x1a);		if (EQ(x0a, (X)0xcf13579a)) {OK;} else {NG;}
	SET2(x0a, (X)0x789abcde, x1s, (X)2);		SHL(x0a, x1s);		if (EQ(x0a, (X)0xe26af378)) {OK;} else {NG;}
	SET2(x0s, (X)0x89abcdef, x1a, (X)3);		SHL(x0s, x1a);		if (EQ(x0s, (X)0x4d5e6f78)) {OK;} else {NG;}
	SET2(x0s, (X)0x9abcdef0, x1s, (X)4);		SHL(x0s, x1s);		if (EQ(x0s, (X)0xabcdef00)) {OK;} else {NG;}

	/* lslul */
	SET2(ux0a, (UX)0x6789abcd, ux1a, (UX)1);	SHL(ux0a, ux1a);	if (EQ(ux0a, (UX)0xcf13579a)) {OK;} else {NG;}
	SET2(ux0a, (UX)0x789abcde, ux1s, (UX)2);	SHL(ux0a, ux1s);	if (EQ(ux0a, (UX)0xe26af378)) {OK;} else {NG;}
	SET2(ux0s, (UX)0x89abcdef, ux1a, (UX)3);	SHL(ux0s, ux1a);	if (EQ(ux0s, (UX)0x4d5e6f78)) {OK;} else {NG;}
	SET2(ux0s, (UX)0x9abcdef0, ux1s, (UX)4);	SHL(ux0s, ux1s);	if (EQ(ux0s, (UX)0xabcdef00)) {OK;} else {NG;}

	mikeos_print_newline();

	return;
}

static	void	check_shr(void)
{	
	X		x0a, x1a;
	UX		ux0a, ux1a;
	static	X	x0s, x1s;
	static	UX	ux0s, ux1s;
	static	char	str[] = "check_shr ";

	mikeos_print_string(str);
	P(x0a); P(x1a); P(ux0a); P(ux1a);
	P(x0s); P(x1s); P(ux0s); P(ux1s);
	mikeos_print_newline();

	/* lsrl */
	SET2(x0a, (X)0x6789abcd, x1a, (X)1);		SHR(x0a, x1a);		if (EQ(x0a, (X)0x33c4d5e6)) {OK;} else {NG;}
	SET2(x0a, (X)0x789abcde, x1s, (X)2);		SHR(x0a, x1s);		if (EQ(x0a, (X)0x1e26af37)) {OK;} else {NG;}
	SET2(x0s, (X)0x89abcdef, x1a, (X)3);		SHR(x0s, x1a);		if (EQ(x0s, (X)0xf13579bd)) {OK;} else {NG;}
	SET2(x0s, (X)0x9abcdef0, x1s, (X)4);		SHR(x0s, x1s);		if (EQ(x0s, (X)0xf9abcdef)) {OK;} else {NG;}

	/* lsrul */
	SET2(ux0a, (UX)0x6789abcd, ux1a, (UX)1);	SHR(ux0a, ux1a);	if (EQ(ux0a, (UX)0x33c4d5e6)) {OK;} else {NG;}
	SET2(ux0a, (UX)0x789abcde, ux1s, (UX)2);	SHR(ux0a, ux1s);	if (EQ(ux0a, (UX)0x1e26af37)) {OK;} else {NG;}
	SET2(ux0s, (UX)0x89abcdef, ux1a, (UX)3);	SHR(ux0s, ux1a);	if (EQ(ux0s, (UX)0x113579bd)) {OK;} else {NG;}
	SET2(ux0s, (UX)0x9abcdef0, ux1s, (UX)4);	SHR(ux0s, ux1s);	if (EQ(ux0s, (UX)0x09abcdef)) {OK;} else {NG;}

	mikeos_print_newline();

	return;
}

static	void	check_inc(void)
{	
	X		x0a;
	UX		ux0a;
	static	X	x0s;
	static	UX	ux0s;
	static	char	str[] = "check_inc ";

	mikeos_print_string(str);
	P(x0a); P(ux0a);
	P(x0s); P(ux0s);
	mikeos_print_newline();

	/* lincl */
	SET1(x0a, (X)0x00000000);	if (EQ(++x0a, (X)0x00000001)) {OK;} else {NG;}
	SET1(x0s, (X)0x0000ffff);	if (EQ(++x0s, (X)0x00010000)) {OK;} else {NG;}

	SET1(ux0a, (UX)0x00010000);	if (EQ(++ux0a, (UX)0x00010001)) {OK;} else {NG;}
	SET1(ux0s, (UX)0xffffffff);	if (EQ(++ux0s, (UX)0x00000000)) {OK;} else {NG;}

	mikeos_print_newline();

	return;
}

static	void	check_dec(void)
{	
	X		x0a;
	UX		ux0a;
	static	X	x0s;
	static	UX	ux0s;
	static	char	str[] = "check_dec ";

	mikeos_print_string(str);
	P(x0a); P(ux0a);
	P(x0s); P(ux0s);
	mikeos_print_newline();

	/* ldecl */
	SET1(x0a, (X)0x00000000);	if (EQ(--x0a, (X)0xffffffff)) {OK;} else {NG;}
	SET1(x0s, (X)0x00010000);	if (EQ(--x0s, (X)0x0000ffff)) {OK;} else {NG;}

	SET1(ux0a, (UX)0x00010001);	if (EQ(--ux0a, (UX)0x00010000)) {OK;} else {NG;}
	SET1(ux0s, (UX)0x00000001);	if (EQ(--ux0s, (UX)0x00000000)) {OK;} else {NG;}

	mikeos_print_newline();

	return;
}

static	void	check_tst(void)
{	
	X		x0a;
	UX		ux0a;
	static	X	x0s;
	static	UX	ux0s;
	static	char	str[] = "check_tst ";

	mikeos_print_string(str);
	P(x0a); P(ux0a);
	P(x0s); P(ux0s);
	mikeos_print_newline();

	/* ltstl */
	SET1(x0a, (X)0x00000000);	if (x0a) {NG;} else {OK;}
	SET1(x0a, (X)0x00000001);	if (x0a) {OK;} else {NG;}
	SET1(x0s, (X)0x00000000);	if (x0s) {NG;} else {OK;}
	SET1(x0s, (X)0x00010000);	if (x0s) {OK;} else {NG;}

	SET1(ux0a, (UX)0x00000000);	if (ux0a) {NG;} else {OK;}
	SET1(ux0a, (UX)0x00000001);	if (ux0a) {OK;} else {NG;}
	SET1(ux0s, (UX)0x00000000);	if (ux0s) {NG;} else {OK;}
	SET1(ux0s, (UX)0x00010000);	if (ux0s) {OK;} else {NG;}

	mikeos_print_newline();

	return;
}

static	void	check_add(void)
{	
	X		x0a, x1a;
	UX		ux0a, ux1a;
	static	X	x0s, x1s;
	static	UX	ux0s, ux1s;
	static	char	str[] = "check_add ";

	mikeos_print_string(str);
	P(x0a); P(x1a); P(ux0a); P(ux1a);
	P(x0s); P(x1s); P(ux0s); P(ux1s);
	mikeos_print_newline();

	/* laddl */
	SET2(x0a, (X)1234567890, x1a, (X)901234567);		ADD(x0a, x1a);		if (EQ(x0a, (X)2135802457)) {OK;} else {NG;}
	SET2(x0a, (X)1234567890, x1s, (X)-901234567);		ADD(x0a, x1s);		if (EQ(x0a, (X)333333323)) {OK;} else {NG;}
	SET2(x0s, (X)-1234567890, x1a, (X)901234567);		ADD(x0s, x1a);		if (EQ(x0s, (X)-333333323)) {OK;} else {NG;}
	SET2(x0s, (X)-1234567890, x1s, (X)-901234567);		ADD(x0s, x1s);		if (EQ(x0s, (X)-2135802457)) {OK;} else {NG;}

	/* laddul */
	SET2(ux0a, (UX)1234567890, ux1a, (UX)901234567);	ADD(ux0a, ux1a);	if (EQ(ux0a, (UX)2135802457)) {OK;} else {NG;}
	SET2(ux0a, (UX)1234567890, ux1s, (UX)2345678901);	ADD(ux0a, ux1s);	if (EQ(ux0a, (UX)3580246791)) {OK;} else {NG;}
	SET2(ux0s, (UX)3060399406, ux1a, (UX)901234567);	ADD(ux0s, ux1a);	if (EQ(ux0s, (UX)3961633973)) {OK;} else {NG;}
	SET2(ux0s, (UX)3060399466, ux1s, (UX)333333323);	ADD(ux0s, ux1s);	if (EQ(ux0s, (UX)3393732789)) {OK;} else {NG;}

	mikeos_print_newline();

	return;
}

static	void	check_sub(void)
{	
	X		x0a, x1a;
	UX		ux0a, ux1a;
	static	X	x0s, x1s;
	static	UX	ux0s, ux1s;
	static	char	str[] = "check_sub ";

	mikeos_print_string(str);
	P(x0a); P(x1a); P(ux0a); P(ux1a);
	P(x0s); P(x1s); P(ux0s); P(ux1s);
	mikeos_print_newline();

	/* lsubl */
	SET2(x0a, (X)1234567890, x1a, (X)901234567);		SUB(x0a, x1a);		if (EQ(x0a, (X)333333323)) {OK;} else {NG;}
	SET2(x0a, (X)1234567890, x1s, (X)-901234567);		SUB(x0a, x1s);		if (EQ(x0a, (X)2135802457)) {OK;} else {NG;}
	SET2(x0s, (X)-1234567890, x1a, (X)901234567);		SUB(x0s, x1a);		if (EQ(x0s, (X)-2135802457)) {OK;} else {NG;}
	SET2(x0s, (X)-1234567890, x1s, (X)-901234567);		SUB(x0s, x1s);		if (EQ(x0s, (X)-333333323)) {OK;} else {NG;}

	/* lsubul */
	SET2(ux0a, (UX)1234567890, ux1a, (UX)901234567);	SUB(ux0a, ux1a);	if (EQ(ux0a, (UX)333333323)) {OK;} else {NG;}
	SET2(ux0a, (UX)2345678901, ux1s, (UX)1234567890);	SUB(ux0a, ux1s);	if (EQ(ux0a, (UX)1111111011)) {OK;} else {NG;}
	SET2(ux0s, (UX)3060399406, ux1a, (UX)901234567);	SUB(ux0s, ux1a);	if (EQ(ux0s, (UX)2159164839)) {OK;} else {NG;}
	SET2(ux0s, (UX)3060399466, ux1s, (UX)333333323);	SUB(ux0s, ux1s);	if (EQ(ux0s, (UX)2727066143)) {OK;} else {NG;}

	mikeos_print_newline();

	return;
}

static	void	check_neg(void)
{	
	X		x0a;
	UX		ux0a;
	static	X	x0s;
	static	UX	ux0s;
	static	char	str[] = "check_neg ";

	mikeos_print_string(str);
	P(x0a); P(ux0a);
	P(x0s); P(ux0s);
	mikeos_print_newline();

	/* lnegl */
	SET1(x0a, (X)0x00000000);	if (EQ((X)(-x0a), (X)0x00000000)) {OK;} else {NG;}
	SET1(x0s, (X)0xffffffff);	if (EQ((X)(-x0s), (X)0x00000001)) {OK;} else {NG;}

	SET1(ux0a, (UX)0x7fffffff);	if (EQ((UX)(-ux0a), (UX)0x80000001)) {OK;} else {NG;}
	SET1(ux0s, (UX)0x80000000);	if (EQ((UX)(-ux0s), (UX)0x80000000)) {OK;} else {NG;}

	/* mikeos_long_int_negate */
	SET1(x0a, (X)0x00000000);	x0a = mikeos_long_int_negate(x0a);	if (EQ(x0a, (X)0x00000000)) {OK;} else {NG;}
	SET1(x0s, (X)0xffffffff);	x0s = mikeos_long_int_negate(x0s);	if (EQ(x0s, (X)0x00000001)) {OK;} else {NG;}

	SET1(ux0a, (UX)0x7fffffff);	ux0a = mikeos_long_int_negate(ux0a);	if (EQ(ux0a, (UX)0x80000001)) {OK;} else {NG;}
	SET1(ux0s, (UX)0x80000000);	ux0s = mikeos_long_int_negate(ux0s);	if (EQ(ux0s, (UX)0x80000000)) {OK;} else {NG;}

	mikeos_print_newline();

	return;
}

static	void	check_mul(void)
{	
	X		x0a, x1a;
	UX		ux0a, ux1a;
	static	X	x0s, x1s;
	static	UX	ux0s, ux1s;
	static	char	str[] = "check_mul ";

	mikeos_print_string(str);
	P(x0a); P(x1a); P(ux0a); P(ux1a);
	P(x0s); P(x1s); P(ux0s); P(ux1s);
	mikeos_print_newline();

	/* lmull */
	SET2(x0a, (X)12345, x1a, (X)90123);		MUL(x0a, x1a);		if (EQ(x0a, (X)1112568435)) {OK;} else {NG;}
	SET2(x0a, (X)12345, x1s, (X)-90123);		MUL(x0a, x1s);		if (EQ(x0a, (X)-1112568435)) {OK;} else {NG;}
	SET2(x0s, (X)-62125, x1a, (X)34567);		MUL(x0s, x1a);		if (EQ(x0s, (X)-2147474875)) {OK;} else {NG;}
	SET2(x0s, (X)-62125, x1s, (X)-34567);		MUL(x0s, x1s);		if (EQ(x0s, (X)2147474875)) {OK;} else {NG;}

	/* lmulul */
	SET2(ux0a, (UX)12345, ux1a, (UX)90123);		MUL(ux0a, ux1a);	if (EQ(ux0a, (UX)1112568435)) {OK;} else {NG;}
	SET2(ux0a, (UX)12345, ux1s, (UX)239613);	MUL(ux0a, ux1s);	if (EQ(ux0a, (UX)2958022485)) {OK;} else {NG;}
	SET2(ux0s, (UX)90123, ux1a, (UX)12345);		MUL(ux0s, ux1a);	if (EQ(ux0s, (UX)1112568435)) {OK;} else {NG;}
	SET2(ux0s, (UX)239613, ux1s, (UX)12345);	MUL(ux0s, ux1s);	if (EQ(ux0s, (UX)2958022485)) {OK;} else {NG;}

	mikeos_print_newline();

	return;
}

static	void	check_div(void)
{	
	X		x0a, x1a;
	UX		ux0a, ux1a;
	static	X	x0s, x1s;
	static	UX	ux0s, ux1s;
	static	char	str[] = "check_div ";

	mikeos_print_string(str);
	P(x0a); P(x1a); P(ux0a); P(ux1a);
	P(x0s); P(x1s); P(ux0s); P(ux1s);
	mikeos_print_newline();

	/* ldivl */
	SET2(x0a, (X)1112568435, x1a, (X)90123);	DIV(x0a, x1a);		if (EQ(x0a, (X)12345)) {OK;} else {NG;}
	SET2(x0a, (X)1112568435, x1s, (X)-12345);	DIV(x0a, x1s);		if (EQ(x0a, (X)-90123)) {OK;} else {NG;}
	SET2(x0s, (X)-2147474875, x1a, (X)34567);	DIV(x0s, x1a);		if (EQ(x0s, (X)-62125)) {OK;} else {NG;}
	SET2(x0s, (X)-2147474875, x1s, (X)-62125);	DIV(x0s, x1s);		if (EQ(x0s, (X)34567)) {OK;} else {NG;}

	/* ldivul */
	SET2(ux0a, (UX)1112568435, ux1a, (UX)12345);	DIV(ux0a, ux1a);	if (EQ(ux0a, (UX)90123)) {OK;} else {NG;}
	SET2(ux0a, (UX)2958022485, ux1s, (UX)239613);	DIV(ux0a, ux1s);	if (EQ(ux0a, (UX)12345)) {OK;} else {NG;}
	SET2(ux0s, (UX)1112568435, ux1a, (UX)12345);	DIV(ux0s, ux1a);	if (EQ(ux0s, (UX)90123)) {OK;} else {NG;}
	SET2(ux0s, (UX)2958022485, ux1s, (UX)239613);	DIV(ux0s, ux1s);	if (EQ(ux0s, (UX)12345)) {OK;} else {NG;}

	mikeos_print_newline();

	return;
}

static	void	check_mod(void)
{	
	X		x0a, x1a;
	UX		ux0a, ux1a;
	static	X	x0s, x1s;
	static	UX	ux0s, ux1s;
	static	char	str[] = "check_mod ";

	mikeos_print_string(str);
	P(x0a); P(x1a); P(ux0a); P(ux1a);
	P(x0s); P(x1s); P(ux0s); P(ux1s);
	mikeos_print_newline();

	/* lmodl */
	SET2(x0a, (X)1112604878, x1a, (X)90123);	MOD(x0a, x1a);		if (EQ(x0a, (X)36443)) {OK;} else {NG;}
	SET2(x0a, (X)1112524644, x1s, (X)-12345);	MOD(x0a, x1s);		if (EQ(x0a, (X)5589)) {OK;} else {NG;}
	SET2(x0s, (X)-2147474800, x1a, (X)34567);	MOD(x0s, x1a);		if (EQ(x0s, (X)-34492)) {OK;} else {NG;}
	SET2(x0s, (X)-2147474800, x1s, (X)-62125);	MOD(x0s, x1s);		if (EQ(x0s, (X)-62050)) {OK;} else {NG;}

	/* lmodul */
	SET2(ux0a, (UX)1112524644, ux1a, (UX)12345);	MOD(ux0a, ux1a);	if (EQ(ux0a, (UX)5589)) {OK;} else {NG;}
	SET2(ux0a, (UX)2958242607, ux1s, (UX)239613);	MOD(ux0a, ux1s);	if (EQ(ux0a, (UX)220122)) {OK;} else {NG;}
	SET2(ux0s, (UX)1112604878, ux1a, (UX)90123);	MOD(ux0s, ux1a);	if (EQ(ux0s, (UX)36443)) {OK;} else {NG;}
	SET2(ux0s, (UX)2958242600, ux1s, (UX)239613);	MOD(ux0s, ux1s);	if (EQ(ux0s, (UX)220115)) {OK;} else {NG;}

	mikeos_print_newline();

	return;
}

#if defined(TEST_BCC_MIKEOS)
int	MikeMain(void *argument)
#else
int	main(int argc, char *argv[])
#endif
{
	check_eq();
	check_and();
	check_or();
	check_xor();
	check_not();
	check_shl();
	check_shr();
	check_inc();
	check_dec();

	mikeos_wait_for_key();

	check_tst();
	check_add();
	check_sub();
	check_neg();
	check_mul();
	check_div();
	check_mod();

	mikeos_print_4hex(NGCount);
	mikeos_print_newline();

	/* test exit() */
	exit(0);
}

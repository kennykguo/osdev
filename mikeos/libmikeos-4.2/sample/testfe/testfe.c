/* testfe.c - short operation (bcc stuff) */

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

typedef	short		X;
typedef	unsigned short	UX;

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

	SET2(x0a, (X)0x4567, x1a, (X)0x4567);		if (EQ(x0a, x1a)) {OK;} else {NG;}
	SET2(x0a, (X)0x5678, x1s, (X)0x5678);		if (EQ(x0a, x1s)) {OK;} else {NG;}
	SET2(x0s, (X)0x6789, x1a, (X)0x6789);		if (EQ(x0s, x1a)) {OK;} else {NG;}
	SET2(x0s, (X)0x789a, x1s, (X)0x789a);		if (EQ(x0s, x1s)) {OK;} else {NG;}

	SET2(x0a, (X)0x4567, x1a, (X)0x5678);		if (EQ(x0a, x1a)) {NG;} else {OK;}
	SET2(x0a, (X)0x5678, x1s, (X)0x6789);		if (EQ(x0a, x1s)) {NG;} else {OK;}
	SET2(x0s, (X)0x6789, x1a, (X)0x789a);		if (EQ(x0s, x1a)) {NG;} else {OK;}
	SET2(x0s, (X)0x789a, x1s, (X)0x4567);		if (EQ(x0s, x1s)) {NG;} else {OK;}

	SET2(ux0a, (UX)0x4567, ux1a, (UX)0x4567);	if (EQ(ux0a, ux1a)) {OK;} else {NG;}
	SET2(ux0a, (UX)0x5678, ux1s, (UX)0x5678);	if (EQ(ux0a, ux1s)) {OK;} else {NG;}
	SET2(ux0s, (UX)0x6789, ux1a, (UX)0x6789);	if (EQ(ux0s, ux1a)) {OK;} else {NG;}
	SET2(ux0s, (UX)0x789a, ux1s, (UX)0x789a);	if (EQ(ux0s, ux1s)) {OK;} else {NG;}

	SET2(ux0a, (UX)0x4567, ux1a, (UX)0x5678);	if (EQ(ux0a, ux1a)) {NG;} else {OK;}
	SET2(ux0a, (UX)0x5678, ux1s, (UX)0x6789);	if (EQ(ux0a, ux1s)) {NG;} else {OK;}
	SET2(ux0s, (UX)0x6789, ux1a, (UX)0x789a);	if (EQ(ux0s, ux1a)) {NG;} else {OK;}
	SET2(ux0s, (UX)0x789a, ux1s, (UX)0x4567);	if (EQ(ux0s, ux1s)) {NG;} else {OK;}

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

	SET2(x0a, (X)0x4567, x1a, (X)0x5678);		AND(x0a, x1a);		if (EQ(x0a, (X)0x4460)) {OK;} else {NG;}
	SET2(x0a, (X)0x5678, x1s, (X)0x6789);		AND(x0a, x1s);		if (EQ(x0a, (X)0x4608)) {OK;} else {NG;}
	SET2(x0s, (X)0x6789, x1a, (X)0x789a);		AND(x0s, x1a);		if (EQ(x0s, (X)0x6088)) {OK;} else {NG;}
	SET2(x0s, (X)0x789a, x1s, (X)0x4567);		AND(x0s, x1s);		if (EQ(x0s, (X)0x4002)) {OK;} else {NG;}

	SET2(ux0a, (UX)0x04567, ux1a, (UX)0x5678);	AND(ux0a, ux1a);	if (EQ(ux0a, (UX)0x4460)) {OK;} else {NG;}
	SET2(ux0a, (UX)0x05678, ux1s, (UX)0x6789);	AND(ux0a, ux1s);	if (EQ(ux0a, (UX)0x4608)) {OK;} else {NG;}
	SET2(ux0s, (UX)0x06789, ux1a, (UX)0x789a);	AND(ux0s, ux1a);	if (EQ(ux0s, (UX)0x6088)) {OK;} else {NG;}
	SET2(ux0s, (UX)0x0789a, ux1s, (UX)0x4567);	AND(ux0s, ux1s);	if (EQ(ux0s, (UX)0x4002)) {OK;} else {NG;}

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

	SET2(x0a, (X)0x4567, x1a, (X)0x5678);		OR(x0a, x1a);	if (EQ(x0a, (X)0x577f)) {OK;} else {NG;}
	SET2(x0a, (X)0x5678, x1s, (X)0x6789);		OR(x0a, x1s);	if (EQ(x0a, (X)0x77f9)) {OK;} else {NG;}
	SET2(x0s, (X)0x6789, x1a, (X)0x789a);		OR(x0s, x1a);	if (EQ(x0s, (X)0x7f9b)) {OK;} else {NG;}
	SET2(x0s, (X)0x789a, x1s, (X)0x4567);		OR(x0s, x1s);	if (EQ(x0s, (X)0x7dff)) {OK;} else {NG;}

	SET2(ux0a, (UX)0x4567, ux1a, (UX)0x5678);	OR(ux0a, ux1a);	if (EQ(ux0a, (UX)0x577f)) {OK;} else {NG;}
	SET2(ux0a, (UX)0x5678, ux1s, (UX)0x6789);	OR(ux0a, ux1s);	if (EQ(ux0a, (UX)0x77f9)) {OK;} else {NG;}
	SET2(ux0s, (UX)0x6789, ux1a, (UX)0x789a);	OR(ux0s, ux1a);	if (EQ(ux0s, (UX)0x7f9b)) {OK;} else {NG;}
	SET2(ux0s, (UX)0x789a, ux1s, (UX)0x4567);	OR(ux0s, ux1s);	if (EQ(ux0s, (UX)0x7dff)) {OK;} else {NG;}

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

	SET2(x0a, (X)0x4567, x1a, (X)0x5678);		XOR(x0a, x1a);		if (EQ(x0a, (X)0x131f)) {OK;} else {NG;}
	SET2(x0a, (X)0x5678, x1s, (X)0x6789);		XOR(x0a, x1s);		if (EQ(x0a, (X)0x31f1)) {OK;} else {NG;}
	SET2(x0s, (X)0x6789, x1a, (X)0x789a);		XOR(x0s, x1a);		if (EQ(x0s, (X)0x1f13)) {OK;} else {NG;}
	SET2(x0s, (X)0x789a, x1s, (X)0x4567);		XOR(x0s, x1s);		if (EQ(x0s, (X)0x3dfd)) {OK;} else {NG;}

	SET2(ux0a, (UX)0x4567, ux1a, (UX)0x5678);	XOR(ux0a, ux1a);	if (EQ(ux0a, (UX)0x131f)) {OK;} else {NG;}
	SET2(ux0a, (UX)0x5678, ux1s, (UX)0x6789);	XOR(ux0a, ux1s);	if (EQ(ux0a, (UX)0x31f1)) {OK;} else {NG;}
	SET2(ux0s, (UX)0x6789, ux1a, (UX)0x789a);	XOR(ux0s, ux1a);	if (EQ(ux0s, (UX)0x1f13)) {OK;} else {NG;}
	SET2(ux0s, (UX)0x789a, ux1s, (UX)0x4567);	XOR(ux0s, ux1s);	if (EQ(ux0s, (UX)0x3dfd)) {OK;} else {NG;}

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

	SET1(x0a, (X)0x4567);	NOT(x0a);	if (EQ(x0a, (X)0xba98)) {OK;} else {NG;}
	SET1(x0s, (X)0x6789);	NOT(x0s);	if (EQ(x0s, (X)0x9876)) {OK;} else {NG;}

	SET1(ux0a, (UX)0x4567);	NOT(ux0a);	if (EQ(ux0a, (UX)0xba98)) {OK;} else {NG;}
	SET1(ux0s, (UX)0x6789);	NOT(ux0s);	if (EQ(ux0s, (UX)0x9876)) {OK;} else {NG;}

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

	SET2(x0a, (X)0xabcd, x1a, (X)1);	SHL(x0a, x1a);		if (EQ(x0a, (X)0x579a)) {OK;} else {NG;}
	SET2(x0a, (X)0xbcde, x1s, (X)2);	SHL(x0a, x1s);		if (EQ(x0a, (X)0xf378)) {OK;} else {NG;}
	SET2(x0s, (X)0xcdef, x1a, (X)3);	SHL(x0s, x1a);		if (EQ(x0s, (X)0x6f78)) {OK;} else {NG;}
	SET2(x0s, (X)0xdef0, x1s, (X)4);	SHL(x0s, x1s);		if (EQ(x0s, (X)0xef00)) {OK;} else {NG;}

	SET2(ux0a, (UX)0xabcd, ux1a, (UX)1);	SHL(ux0a, ux1a);	if (EQ(ux0a, (UX)0x579a)) {OK;} else {NG;}
	SET2(ux0a, (UX)0xbcde, ux1s, (UX)2);	SHL(ux0a, ux1s);	if (EQ(ux0a, (UX)0xf378)) {OK;} else {NG;}
	SET2(ux0s, (UX)0xcdef, ux1a, (UX)3);	SHL(ux0s, ux1a);	if (EQ(ux0s, (UX)0x6f78)) {OK;} else {NG;}
	SET2(ux0s, (UX)0xdef0, ux1s, (UX)4);	SHL(ux0s, ux1s);	if (EQ(ux0s, (UX)0xef00)) {OK;} else {NG;}

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

	SET2(x0a, (X)0xabcd, x1a, (X)1);	SHR(x0a, x1a);		if (EQ(x0a, (X)0xd5e6)) {OK;} else {NG;}
	SET2(x0a, (X)0xbcde, x1s, (X)2);	SHR(x0a, x1s);		if (EQ(x0a, (X)0xef37)) {OK;} else {NG;}
	SET2(x0s, (X)0xcdef, x1a, (X)3);	SHR(x0s, x1a);		if (EQ(x0s, (X)0xf9bd)) {OK;} else {NG;}
	SET2(x0s, (X)0xdef0, x1s, (X)4);	SHR(x0s, x1s);		if (EQ(x0s, (X)0xfdef)) {OK;} else {NG;}

	SET2(ux0a, (UX)0xabcd, ux1a, (UX)1);	SHR(ux0a, ux1a);	if (EQ(ux0a, (UX)0x55e6)) {OK;} else {NG;}
	SET2(ux0a, (UX)0xbcde, ux1s, (UX)2);	SHR(ux0a, ux1s);	if (EQ(ux0a, (UX)0x2f37)) {OK;} else {NG;}
	SET2(ux0s, (UX)0xcdef, ux1a, (UX)3);	SHR(ux0s, ux1a);	if (EQ(ux0s, (UX)0x19bd)) {OK;} else {NG;}
	SET2(ux0s, (UX)0xdef0, ux1s, (UX)4);	SHR(ux0s, ux1s);	if (EQ(ux0s, (UX)0x0def)) {OK;} else {NG;}

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

	SET1(x0a, (X)0x0000);	if (EQ(++x0a, (X)0x0001)) {OK;} else {NG;}
	SET1(x0s, (X)0x00ff);	if (EQ(++x0s, (X)0x0100)) {OK;} else {NG;}

	SET1(ux0a, (UX)0x0100);	if (EQ(++ux0a, (UX)0x0101)) {OK;} else {NG;}
	SET1(ux0s, (UX)0xffff);	if (EQ(++ux0s, (UX)0x0000)) {OK;} else {NG;}

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

	SET1(x0a, (X)0x0000);	if (EQ(--x0a, (X)0xffff)) {OK;} else {NG;}
	SET1(x0s, (X)0x0100);	if (EQ(--x0s, (X)0x00ff)) {OK;} else {NG;}

	SET1(ux0a, (UX)0x0101);	if (EQ(--ux0a, (UX)0x0100)) {OK;} else {NG;}
	SET1(ux0s, (UX)0x0001);	if (EQ(--ux0s, (UX)0x0000)) {OK;} else {NG;}

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

	SET1(x0a, (X)0x0000);	if (x0a) {NG;} else {OK;}
	SET1(x0a, (X)0x0001);	if (x0a) {OK;} else {NG;}
	SET1(x0s, (X)0x0000);	if (x0s) {NG;} else {OK;}
	SET1(x0s, (X)0x0100);	if (x0s) {OK;} else {NG;}

	SET1(ux0a, (UX)0x0000);	if (ux0a) {NG;} else {OK;}
	SET1(ux0a, (UX)0x0001);	if (ux0a) {OK;} else {NG;}
	SET1(ux0s, (UX)0x0000);	if (ux0s) {NG;} else {OK;}
	SET1(ux0s, (UX)0x0100);	if (ux0s) {OK;} else {NG;}

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

	SET2(x0a, (X)23456, x1a, (X)9012);	ADD(x0a, x1a);		if (EQ(x0a, (X)32468)) {OK;} else {NG;}
	SET2(x0a, (X)23456, x1s, (X)-9012);	ADD(x0a, x1s);		if (EQ(x0a, (X)14444)) {OK;} else {NG;}
	SET2(x0s, (X)-23456, x1a, (X)9012);	ADD(x0s, x1a);		if (EQ(x0s, (X)-14444)) {OK;} else {NG;}
	SET2(x0s, (X)-23456, x1s, (X)-9012);	ADD(x0s, x1s);		if (EQ(x0s, (X)-32468)) {OK;} else {NG;}

	SET2(ux0a, (UX)23456, ux1a, (UX)9012);	ADD(ux0a, ux1a);	if (EQ(ux0a, (UX)32468)) {OK;} else {NG;}
	SET2(ux0a, (UX)23456, ux1s, (UX)12345);	ADD(ux0a, ux1s);	if (EQ(ux0a, (UX)35801)) {OK;} else {NG;}
	SET2(ux0s, (UX)34567, ux1a, (UX)9012);	ADD(ux0s, ux1a);	if (EQ(ux0s, (UX)43579)) {OK;} else {NG;}
	SET2(ux0s, (UX)45678, ux1s, (UX)12345);	ADD(ux0s, ux1s);	if (EQ(ux0s, (UX)58023)) {OK;} else {NG;}

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

	SET2(x0a, (X)32468, x1a, (X)9012);	SUB(x0a, x1a);		if (EQ(x0a, (X)23456)) {OK;} else {NG;}
	SET2(x0a, (X)14444, x1s, (X)-9012);	SUB(x0a, x1s);		if (EQ(x0a, (X)23456)) {OK;} else {NG;}
	SET2(x0s, (X)-14444, x1a, (X)9012);	SUB(x0s, x1a);		if (EQ(x0s, (X)-23456)) {OK;} else {NG;}
	SET2(x0s, (X)-32468, x1s, (X)-9012);	SUB(x0s, x1s);		if (EQ(x0s, (X)-23456)) {OK;} else {NG;}

	SET2(ux0a, (UX)32468, ux1a, (UX)9012);	SUB(ux0a, ux1a);	if (EQ(ux0a, (UX)23456)) {OK;} else {NG;}
	SET2(ux0a, (UX)35801, ux1s, (UX)12345);	SUB(ux0a, ux1s);	if (EQ(ux0a, (UX)23456)) {OK;} else {NG;}
	SET2(ux0s, (UX)43579, ux1a, (UX)9012);	SUB(ux0s, ux1a);	if (EQ(ux0s, (UX)34567)) {OK;} else {NG;}
	SET2(ux0s, (UX)58023, ux1s, (UX)12345);	SUB(ux0s, ux1s);	if (EQ(ux0s, (UX)45678)) {OK;} else {NG;}

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

	SET1(x0a, (X)0x0000);	if (EQ((X)(-x0a), (X)0x0000)) {OK;} else {NG;}
	SET1(x0s, (X)0xffff);	if (EQ((X)(-x0s), (X)0x0001)) {OK;} else {NG;}

	SET1(ux0a, (UX)0x7fff);	if (EQ((UX)(-ux0a), (UX)0x8001)) {OK;} else {NG;}
	SET1(ux0s, (UX)0x8000);	if (EQ((UX)(-ux0s), (UX)0x8000)) {OK;} else {NG;}

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

	SET2(x0a, (X)345, x1a, (X)90);		MUL(x0a, x1a);		if (EQ(x0a, (X)31050)) {OK;} else {NG;}
	SET2(x0a, (X)345, x1s, (X)-90);		MUL(x0a, x1s);		if (EQ(x0a, (X)-31050)) {OK;} else {NG;}
	SET2(x0s, (X)-345, x1a, (X)90);		MUL(x0s, x1a);		if (EQ(x0s, (X)-31050)) {OK;} else {NG;}
	SET2(x0s, (X)-345, x1s, (X)-90);	MUL(x0s, x1s);		if (EQ(x0s, (X)31050)) {OK;} else {NG;}

	SET2(ux0a, (UX)345, ux1a, (UX)90);	MUL(ux0a, ux1a);	if (EQ(ux0a, (UX)31050)) {OK;} else {NG;}
	SET2(ux0a, (UX)345, ux1s, (UX)123);	MUL(ux0a, ux1s);	if (EQ(ux0a, (UX)42435)) {OK;} else {NG;}
	SET2(ux0s, (UX)255, ux1a, (UX)255);	MUL(ux0s, ux1a);	if (EQ(ux0s, (UX)65025)) {OK;} else {NG;}
	SET2(ux0s, (UX)13107, ux1s, (UX)5);	MUL(ux0s, ux1s);	if (EQ(ux0s, (UX)65535)) {OK;} else {NG;}

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

	SET2(x0a, (X)31050, x1a, (X)345);	DIV(x0a, x1a);		if (EQ(x0a, (X)90)) {OK;} else {NG;}
	SET2(x0a, (X)31050, x1s, (X)-345);	DIV(x0a, x1s);		if (EQ(x0a, (X)-90)) {OK;} else {NG;}
	SET2(x0s, (X)-31050, x1a, (X)90);	DIV(x0s, x1a);		if (EQ(x0s, (X)-345)) {OK;} else {NG;}
	SET2(x0s, (X)-31050, x1s, (X)-90);	DIV(x0s, x1s);		if (EQ(x0s, (X)345)) {OK;} else {NG;}

	/* idiv_u */
	SET2(ux0a, (UX)31050, ux1a, (UX)345);	DIV(ux0a, ux1a);	if (EQ(ux0a, (UX)90)) {OK;} else {NG;}
	SET2(ux0a, (UX)42435, ux1s, (UX)123);	DIV(ux0a, ux1s);	if (EQ(ux0a, (UX)345)) {OK;} else {NG;}
	SET2(ux0s, (UX)65025, ux1a, (UX)255);	DIV(ux0s, ux1a);	if (EQ(ux0s, (UX)255)) {OK;} else {NG;}
	SET2(ux0s, (UX)65535, ux1s, (UX)5);	DIV(ux0s, ux1s);	if (EQ(ux0s, (UX)13107)) {OK;} else {NG;}

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

	/* imod */
	SET2(x0a, (X)31394, x1a, (X)345);	MOD(x0a, x1a);		if (EQ(x0a, (X)344)) {OK;} else {NG;}
	SET2(x0a, (X)31152, x1s, (X)-345);	MOD(x0a, x1s);		if (EQ(x0a, (X)102)) {OK;} else {NG;}
	SET2(x0s, (X)-31110, x1a, (X)90);	MOD(x0s, x1a);		if (EQ(x0s, (X)-60)) {OK;} else {NG;}
	SET2(x0s, (X)-31139, x1s, (X)-90);	MOD(x0s, x1s);		if (EQ(x0s, (X)-89)) {OK;} else {NG;}

	/* imodu */
	SET2(ux0a, (UX)31394, ux1a, (UX)345);	MOD(ux0a, ux1a);	if (EQ(ux0a, (UX)344)) {OK;} else {NG;}
	SET2(ux0a, (UX)42495, ux1s, (UX)123);	MOD(ux0a, ux1s);	if (EQ(ux0a, (UX)60)) {OK;} else {NG;}
	SET2(ux0s, (UX)65127, ux1a, (UX)255);	MOD(ux0s, ux1a);	if (EQ(ux0s, (UX)102)) {OK;} else {NG;}
	SET2(ux0s, (UX)65532, ux1s, (UX)5);	MOD(ux0s, ux1s);	if (EQ(ux0s, (UX)2)) {OK;} else {NG;}

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

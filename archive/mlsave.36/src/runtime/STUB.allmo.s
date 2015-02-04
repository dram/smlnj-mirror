#ifdef NS32
#define	longword(x)	.double x
#else
#define longword(x)	.long x
#endif

#if 1
	/* This defines _datalist with one garbage entry. */
	.globl	_datalist
	.text
	longword(0x0000010f)		/* String tag */
LC0:
	.ascii	"%never%^match\0\0\0"
	longword(0x0000010f)		/* String tag */
LC1:
	.ascii	"%bad%^%^data%\0\0\0"
	.data
	.align	2
_datalist:
	longword(LC0)
	longword(LC1)
	longword(1)	/* Next link: null */
#else
	/* This version works on the Encore Multimax */
	.globl	_datalist
	.set	_datalist,1
#endif

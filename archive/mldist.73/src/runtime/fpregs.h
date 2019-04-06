#ifndef C
#if defined(SPARC)
#	define NSAVED_FPREGS	0
#endif
#if defined(M68)
#	define NSAVED_FPREGS 	7
#endif
#if defined(VAX)
#	define NSAVED_FPREGS	0
#endif
#if defined(NS32)
#	define NSAVED_FPREGS 	??
#endif
#if defined(MIPS)
#	define NSAVED_FPREGS	5
#endif
#endif

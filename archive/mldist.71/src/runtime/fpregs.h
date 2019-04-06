#ifndef C
#if defined(SPARC)
#	define NSAVED_FPREGS	0
#elif defined(M68)
#	define NSAVED_FPREGS 	7
#elif defined(VAX)
#	define NSAVED_FPREGS	0
#elif defined(NS32)
#	define NSAVED_FPREGS 	??
#elif defined(MIPS)
#	define NSAVED_FPREGS	5
#endif
#endif

/*
 *	File:		Cache.c
 *
 *	Contains:	FlushCacheRange: a machine and system independent cache flush routine
 *
 *	Cache control is a highly CPU specific function.  Although some system independence
 *	is achieved via use of the _HWPriv trap, this solution may not be general for all
 *	hardware/system software combinations.  FlushCacheRange should solve that for
 *	developers by working on all systems.
 *	
 *	Written by:	Dave Radcliffe
 *
 *	Copyright:	© 1991 by Apple Computer, Inc., all rights reserved.
 *
 *	Change History:
 *
 *		1/20/92		DR		Added System603OrLater conditional code
 *		1/10/92		DR		Added FlushCacheWithCPushA()
 *		12/18/91	DR		New today
 *
 */

#ifndef __TYPES__
#include <Types.h>
#endif

#ifndef __TRAPS__
#include <Traps.h>
#endif

#ifndef __OSUTILS__
#include <OSUtils.h>
#endif

#ifndef __GESTALTEQU__
#include <GestaltEqu.h>
#endif

#ifndef __ERRORS__
#include <Errors.h>
#endif

/*
 * If you can guarantee you are running on System 6.0.3 or later, then some of the
 * alternative cache flushing code implemented below is unnecessary as you are
 * assured of having the _HWPriv trap.  So, if you are running on System 6.0.3 or
 * later, uncommenting the following line can reduce this code by more than half.
 */
#define System603OrLater

/* The next two declarations are defined in Tech Note #261 */
#ifndef FlushCodeCacheRange
/* MPW C 3.1 and earlier, and THINK C should declare the function as   */
/* ÒpascalÓ and use the same inline constants as the Pascal interface: */
pascal OSErr FlushCodeCacheRange (void *address, unsigned long count) =
    {0x225F, 0x205F, 0x7009, 0xA198, 0x3E80};
	
/* The above declaration works for MPW C 3.2 as well, but 3.2 allows   */
/* register specifications to make register-based inline calls very    */
/* efficient.  So, under MPW C 3.2, you may choose to uncomment the    */
/* following code, instead.                                            */
/*
#pragma parameter __D0 FlushCodeCacheRange(__A0,__A1)
OSErr FlushCodeCacheRange (void *address, unsigned long count) =
    {0x7009, 0xA198};
 */
#endif

#ifndef FlushCodeCache
#define _CacheFlush 0xA0BD
void FlushCodeCache (void) = _CacheFlush;
#endif

/* 
 * FlushCacheViaCACR is an inline assembly routine that flushes both the 
 * instruction and data caches by writing directly to the CACR.  Used only
 * as a last resort by FlushCacheRange
 */
void FlushCacheViaCACR ( void ) =
	{ 0x4E7A, 0x0002,		/* MOVEC	CACR,D0 */
	  0x08C0, 0x0003,		/* BSET		#3,D0   */
	  0x4E7B, 0x0002 };		/* MOVEC	D0,CACR */

/*
 * FlushCacheWithCPushA is another inline assembly routine that flushes caches
 * on the MC68040 using the CPushA instruction.  Used only as a last resort
 * by FlushCacheRange
 */
void FlushCacheWithCPushA ( void ) =
	{ 0x4E71,				/* NOP, to clear pending writes */
	  0xF4F8 };				/* CPUSHA	BC */
	  
/*
 * FlushCacheRange flushes both the data and instruction caches for the block of 
 * memory starting at location address with size count.  Flushing the cache for 
 * a range of memory is only supported on the 68040, so if this functionality is
 * unavailable, the entire cache is flushed (if appropriate).
 *
 * If either address is NIL or count is zero, the entire cache is flushed anyway.
 * Selective flushing of the cache is a time consuming process and you may wish to
 * avoid it when there is no benefit to doing so.  For example, if you've recently
 * manipulated a block larger than 4K (the size of the 68040 caches), selective
 * flushing will probably end up flushing the entire cache anyway, so why bother?
 *
 * The preferred method for flushing the cache is to use the _HWPriv trap documented
 * in Tech Note #261.  Some older systems may not have this trap implemented, so
 * alternate methods must be used.  The first thing to try is the _CacheFlush trap.
 * This was implemented beginning with the Mac II (and is also documented in Tech
 * Note #261).
 *
 * MC68000 based systems have no cache, but if an accelerator board has been added.
 * they may have a CPU which does have a cache.  Accelerator board vendors should
 * implement _HWPriv or _CacheFlush for such systems, but if they haven't then
 * our last resort is to control the caches directly (using privileged
 * instructions (GAK!!)).
 *
 * FINALLY, the full implementation of FlushCacheRange is probably overkill and may be 
 * less than optimal for some applications.  For example, testing for _HWPriv 
 * should be unnecessary on systems later than 6.0.2, so I've added conditional code
 * to allow you to bypass that if appropriate (see comment above on System603OrLater).
 * The full implementation tries to cover a lot of obscure cases, but it also does 
 * some things inefficiently.  For example, having determined the _HWPriv is 
 * implemented, it would be more efficient to just keep that information around, 
 * but I wanted to avoid the complications of global or static variables.  But 
 * it is also true that if you find it necessary to call this code more than a
 * few times between the time your application starts and the time it quits, 
 * YOU ARE DOING SOMETHING WRONG!!   Go back and rethink your code.
 */
void FlushCacheRange (void *address, unsigned long count)
{
#ifndef System603OrLater
	long	gestaltResponse;			/* For CPU type */
	long	unimpTrapAddress;			/* Address of Unimplemented Trap */
	
	/* First check to see if _HWPriv is implemented */
	if ((unimpTrapAddress = NGetTrapAddress(_Unimplemented, ToolTrap)) != 
		NGetTrapAddress(_HWPriv, OSTrap)) {
#endif
		/* 
		 * Try to flush the specified range.  If it fails or if there is no range,
		 * then flush the entire cache 
		 */
		if (!(address && count && (FlushCodeCacheRange (address, count) != hwParamErr))) {	
			/* Flush entire cache */
			FlushInstructionCache ();
		}
#ifndef System603OrLater
	} else {				/* No _HWPriv trap */
		/* Try for _CacheFlush trap */
		if (unimpTrapAddress != NGetTrapAddress(_CacheFlush, OSTrap))
			FlushCodeCache ();
		else		/* Nothing else works, so do machine specific cache control */
			/* Only bother with cache control on 68020 and above */
			if (!Gestalt (gestaltProcessorType, &gestaltResponse) && 
				(gestaltResponse >= gestalt68020))
					if (gestaltResponse <= gestalt68030)
						FlushCacheViaCACR ();
					else
						FlushCacheWithCPushA ();
	}
#endif
	return;
}	/* FlushCacheRange */
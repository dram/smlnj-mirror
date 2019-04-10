/*
 *	File:		Cache.h
 *
 *	Contains:	Prototype declaration of:
 *				FlushCacheRange: a machine and system independent cache flush routine
 *
 *	Cache control is a highly CPU specific function.  Although some system independence
 *	is achieved via use of the _HWPriv trap, this solution may not be general for all
 *	hardware/system software combinations.  FlushCacheRange should solve that for
 *	developers by working on all systems.
 *	
 *	Written by:	Dave Radcliffe
 *
 *	Copyright:	© 1991, 1992 by Apple Computer, Inc., all rights reserved.
 *
 *	Change History:
 *
 *		1/20/92		DR		Updated comments to match Cache.c
 *		1/10/92		DR		Updated comments to match Cache.c
 *		12/18/91	DR		New today
 *
 */
#ifndef __CACHE__
#define __CACHE__
#endif

/* Prototype declaration of FlushCacheRange.  See comments for usage. */
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
 * Note #261.
 *
 * MC68000 based systems have no cache, but if an accelerator board has been added.
 * they may have a CPU which does have a cache.  Accelerator board vendors should
 * implement _HWPriv or _CacheFlush for such systems, but if they haven't then
 * our last resort is to write directly to the cache control register (a privileged
 * instruction (GAK!!)).
 *
 * FINALLY, the full implementation of FlushCacheRange is probably overkill and may be 
 * less than optimal for some applications.  For example, testing for _HWPriv 
 * should be unnecessary on systems later than 6.0.2, so I've added conditional code
 * to allow you to bypass that if appropriate (see comment on System603OrLater).
 * The full implementation tries to cover a lot of obscure cases, but it also does 
 * some things inefficiently.  For example, having determined the _HWPriv is 
 * implemented, it would be more efficient to just keep that information around, 
 * but I wanted to avoid the complications of global or static variables.  But 
 * it is also true that if you find it necessary to call this code more than a
 * few times between the time your application starts and the time it quits, 
 * YOU ARE DOING SOMETHING WRONG!!   Go back and rethink your code.
*/
#ifdef __cplusplus
extern "C" {
#endif
void FlushCacheRange (void *address, unsigned long count);
#ifdef __cplusplus
}
#endif

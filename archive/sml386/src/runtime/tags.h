/* tags.h   (MS-Windows version)
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 * 
 * Altered 20 Dec. 1991 by:    Yngvi S. Guttesen
 *                             Department of Computer Science
 *                             The Technical University of Denmark
 *                             DK-2800 Lyngby
 *
 * This file has a corresponding ML structure tags embedded in structure Boot
 * in the file boot/perv.sml.
 */

#ifndef _TAGS_
#define _TAGS_

#define width_tags	4
#define power_tags	16
#define tag_record	1     /* 0001 */
#define tag_array	9     /* 1001 */
#define tag_bytearray	11    /* 1011 */
#define tag_string	15    /* 1111 */
#define tag_embedded	7     /* 0111 */
#define tag_suspension	13    /* 1101 */
#define tag_backptr	5     /* 0101 */
#define tag_forwarded	3     /* 0011 */
#define contains_no_ptrs(x) ((x)&2)

/* If the tag is tag_suspension, then the high-order part is NOT a length.
   Instead, it is:   0 = unevaluated suspension
                     1 = evaluated suspension
                     2 = weak pointer
		     3 = nulled weak pointer
*/


/* make an object descriptor from the length and the tag.  This
 * uses * instead of << so it will work for assemblers */
#define MAKE_DESC(l,t) ((l)*power_tags+(t))

#endif

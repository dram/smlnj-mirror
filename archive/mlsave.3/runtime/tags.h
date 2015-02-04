/* This file has a corresponding ML structure tags embedded in structure Boot
 * in the file boot/perv.sml.
 */

/* new tags */
#ifdef NEWTAGS
#define width_tags 4
#define power_tags 16
#define tag_record	1
#define tag_array	9
#define tag_bytearray	11
#define tag_string	15
#define tag_embedded	7
#define tag_closure	13
#define tag_backptr	5
#define tag_forwarded	3
#define contains_ptrs(x) ((x)&2)
#endif

/* old tags */
#ifndef NEWTAGS
#define width_tags 3
#define power_tags 8
#define tag_record	1
#define tag_array	1
#define tag_bytearray	5
#define tag_string	5
#define tag_embedded	7 /* not used */
#define tag_closure	1
#define tag_backptr	7
#define tag_forwarded	3
#define contains_ptrs(x) ((x)&4)
#endif

/* independent.  uses * instead of << so it will work in as */
#define mak_desc(l,t) ((l)*power_tags+(t))
#define ML_UNIT	    1
#define ML_NIL	    1

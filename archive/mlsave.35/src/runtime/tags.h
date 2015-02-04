/* This file has a corresponding ML structure tags embedded in structure Boot
 * in the file boot/perv.sml.
 */

/* new tags */
#define width_tags 4
#define power_tags 16
#define tag_record	1     /* 0001 */
#define tag_array	9     /* 1001 */
#define tag_bytearray	11    /* 1011 */
#define tag_string	15    /* 1111 */
#define tag_embedded	7     /* 0111 */
#define tag_closure	13    /* 1101 */
#define tag_backptr	5     /* 0101 */
#define tag_forwarded	3     /* 0011 */
#define contains_no_ptrs(x) ((x)&2)

/* independent.  uses * instead of << so it will work in as */
#define mak_desc(l,t) ((l)*power_tags+(t))

#define ML_UNIT  1
#define ML_NIL   1
#define ML_FALSE 1
#define ML_TRUE  3

#define CAUSE_GC 1
#define CAUSE_RET 2
#define CAUSE_EXN 3
#define CAUSE_EXPORT 4
#define CAUSE_BLAST 5
#define CAUSE_FAULT 6
#define CAUSE_STOR 7

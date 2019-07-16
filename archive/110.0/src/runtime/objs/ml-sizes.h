/* ml-sizes.h
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 *
 * NOTE: this file is generated --- do not edit!!!
 */

#ifndef _ML_SIZES_
#define _ML_SIZES_

#define WORD_SZB           4
#define ADDR_SZB           4
#define REALD_SZB          8
#define BITS_PER_WORD      32
#define LOG_BITS_PER_WORD  5
#define LOG_BYTES_PER_WORD 2

#define BYTE_ORDER_BIG

#ifndef _ASM_
typedef short Int16_t;
typedef unsigned short Unsigned16_t;
typedef long Int32_t;
typedef unsigned long Unsigned32_t;

typedef unsigned char Byte_t;
typedef Unsigned32_t Word_t;
typedef Int32_t      Int_t;
typedef Unsigned32_t Addr_t;
#endif

#endif /* !_ML_SIZES_ */

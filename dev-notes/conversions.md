## Integer/Word Conversions Explained

This note describes an approach to representing and optimizing
the conversions between integers (and words) of different sizes
in Standard ML.

### Background

Because different SML implementations can have different sizes
of integer and word types, the designers of the SML Basis Library
had to design a API for conversions that would work for any SML
implementation.  The approach is based on the idea that one can
convert from `IntN.int` to `IntM.int` by first converting from
`IntN.int` to `LargeInt.int` and then converting from `LargeInt.int`
to `IntM.int`.  While this approach works for source code, inside
a given compiler we want to have direct conversions.

### Primitive conversion operations

All integer/word conversion operations are expressed using five
primitive conversion operators. Algebraic equations over these
operators are easy to define and can be used to simplify composition
of conversion operations.

The five basic conversion operators are as follows (in all cases,
we assume that (n >= m):

  TEST(n,m)	-- map an n-bit, 2's complement signed value to an 
		   m-bit, 2's complement signed value; 
		   raise Overflow if the value is too large.

  TESTU(n,m)    -- map an unsigned n-bit value to an m-bit 2's 
	 	   complement value; raise Overflow if the value 
		   is too large.

  EXTEND(m,n)	-- sign extend an m-bit value to a n-bit value

  TRUNC(n,m)	-- truncate an n-bit value to an m-bit value.

  COPY(m,n)	-- copy (i.e., zero-extend) an m-bit value to an n-bit
                   value.  The operation COPY(n,n) is the identity
                   function on n-bit values.

TEST, TESTU, and TRUNC are used to go from large values to small
ones, and EXTEND and COPY are used to go from small values to
large. The operators EXTEND, TRUNC, and COPY are "pure," while TEST
and TESTU may raise Overflow.  We use `*` for m or n to denote
arbitrary precision integers (IntInf.int).

Conversions where the sizes are the same can be simplified to copies:

  TEST(n,n)     == COPY(n,n)
  EXTEND(n,n)	== COPY(n,n)	Note: this does not apply to TESTU
  TRUNC(n,n)	== COPY(n,n)

### Examples

Assuming that LargeInt is aribitrary precision and the default Int
and Word types are 31-bits, then the translation of conversion operations
in the Word32 structure is given by:

  		toLargeInt    => TESTU(32,*)		
		toLargeIntX   => EXTEND(32,*)		= COPY(32,32)
		fromLargeInt  => TESTU(*,32)		
		toInt	      => TESTU(32,31)		
		toIntX	      => TEST(32,31)		
		fromInt       => EXTEND(31,32)		
		toLargeWord   => COPY(32,32)		
		toLargeWordX  => EXTEND(32,32)		= COPY(32,32)
		fromLargeWord => TRUNC(32,32)		= COPY(32,32)

And if LargeInt was Int32, then the operations in Word8 would be

  	 	toLargeInt    => COPY(8,32)		
		toLargeIntX   => EXTEND(8,32)		
		fromLargeInt  => TRUNC(32,8)		
		toInt	      => COPY(8,31)		
		toIntX	      => EXTEND(8,31)		
		fromInt       => TRUNC(31,8)		
		toLargeWord   => COPY(8,32)		
		toLargeWordX  => EXTEND(8,32)		
		fromLargeWord => TRUNC(32,8)		

### Rewrites

These operations allow for simplification via algebraic rewrites.

Each operator composed with itself is itself, but with different parameters:

  TEST(n,m) o TEST(p,n)		== TEST(p,m)			
  TESTU(n,m) o TESTU(p,n)	== TESTU(p,m)			
  EXTEND(n,m) o EXTEND(p,n)	== EXTEND(p,m)			
  TRUNC(n,m) o TRUNC(p,n)	== TRUNC(p,m)			
  COPY(n,m) o COPY(p,n)		== COPY(p,m)			

The composition of different operators can be described by a simple algebra.

  EXTEND(n,m) o COPY(p,n)	== COPY(p,m)   if (n > p)
  				== EXTEND(p,m) if (n = p)

  COPY(n,m) o EXTEND(p,n)	== EXTEND(p,m) if (n = m)

  TRUNC(n,m) o COPY(p,n)	== COPY(p,m)   if (m >= p)
				== TRUNC(p,m)  if (m < p)

  COPY(n,m) o TRUNC(p,n)	== TRUNC(p,m)  if (n = m)

  COPY(n,m) o TEST(p,n)		== TEST(p,m)   if (n = m)

  TEST(n,m) o COPY(p,n)		== COPY(p,m)   if (m >= p)
				== TEST(p,m)   if (m < p)
	 
  TESTU(n,m) o COPY(p,n)	== COPY(p,m)   if (m >= p)
				== TESTU(p,m)  if (m < p)

  COPY(n,m) o TESTU(p,n)	== TESTU(p,m)  if (n = m)

  TRUNC(n,m) o EXTEND(p,n)	== EXTEND(p,m) if (m >= p)
				== TRUNC(p,m)  if (m < p)

  TEST(n,m) o EXTEND(p,n)	== EXTEND(p,m) if (m >= p)
				== TEST(p,m)   if (m < p)

  TESTU(n,m) o EXTEND(p,n)	== EXTEND(p,m) if (m >= p)
				== TESTU(p,m)  if (m < p)

For example, consider:
	Word.toInt o Word.fromLargeWord o Word8.toLargeWord

This translates to:
	TESTU(31,31) o TRUNC(32,31) o COPY(8,32)

and simplifies to:
	TESTU(31,31) o COPY(8,31)

This expression further simplifies to:
	COPY(8, 31)

Since both 8-bit and 31-bit quantities are tagged the same way, this
can be translated to a MOVE. With a smart register allocator that MOVE
can be eliminated.

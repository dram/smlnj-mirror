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

  `COPY`(m,n)   -- copy (i.e., zero-extend) an m-bit value to an n-bit
                   value.  The operation `COPY`(n,n) is the identity
                   function on n-bit values.

  `EXTEND`(m,n) -- sign extend an m-bit value to a n-bit value.

  `TRUNC`(n,m)  -- truncate an n-bit value to an m-bit value.

  `TEST`(n,m)   -- map an n-bit, 2's complement signed value to an
                   m-bit, 2's complement signed value;
                   raise `Overflow` if the value is too large.

  `TESTU`(n,m)  -- map an unsigned n-bit value to an m-bit 2's
                   complement value; raise `Overflow` if the value
                   is too large.

`COPY` and `EXTEND` are used to go from small values to large, while
`TRUNC`, `TEST`, and `TESTU` are are used to go from large values to
small.  The operators `COPY`, `EXTEND`, and `TRUNC` are "pure," while
`TEST` and `TESTU` may raise `Overflow`.  We use `*` for *m* or *n* to
denote arbitrary precision integers (IntInf.int).

(Note: the **SML/NJ** implementation has a second set of
primops -- `TEST_INF`, etc. -- for conversions involving `IntInf.int`)

Conversions where the sizes are the same can be simplified to copies:

  TEST(n,n)     == COPY(n,n)    Note: this rewrite does not apply to TESTU
  EXTEND(n,n)   == COPY(n,n)
  TRUNC(n,n)    == COPY(n,n)

### Examples

Assuming that LargeInt is aribitrary precision and the default Int
and Word types are 31-bits, then the translation of conversion operations
in the Word32 structure is given by:

````
                toLargeInt    => TESTU(32,*)
                toLargeIntX   => EXTEND(32,*)           = COPY(32,32)
                fromLargeInt  => TESTU(*,32)
                toInt         => TESTU(32,31)
                toIntX        => TEST(32,31)
                fromInt       => EXTEND(31,32)
                toLargeWord   => COPY(32,32)
                toLargeWordX  => EXTEND(32,32)          = COPY(32,32)
                fromLargeWord => TRUNC(32,32)           = COPY(32,32)
````

And if LargeInt was Int32, then the operations in Word8 would be

````
                toLargeInt    => COPY(8,32)
                toLargeIntX   => EXTEND(8,32)
                fromLargeInt  => TRUNC(32,8)
                toInt         => COPY(8,31)
                toIntX        => EXTEND(8,31)
                fromInt       => TRUNC(31,8)
                toLargeWord   => COPY(8,32)
                toLargeWordX  => EXTEND(8,32)
                fromLargeWord => TRUNC(32,8)
````

### Rewrites

These operations allow for simplification via algebraic rewrites.

Each operator composed with itself is itself, but with different parameters:

````
  TEST(n,p) o TEST(m,n)         == TEST(m,p)
  TESTU(n,p) o TESTU(m,n)       == TESTU(m,p)
  EXTEND(n,p) o EXTEND(m,n)     == EXTEND(m,p)
  TRUNC(n,p) o TRUNC(m,n)       == TRUNC(m,p)
  COPY(n,p) o COPY(m,n)         == COPY(m,p)
````

The composition of different operators is described by the following
simple algebra:

````
  EXTEND(n,p) o COPY(m,n)       == COPY(m,p)   if (n > m)
                                == EXTEND(m,p) if (n = m)

  TRUNC(n,p) o COPY(m,n)        == COPY(m,p)   if (p >= m)
                                == TRUNC(m,p)  if (p < m)

  TEST(n,p) o COPY(m,n)         == COPY(m,p)   if (p >= m)
                                == TEST(m,p)   if (p < m)

  TESTU(n,p) o COPY(m,n)        == COPY(m,p)   if (p >= m)
                                == TESTU(m,p)  if (p < m)

  COPY(n,p) o EXTEND(m,n)       == EXTEND(m,p) if (n = p)

  TRUNC(n,p) o EXTEND(m,n)      == EXTEND(m,p) if (p >= m)
                                == TRUNC(m,p)  if (p < m)

  TEST(n,p) o EXTEND(m,n)       == EXTEND(m,p) if (p >= m)
                                == TEST(m,p)   if (p < m)

  TESTU(n,p) o EXTEND(m,n)      == EXTEND(m,p) if (p >= m)
                                == TESTU(m,p)  if (p < m)

  COPY(n,p) o TRUNC(m,n)        == TRUNC(m,p)  if (n = p)

  COPY(n,p) o TEST(m,n)         == TEST(m,p)   if (n = p)

  COPY(n,p) o TESTU(m,n)        == TESTU(m,p)  if (n = p)
````

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

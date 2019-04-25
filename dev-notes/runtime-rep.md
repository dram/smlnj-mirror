# SML/NJ Runtime Representations

This document describes the runtime representations used in the **SML/NJ**
system.

## Boxed vs. Unboxed Values

SML values have a *uniform* representation of a single machine word.
The lowest bit of the word is used to distinguish between values
that are represented by pointers and values that are represented
as immediate integers.

For words in the heap, we actually define the two lowest bits as the
**tag** bits, which have the following interpretation:

* `00` -- boxed object
* `x1` -- unboxed (tagged) integer
* `10` -- object descriptor

## Heap Objects

### Arrays and Vectors

Array and vector values are represented by a header object that contains
a pointer to the underlying data object and the length of the object (in
elements).

### Pairs

Pairs are allocated as records of length two, but the descriptor word is
stripped when the object is promoted to the first generation.  The garbage
collector uses a BIBOP scheme to identify headless pairs.

## Object Descriptors

With the exception of pairs in the Pair Arena, every object in the heap has
a one-word descriptor (or header) word that immediately precedes the first
word of the object.

On object descriptor word is partitioned into three fixed-size fields.
The lowest two bits (*i.e.*, the tag bits) are always `10`.  The next five
bits are the **descriptor-tag**, which specifies the basic kind of object.
The remaining bits are the **length** field; for most kinds of objects, the
length is the number of words in the object, but in some cases it has a
different interpretation.

The descriptor tags are as follows:

* `00000` (`0x00`) -- record/tuple object; also used for polymorphic vector data.
* `00001` (`0x01`) -- vector header object; the length field specifies the kind of vector.
* `00010` (`0x02`) -- array header; the length field specifies the kind of array.
* `00011` (`0x03`) -- polymorphic array data; also used for reference cells.
* `00100` (`0x04`) -- 32-bit aligned non-pointer data
* `00101` (`0x05`) -- 64-bit aligned non-pointer data
* `00110` (`0x06`) -- special object; length field specifies kind and state of object.
* `00111` (`0x07`) --
* `01000` (`0x08`) --
* `01001` (`0x09`) --
* `01010` (`0x0A`) --
* `01011` (`0x0B`) --
* `01100` (`0x0C`) --
* `01101` (`0x0D`) --
* `01110` (`0x0E`) --
* `01111` (`0x0F`) --
* `10000` (`0x10`) -- external symbol reference; used in exported heap images
* `10001` (`0x11`) --
* `10010` (`0x12`) --
* `10011` (`0x13`) --
* `10100` (`0x14`) --
* `10101` (`0x15`) --
* `10110` (`0x16`) --
* `10111` (`0x17`) --
* `11000` (`0x18`) --
* `11001` (`0x19`) --
* `11010` (`0x1A`) --
* `11011` (`0x1B`) --
* `11100` (`0x1C`) --
* `11101` (`0x1D`) --
* `11110` (`0x1E`) --
* `11111` (`0x1F`) --  forward pointer; used during garbage collection

### Sequence Kinds

For vector and array headers, we
* `0x0` -- polymorphic sequence
* `0x1` -- 8-bit integer/word sequence
* `0x2` -- 16-bit integer/word sequence (unused)
* `0x3` -- 31-bit integer/word sequence (unused)
* `0x4` -- 32-bit integer/word sequence (unused)
* `0x5` -- 32-bit floating-point sequence (unused)
* `0x6` -- 64-bit floating-point sequence

We need to add a 64-bit integer kind, which might involve shifting the real
sequence tags.  Perhaps we should change 31-bit integers to 32-bit and 32-bit
to 64-bits?  We may not need a different kind for sequences of tagged integers.

### Special-Object Kinds

* `0x0` -- evaluated suspension
* `0x1` -- unevaluated suspension
* `0x2` -- weak pointer
* `0x3` -- nullified weak pointer

## 32-bit Specifics

For 32-bit systems, the 64-bit aligned data (which is used to represent reals
and packed sequences of reals) requires aligning the allocation pointer before
allocating the object.

## 64-bit Specifics

Unclear what we should do about 32-bit aligned non-pointer data objects.  Is there
a need for them in the 64-bit world?

## Possible Improvements

* reduce the number of descriptor-tag bits to four, since more than half of them
  are currently unused.  Or we could use the extra bits to encode headers for
  small mixed objects of fixed length.

* add vector and array-slice headers.  These would be four-word records, consisting
  of data pointer, starting index, length, and length of underlying array/vector.

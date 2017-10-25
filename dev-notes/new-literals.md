# New Literal Representation

This document describes a new bytecode for generating heap-allocated
literals in the SML/NJ compiler.  The main reason for introducing a
new bytecode is to prepare for future compiler improvements (64-bit
support, `Real32`, and better `Int64` and `IntInf` integration).

## Endianess

Multiple byte quantities are represented in big-endian form (most-significant
byte first).

## Header

The first four 32-bit words of the literal representation correspond to the
following **C** struct:

````C
struct literal_header {
    uint32_t    magic;
    uint32_t    maxstk;
    uint32_t    wordsz;
    uint32_t    maxsaved;
};
````

where

* `magic` contains the version ID (which should be `0x20171031`)

* `maxstk` is the maximum stack depth required, and

* `wordsz` is the size of an ML value (32 or 64)

* `maxsaved` is the number of saved literals (used for sharing)

Note that Version 1 files will have the version ID `0x19981022` and have
the first two header fields, but not the `wordsz` or `numsaved` fields.

## Opcodes

The following is a list of the symbolic opcodes used in the interpreter.
We describe the instruction encoding below.

* **INT**(*n*) literal value in the default (tagged) integer or
    word type (`Int.int` or `Word.word`).  The value `n` should be
    in the range -2^*w*-1^ to 2^*w*^-1 when encoded as a w-bit 2's complement
    integer.  The width *w* will be 31 or 63 depending on the host
    architecture.

* **INT32**(*n*) 32-bit literal value for either the type `Int32.int` or `Word32.int`.

* **INT64**(*n*) 64-bit literal value for either the type `Int32.int` or `Word32.int`.

* **BIGINT**(*n*) arbitrary precision integer literal (currently not used).

* **IVEC8**(*n*, *b~1~*, ..., *b~n~*) packed vector of 8-bit integers for either the type
    `Int8Vector.vector` or `Word8Vector.vector`.

* **IVEC16**(*n*, *h~1~*, ..., *h~n~*) packed vector of 16-bit integers for either the type
    `Int16Vector.vector` or `Word16Vector.vector` (currently not used).

* **IVEC32**(*n*, *w~1~*, ..., *w~n~*) packed vector of 32-bit integers for either the type
    `Int32Vector.vector` or `Word32Vector.vector` (currently not used).

* **IVEC64**(*n*, *d~1~*, ..., *d~n~*) packed vector of 64-bit integers for either the type
    `Int64Vector.vector` or `Word64Vector.vector` (currently not used).

* **REAL32**(*f*) 32-bit floating-point literal for the type `Real32.real`
    (currently not used).

* **REAL64**(*f*) 64-bit floating-point literal for the type `Real32.real`.

* **RVEC32**(*n*, *f~1~*, ..., *f~n~*) packed vector of 32-bit floating-point literals
     for the type `Real32Vector.vector` (currently not used).

* **RVEC64**(*n*, *F~1~*, ..., *F~n~*) packed vector of 64-bit floating-point literals
     for the type `Real32Vector.vector`.

* **STR8**(s) string literal (8-bit characters)

* **RECORD**(n) construct record from the topmost n literal values

* **VECTOR**(n) construct a vector from the topmost n literal values

* **CONCAT**(n) pop *n* records/vectors from the stack and concatenate them
    into a single record/vector.  This operation allows the implementation
    to avoid excessively large stacks when building very large record/vector
    literals.

* **SAVE**(i) save the top of the stack in the i^th^ save slot, which allows it
    to be shared by some subsequent aggregate literal.

* **LOAD**(i) push the i^th^ saved literal onto the stack.

* **RETURN**
    signals the end of the program; the stack depth should be one and that value
    is popped and returns as the result.

### Future extensions

There are a number of additional features that we might want to support, which we
list here.

* support for 32-bit string literals for the type `WideString.string`

* support for array literals (like vectors, but mutable)


## Instruction encoding

### Notation
In the encoding below, we use the following conventions:

* *b* represents a signed 8-bit integer.

* *ub* represents an unsigned 8-bit integer.

* *c* represents a 8-bit character.

* *h* represents a signed 16-bit integer.

* *w* represents a signed 32-bit integer.

* *lw* represents a signed 64-bit integer.

* *n* represents a 32-bit integer length (usually unsigned).

* *d* represents a bignum digit whose size will be the default word size.

* *f* represents a 32-bit floating-point literal.

* *F* represents a 64-bit floating-point literal.

* *i* represents a tagged default int or word literal (*e.g.*, `Int.int` or
    `Word.word`).

### Encoding

* `00000000` (`0x00`) <br />
    **INT**(0) <br />
    default tagged literal value 0.

* `00000001` (`0x01`) <br />
    **INT**(1) <br />
    default tagged literal value 1.

* `00000010` (`0x02`) <br />
    **INT**(2) <br />
    default tagged literal value 2.

* `00000011` (`0x03`) <br />
    **INT**(3) <br />
    default tagged literal value 3.

* `00000100` (`0x04`) <br />
    **INT**(4)
    default tagged literal value 4.

* `00000101` (`0x05`) <br />
    **INT**(5)
    default tagged literal value 5.

* `00000110` (`0x06`) <br />
    **INT**(6)
    default tagged literal value 6.

* `00000111` (`0x07`) <br />
    **INT**(7)
    default tagged literal value 7.

* `00001000` (`0x08`) <br />
    **INT**(8)
    default tagged literal value 8.

* `00001001` (`0x09`) <br />
    **INT**(9)
    default tagged literal value 9.

* `00001010` (`0x0A`) <br />
    **INT**(10)
    default tagged literal value 10.

* `00001011` (`0x0B`) <br />
    **INT**(-1)
    default tagged literal value -1.

* `00001100` (`0x0C`) <br />
    **INT**(-2)
    default tagged literal value -2.

* `00001101` (`0x0D`) <br />
    **INT**(-3)
    default tagged literal value -3.

* `00001110` (`0x0E`) <br />
    **INT**(-4)
    default tagged literal value -4.

* `00001111` (`0x0F`) <br />
    **INT**(-5)
    default tagged literal value -5.

* `00010000` (`0x10` *b*) <br />
    **INT**(*b*) --- for tagged integer literals in the range -128..127.

* `00010001` (`0x11` *h*) <br />
    **INT**(*h*) --- for tagged integer literals in the range -32768..32767.

* `00010010` (`0x12` *w*) <br />
    **INT**(*w*) --- for tagged integer literals in the range -2147483648..2147483647.

* `00010011` (`0x13` *lw*) <br />
    **INT**(*lw*) --- for all other tagged integer literals (64-bit target only).

* `00010100` (`0x14` *b*) <br />
    **INT32**(*b*) --- for 32-bit integer literals in the range -128..127.

* `00010101` (`0x15` *h*) <br />
    **INT32**(*h*) --- for 32-bit integer literals in the range -32768..32767.

* `00010110` (`0x16` *w*) <br />
    **INT32**(*w*) --- for all other 32-bit integer literals.

* `00010111` (`0x17` *b*) <br />
    **INT64**(*b*) --- for 64-bit integer literals in the range -128..127.

* `00011000` (`0x18` *h*) <br />
    **INT64**(*h*) --- for 64-bit integer literals in the range -64768..64767.

* `00011001` (`0x19` *w*) <br />
    **INT64**(*w*) --- for 64-bit integer literals in the range -2147483648..2147483647.

* `00011010` (`0x1A` *lw*) <br />
    **INT64**(*lw*) --- for all other 64-bit integer literals.

* `00011011` (`0x1B` *n* *d~1~* ... d~|n|~) <br />
    **BIGINT**(*i*) --- where *i* = **sign**(*n*) *b*^|n|-1^ d~|n|~ ... d~1~.
    *I.e.*, the absolute value of *n* is the number of digits, where is *n* is
    negative, then *i* is negative.  The digits follow *n* in least-significant
    to most-significant order.  If *n* is zero, the *i* is zero.  The base *b* and
    size of the digits will depend on the target word size.

* `00011100` (`0x1C` *ub* *i~1~* ... *i~ub~*) <br />
    **IVEC**(*ub*, *i~1~*, ..., *i~ub~*) --- short int vector (up to 255 elements).

* `00011101` (`0x1D` *n* *i~1~* ... *i~n~*) <br />
    **IVEC**(*ub*, *i~1~*, ..., *i~n~*)

* `00011110` (`0x1E` *ub* *b~1~* ... *b~ub~*) <br />
    **IVEC8**(*ub*, *b~1~*, ..., *b~ub~*) --- short bytevectors (up to 255 elements).

* `00011111` (`0x1F` *n* *b~1~* ... *b~n~*) <br />
    **IVEC8**(*n*, *b~1~*, ..., *b~n~*)

* `00100000` (`0x20` *ub* *h~1~* ... *h~ub~*) <br />
    **IVEC16**(*ub*, *h~1~*, ..., *h~ub~*) --- short 16-bit integer vectors (up to 255 elements).

* `00100001` (`0x21` *n* *h~1~* ... *h~n~*) <br />
    **IVEC16**(*n*, *h~1~*, ..., *h~n~*)

* `00100010` (`0x22` *ub* *w~1~* ... *w~ub~*) <br />
    **IVEC32**(*ub*, *w~1~*, ..., *w~ub~*) --- short 32-bit integer vectors (up to 255 elements).

* `00100011` (`0x23` *n* *w~1~* ... *w~n~*) <br />
    **IVEC32**(*n*, *w~1~*, ..., *w~n~*)

* `00100100` (`0x24` *ub* *lw~1~* ... *lw~ub~*) <br />
    **IVEC64**(*ub*, *lw~1~*, ..., *lw~ub~*) --- short 64-bit integer vectors (up to 255 elements).

* `00100101` (`0x25` *n* *lw~1~* ... *lw~n~*) <br />
    **IVEC64**(*n*, *lw~1~*, ..., *lw~n~*)

* `00100110` (`0x26` *f*) <br />
    **REAL32**(*f*)

* `00100111` (`0x27` *F*) <br />
    **REAL64**(*F*)

* `00101000` (`0x28` *ub* *f~1~* ... *f~ub~*) <br />
    **RVEC32**(*ub*, *f~1~*, ..., *f~ub~*) --- short 32-bit real vectors (up to 255 elements).

* `00101001` (`0x29` *n* *f~1~* ... *f~n~*) <br />
    **RVEC32**(*n*, *f~1~*, ..., *f~n~*)

* `00101010` (`0x2A` *ub* *F~1~* ... *F~ub~*) <br />
    **RVEC64**(*ub*, *F~1~*, ..., *F~ub~*) --- short 64-bit real vectors (up to 255 elements).

* `00101011` (`0x2B` *n* *F~1~* ... *F~n~*) <br />
    **RVEC64**(*n*, *F~1~*, ..., *F~n~*)

* `00101100` (`0x2C` *ub* *c~1~* ... *c~ub~*) <br />
    **STR8**(*s*) --- where **size**(*s*) = *ub* and *c~1~*, ..., *c~ub~* are the
    characters of *s*.

* `00101101` (`0x2D` *n* *c~1~* ... *c~n~*) <br />
    **STR8**(*s*) --- where **size**(*s*) = *n* and *c~1~*, ..., *c~n~* are the
    characters of *s*.

* `00101110` (`0x2E`) <br />
    *reserved for STR32*

* `00101111` (`0x2F`) <br />
    *reserved for STR32*

* `00110000` (`0x30`) <br />
    **RECORD**(1)

* `00110001` (`0x31`) <br />
    **RECORD**(2)

* `00110010` (`0x32`) <br />
    **RECORD**(3)

* `00110011` (`0x33`) <br />
    **RECORD**(4)

* `00110100` (`0x34`) <br />
    **RECORD**(5)

* `00110101` (`0x35`) <br />
    **RECORD**(6)

* `00110101` (`0x36`) <br />
    **RECORD**(7)

* `00110101` (`0x37` *ub*) <br />
    **RECORD**(*ub*)

* `00110101` (`0x38` *h*) <br />
    **RECORD**(*h*)

* `00110101` (`0x39` *ub*) <br />
    **VECTOR**(*ub*)

* `00110101` (`0x3A` *h*) <br />
    **VECTOR**(*h*)

* `00110101` (`0x3B` *h*) <br />
    **CONCAT**(*h*)

* `00110101` (`0x3C` *ub*) <br />
    **SAVE**(*ub*)

* `00110101` (`0x3D` *h*) <br />
    **SAVE**(*h*)

* `00110101` (`0x3E` *ub*) <br />
    **LOAD**(*ub*)

* `00111111` (`0x3F` *h*) <br />
    **LOAD**(*h*)

* `01000000` -- `11111110` (`0x40` -- `0xFE`) <br />
    *unused*

* `11111111` (`0xFF`) <br />
    **RETURN**

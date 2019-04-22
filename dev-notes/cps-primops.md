# CPS Primitive Operators

This document describes the CPS primitive operators that are defined in the
`CPS.P` structure.

## Representing type information

CPS types (`cty`) have the following representation:

````sml
datatype cty
  = NUMt of intty       (* integers of the given type *)
  | PTRt of pkind       (* pointer *)
  | FUNt                (* function? *)
  | FLTt of int         (* float of given size *)
  | CNTt                (* continuation *)
````

The primitive type structure distinguishes between three different
kinds of numbers:
````
datatype numkind = INT of int | UINT of int | FLOAT of int
````

## Conditional branches

The `branch` datatype represents the various comparisons and other conditional
tests.

  * `CMP of {oper: cmpop, kind: numkind}`
    Signed and unsigned integer comparison operations; the `oper` field specifies
    the comparison operator (described below) and the `kind` field specifies the
    kind of the arguments (cannot be `FLOAT`). <br/>
    The comparison operators are as follows; their semantics of the first four
    operators depends on the `kind` field of the `CMP` constructor:
      - `GT` -- greater than
      - `GTE` -- greater than or equal
      - `LT` -- less than
      - `LTE` -- less than or equal
      - `EQL` -- equal
      - `NEQ` -- not equal

  * `FCMP of {oper: fcmpop, size: int}`
    Floating-point comparisons; the `oper` field specifies the comparison operator (described
    below) and the `size` field specifies the size of the arguments. <br/>
    The comparison operations (`datatype fcmpop`) follow the IEEE-754 semantics
    and are as follows:
      - `F_EQ` -- equal (`=`)
      - `F_ULG` -- unordered, less than, or greater than (`?<>`)
      - `F_UN` -- unordered (`?`)
      - `F_LEG` -- ordered (`<=>`)
      - `F_GT` -- greater than (`>`)
      - `F_GE` -- greater than or equal (`>=`)
      - `F_UGT` -- unordered or greater than (`?>`)
      - `F_UGE` -- unordered, greater than, or equal (`?>=`)
      - `F_LT` -- less than (`<`)
      - `F_LE` -- less than or equal (`<=`)
      - `F_ULT` -- unordered or less than (`?<`)
      - `F_ULE` -- unordered, less than, or equal (`?<=`)
      - `F_LG` -- less than or greater than (`<>`)
      - `F_UE` -- unordered or equal (`?=`)
      - `F_SGN` -- true if the sign bit of its argument is set

  * `BOXED` -- true if its argument is a pointer; *i.e.*, the lowest bit of
    the argument is 0.

  * `UNBOXED` -- true if its argument is a tagged integer; *i.e.*, the lowest
     bit of the argument is 1.

  * `PEQL` -- equality test for pointers

  * `PNEQ` -- inequality test for pointers

  * `STREQL` -- string equality test.  This operations is use to implement pattern matching
    against string literals and expects three arguments: *n*, *a*, and *b*, where *n*
    is the length of the strings *a* and *b*.

  * `STRNEQ` -- string inequality test.  (See `STREQL` for more discussion)


## Memory write operations

The `setter` type describes operations that write to memory.

  * `NUMUPDATE of {kind: numkind}` -- array update for a packed monomorphic
    array of numbers.  This operation first fetches the data pointer for the
    sequence and uses it to implement the update.

  * `UNBOXEDUPDATE` -- array update for a polymorphic array, where the argument
    is known to be unboxed, thus no store-list entry is created.

  * `UPDATE` -- array update for a polymorphic array; a store-list entry will be created.

  * `UNBOXEDASSIGN` -- assignment to a reference cell, where the argument is known to be
    unboxed, thus no store-list entry is created.

  * `ASSIGN` -- assignment to a reference cell; a store-list entry will be created.

  * `SETHDLR` -- assign a continuation into the global exception-handler reference.

  * `SETVAR` -- assign a value into the global *var* register.

  * `SETSPECIAL` -- sets the kind field of a special-object header; the kind is
    assumed to be a tagged integer.

  * `RAWSTORE of {kind: numkind}` -- raw-memory store operation; used by the `RawMemInlineT`
    structure (`system/smlnj/init/rawmem.sml`) to support direct C-function calls.

  * `RAWUPDATE of cty` -- update to raw record; used in spill phase (`cpscompile/spill-new.sml`)
    to initialize the raw records defined for spill records.


## Memory read operations

The `looker` datatype describes operations that read from mutable memory.

  * `DEREF` -- dereference a reference cell.

  * `SUBSCRIPT` -- fetch a value from a polymorphic array.  This operation first fetches
    the data pointer for the sequence and then the item from the data object.

  * `NUMSUBSCRIPT of {kind: numkind}` -- fetch a value from a packed monomorphic
    array of numbers.  This operation first fetches the data pointer for the
    sequence and uses it to implement the update.

  * `GETSPECIAL` -- get the kind field of a special object as a tagged integer value.

  * `GETHDLR` -- get the current exception-handler contunuation from the global
    exception-handler reference.

  * `GETVAR` -- get the contents of the global *var* register.

  * `RAWLOAD of {kind: numkind}` -- raw-memory store operation; used by the `RawMemInlineT`
    structure (`system/smlnj/init/rawmem.sml`) to support direct C-function calls.
    This primop is used both with one and two arguments; in the latter case, the source
    memory address is computed by adding the two arguments.

## Artithmetic operations

  The `arithop` datatype is used by both the `ARITH` and `PURE_ARITH` constructors in
  the `pure` and `arith` datatypes (resp.) to specify the arithmetic operation.

  * `ADD` -- addition; used for signed (`ARITH`) and unsigned (`PURE_ARITH`) integer
    addition, and for floating-point addition (`PURE_ARITH`).

  * `SUB` -- subtraction; used for signed (`ARITH`) and unsigned (`PURE_ARITH`) integer
    subtraction, and for floating-point subtraction (`PURE_ARITH`).

  * `MUL` -- multiplication; used for signed (`ARITH`) and unsigned (`PURE_ARITH`) integer
    multiplication, and for floating-point multiplication (`PURE_ARITH`).

  * `DIV` -- signed integer division (`ARITH` only) with rounding toward negative
    infinity.  Note that an explicit check for divide by zero is added
    during the translation from Absyn to FLINT.

  * `MOD` -- signed integer remainder (`ARITH` only) with rounding toward negative
    infinity.  Note that an explicit check for divide by zero is added
    during the translation from Absyn to FLINT.

  * `QUOT` -- signed (`ARITH`) and unsigned (`PURE_ARITH`) integer division with
    rounding toward zero.  Note that an explicit check for divide by zero is added
    during the translation from Absyn to FLINT.

  * `REM` -- signed (`ARITH`) and unsigned (`PURE_ARITH`) integer remainder with
    rounding toward zero.  Note that an explicit check for divide by zero is added
    during the translation from Absyn to FLINT.

  * `FDIV` -- floating-point division (`PURE_ARITH` only)

  * `LSHIFT` -- integer left-shift operation (`PURE_ARITH` only)

  * `RSHIFT` -- integer right-shift operation (`PURE_ARITH` only) with sign propagation
    (*a.k.a.* arithemetic right shift).

  * `RSHIFTL` -- integer right-shift operation (`PURE_ARITH` only) with zero propagation
    (*a.k.a.* logical right shift).

  * `ANDB` -- bitwise-and operation (`PURE_ARITH` only).

  * `ORB` -- bitwise-or operation (`PURE_ARITH` only).

  * `XORB` -- bitwise-exclusive-or operation (`PURE_ARITH` only).

  * `NEG` -- arithmetic negation; used for signed (`ARITH`) and unsigned (`PURE_ARITH`) integer
    multiplication, and for floating-point multiplication (`PURE_ARITH`).

  * `ABS` -- floating-point absolute value (`PURE_ARITH` only).

  * `NOTB` -- bitwise logical-negation operation (`PURE_ARITH` only).

  * `FSQRT` -- floating-point square-root operation (`PURE_ARITH` only).  This operation is
    in only used on systems that have hardware support for the operation.

  * `FSIN` -- floating-point sine operation (`PURE_ARITH` only).  This operation is
    in only used on systems that have hardware support for the operation.

  * `FCOS` -- floating-point cosine operation (`PURE_ARITH` only).  This operation is
    in only used on systems that have hardware support for the operation.

  * `FTAN` -- floating-point tangent operation (`PURE_ARITH` only).  This operation is
    in only used on systems that have hardware support for the operation.

## Impure arithmentic operations

The `arith` datatype represents arithmetic operations that can cause an `Overflow`
exception.

  * `ARITH of {oper: arithop, kind: numkind}` -- arithmetic operations that may raise the
    `Overflow` exception (note that division by zero is explicitly checked for by code
    that is added during the translation from Absyn to FLINT).

  * `TEST of {from: int, to: int}` -- checked conversion from larger signed to smaller
    signed integer representation; the `from` field is the size of the input and
    the `to` field is the size of the result.

  * `TESTU of {from: int, to: int}` -- checked conversion from larger unsigned to smaller
    signed integer representation; the `from` field is the size of the input and
    the `to` field is the size of the result.

  * `TEST_INF of int` -- checked conversion from arbitrary-precision signed integer
    to a fixed-size signed integer representation; the argunment is the size of the result.

  * `ROUND of {floor: bool, from: numkind, to: numkind}` -- conversion from floating-point
    to signed-integer representation.  If the `floor` field is true, then the conversion
    returns the floor of the argument, otherwise it rounds the argument.

## Pure operations

The `pure` datatype collects together operations that cannot cause an exception.

  * `PURE_ARITH of {oper: arithop, kind: numkind}`
    arithmetic operations that will **not** raise `Overflow` or `Div` exceptions.

  * `PURE_NUMSUBSCRIPT of {kind: numkind}` -- subscript operation on a monomorphic
    packed vector of numbers (currently `INT 8` is the only kind that is supported).
    This operation first fetches the data pointer for the sequence and then the item
    from the data object.

  * `LENGTH` -- fetch the length component of an array or vector

  * `OBJLENGTH` -- fetch the length field of an object's descriptor and convert
    it to a tagged integer.

  * `MAKEREF` -- allocate a reference cell

  * `COPY of {from: int, to: int}` -- zero-extend an integer value; the `from` field
    is the size of the input and the `to` field is the size of the result.

  * `EXTEND of {from: int, to: int}` -- sign-extend an integer value; the `from` field
    is the size of the input and the `to` field is the size of the result.

  * `TRUNC of {from: int, to: int}` -- truncate an integer value; the `from` field
    is the size of the input and the `to` field is the size of the result.

  * `COPY_INF of int` -- zero extend a fixed-sized integer to an arbitrary-precision
    integer; the argument is the size of the input. <br />
    Note that this primop is replaced with a function call by the `IntInfCnv.elim`
    function during CPS optimization.

  * `EXTEND_INF of int` -- sign extend a fixed-sized integer to an arbitrary-precision
    integer; the argument is the size of the input. <br />
    Note that this primop is replaced with a function call by the `IntInfCnv.elim`
    function during CPS optimization.

  * `TRUNC_INF of int` -- truncate an arbitrary-precision integer to a fixed-sized
    integer; the argument is the size of the result. <br />
    Note that this primop is replaced with a function call by the `IntInfCnv.elim`
    function during CPS optimization.

  * `REAL of {from: numkind, to: numkind}` -- conversion from signed integer types
    to floating-point types.

  * `SUBSCRIPTV` -- fetch a value from a polymorphic vector.  This operation first fetches
    the data pointer for the sequence and then the item from the data object.

  * `GETTAG` -- gets the tag field of the object's header word as an tagged integer.

  * `MKSPECIAL` -- makes a special object from a tagged integer that specifies the
    kind and a value.

  * `CAST` -- no-op copy used to implement type casts.

  * `GETCON` -- gets the tag field (at offset 0) of an data-constructor value.

  * `GETEXN` -- gets the unique exception ID (at offset 0) from an exception value.

  * `BOX` -- appears to be a no-op that has the effect of converting the GC type
    of the argument from an untagged integer (`SMLGCType.INT`) to a pointer
    (`SMLGCType.PTR`).

  * `UNBOX` -- appears to be a no-op that has the effect of converting the GC type
    of the argument from a pointer (`SMLGCType.PTR`) to an untagged integer
    (`SMLGCType.INT`).

  * `WRAP of numkind` -- conversion from unboxed to boxed representation of
    numbers.  Currently works for floating-point and word-sized integers.

  * `UNWRAP of numkind` -- conversion from boxed to unboxed representation of
    numbers.  Currently works for floating-point and word-sized integers.

  * `GETSEQDATA` -- gets the data pointer of an array or vector value.

  * `RECSUBSCRIPT` -- subscript operation on records

  * `RAW64SUBSCRIPT` -- direct load of 64-bit floating-point value from packed
    sequence (*i.e.*, there is no fetching of the data pointer from a header object).

  * `NEWARRAY0` -- allocates a polymorphic array of length 0.

  * `RAWRECORD of record_kind option` -- allocates a block of heap memory.  If the argument
    is `NONE`, then this memory has no header (this form is used by CPS spilling); otherwise
    the header is determined from the argument.  Currently, only `RK_I32BLOCK` and
    `RK_FBLOCK` kinds are used.

## Ideas for improvement

  * move the `F_SGN` operator (which is unary) from the `fcmp` datatype to the `branch`
    datatype

  * replace the `STREQL` and `STRNEQ` branch operators with a pure operation `STRCMP` that
    has semantics like `strncmp` function in **C**.  This operation would allow more
    efficient binary searches to implement pattern matching over string literals.
    Note that if it does word-at-a-time comparisons, then its semantics will depend
    on the endianess of the target architecture.

  * expose the representation of arrays and vectors to CPS so that CSE can be used to
    eliminate redundant memory operations.  This should also simplify the code
    generator a bit, since many operations have to fetch the data pointer.  It could
    also make substrings and slices more efficient.

  * The `arithop` datatype could be made more precise by splitting it into pure and
    impure types.  Of the 21 constructors, 13 are always pure, two are always impure,
    and only six can appear as arguments to either `ARITH` or `PURE_ARITH`.  We might
    also want to make the representation of primitive operations more uniform throughout
    the phases of the compiler to streamline translation. <br/>
    Note: code generation for impure floating-point operations in `mlriscGen.sml`
    has been commented out, since it does not appear to be needed.

  * Replace `RAW64SUBSCRIPT` with a `RAWSUBSCRIPT of numkind` to support packed
    sequences of other sizes and types.


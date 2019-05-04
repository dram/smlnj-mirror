# SML/NJ Primitive Operations

This document describes the primitive operators (primops) that the
compiler exposes.  These are used to define the `InlineT` structure,
which, in turn is used in the implementation of the Basis Library.
With the addition of 64-bit targets, the mapping from primop to
internal representation becomes target-specific in many cases.


## Relavant source files

  * `compiler/ElabData/prim/primop.sml`<br/>
    this file defines the `Primop` structure, which includes the various
    datatypes used to represent primitive operations internally in the
    front-end of the compiler.  The main type is `Primop.primop`.

  * `compiler/ElabData/prim/primop.sig`<br/>
    this file defines the `PRIMOP` signature use for the `Primop` structure.

  * `compiler/Semant/prim/primop-bindings.sml`<br/>
    this file defines the bindings between the SML variables
    exposed by the compiler and the internal `Primop.primop`
    type.

  * `system/smlnj/init/built-in32.sml`<br/>
    this file defines the `InlineT` structure for 32-bit targets


## Naming conventions

Operations that "belong" to a specific type (*e.g.*, addition) have an initial
prefix that specifies the type as follows:

  * "`int`" -- default tagged integer type (*i.e.*, either `Int31.int` or `Int63.int`)
  * "`word`" -- default tagged word type (*i.e.*, either `Word31.word` or `Word63.word`)
  * "`int32`" -- 32-bit integers
  * "`word32`" -- 32-bit words
  * "`int64`" -- 64-bit integers
  * "`word64`" -- 64-bit words
  * "`intinf`" -- arbitrary precision integers
  * "`real32`" -- 32-bit real numbers (not yet supported)
  * "`real64`" -- 64-bit real numbers
  * "`ptr`" -- machine address
  * "`arr`" -- polymorphic arrays
  * "`vec`" -- polymorphic vectors
  * "`seq`" -- sequence types (arrays and vectors)
  * "`word8_arr`" -- used for arrays of `Word8.word`
  * "`word8_vec`" -- vectors of `Word8.word`
  * "`char_arr`" -- arrays of `char`
  * "`char_vec`" -- vectors of `char` (*i.e.*, strings)
  * "`real64_arr`" -- arrays of `Real64.real`

Following the type prefix may one or more attributes, which highlight properties
of the operation.

We use the attribute "`raw`" to denote direct machine operations that are not
directly accesible in the Basis Library (*e.g.*, shift operations, where the basis
versions clamp the shift amount to the word size, but the raw versions do not).

We use the attribute "`unsafe`" for operations that could potentially result in
a crash (*e.g.*, array subscript operations that do not check the index against
the array bounds).

## Primitive operators

### Size-independent primops

#### Continuation operators
  * `callcc : ('a cont -> 'a) -> 'a`<br/>
    `P.CALLCC`

  * `throw : 'a cont -> 'a -> 'b`<br/>
    `P.THROW`

  * `capture : ('a control_cont -> 'a) -> 'a`<br/>
    `P.CAPTURE`

  * `isolate : ('a -> unit) -> 'a cont`<br/>
    `P.ISOLATE`

  * `cthrow : 'a control_cont -> 'a -> 'b`<br/>
    `P.THROW`


#### Reference operations
  * `! : 'a ref -> 'a`<br/>
    `P.DEREF`

  * `:= : 'a ref * 'a -> unit`<br/>
    `P.ASSIGN`

  * `makeref : 'a ref * 'a -> unit`<br/>
    `P.MAKEREF`


#### Boxity tests
  * `boxed : 'a -> bool`<br/>
    `P.BOXED`

  * `unboxed : 'a -> bool`<br/>
    `P.UNBOXED`


#### Type cast
  * `cast : 'a -> 'b`<br/>
    `P.CAST`


#### Equality tests
  * `= : ''a * ''a -> bool`<br/>
    Polymorphic equality.<br/>
    `P.POLYEQL`

  * `<> : ''a * ''a -> bool`<br/>
    Polymorphic inequality.<br/>
    `P.POLYNEQ`

  * `ptr_eql : 'a * 'a -> bool`<br/>
    Pointer equality.<br/>
    `P.PTREQL`

  * `ptr_neq : 'a * 'a -> bool`<br/>
    Pointer inequality.<br/>
    `P.PTRNEQ`


#### Runtime hooks
  * `getvar : unit -> 'a`<br/>
    `P.GETVAR`

  * `setvar : 'a -> unit`<br/>
    `P.SETVAR`

  * `mkspecial : int * 'a -> 'b`<br/>
    `P.MKSPECIAL`

  * `getspecial : 'a -> int`<br/>
    `P.GETSPECIAL`

  * `setspecial : 'a * int -> unit`<br/>
    `P.SETSPECIAL`

  * `gethdlr : unit -> 'a cont`<br/>
    `P.GETHDLR`

  * `sethdlr : 'a cont -> unit`<br/>
    `P.SETHDLR`

  * `gettag : 'a -> int`<br/>
    `P.GETTAG`

  * `objlength : 'a -> int`<br/>
    extracts the length field from an object's header word.<br/>
    `P.OBJLENGTH`


#### Inline operations
These primops are Basis Library functions that should be inlined for efficiency.
  * `inl_compose : ('b -> 'c) * ('a -> 'b) -> 'a -> 'c`<br/>
    Inline function composition.<br/>
    `P.INLCOMPOSE`

  * `inl_before : 'a * 'b -> 'a`<br/>
    Inline `before` operator.<br/>
    `P.INLBEFORE`

  * `inl_ignore : 'a -> unit`<br/>
    Inline `ignore` function.<br/>
    `P.INLIGNORE`

  * `inl_identity : 'a -> 'a`<br/>
    Inline identity function.<br/>
    `P.INLIDENTITY`

  * `inl_bool_not : bool -> bool`<br/>
    Inline boolean negation operator.
    `P.INLNOT`

  * `inl_chr : int -> char`<br/>
    Inline `chr` function.<br/>
     `P.INLCHR`

  * `inl_ord : char -> int`<br/>
    Inline `ord` function.<br/>
     `P.CAST`

Some additional candidates for inlined operations include `hd`, `tl`, and `null`.

If the compiler had the `option` and `order` datatypes builtin (like `bool` and `list`),
then `valOf`, `isSome`, `isNone` and some of the `compare` functions could also
be inlined.

In the long run, however, a better way to support inlining library functions would
be through a reliable cross-module inliner.

### Operations on sequences

#### Polymorphic array and vector
  * `mkarray : int * 'a -> 'a array`<br/>
    create a polymorphic array; this primop is required to support the
    dictionary-passing representation of polymorphic arrays.<br/>
    `P.INLMKARRAY`

  * `arr_unsafe_sub : 'a array * int -> 'a`<br/>
    subscript from polymorphic array without bounds checking.<br/>
    `P.SUBSCRIPT`

  * `arr_sub : 'a array * int -> 'a`<br/>
    subscript from polymorphic array.<br/>
    `P.INLSUBSCRIPT`

  * `vec_unsafe_sub : 'a vector * int -> 'a`<br/>
    subscript from polymorphic vector without bounds checking.<br/>
    `P.SUBSCRIPTV`

  * `vec_sub : 'a vector * int -> 'a`<br/>
    subscript from polymorphic vector.<br/>
    `P.INLSUBSCRIPTV`

  * `arr_unsafe_update : 'a array * int * 'a -> unit`<br/>
    update a polymorphic array without bounds checking.<br/>
    `P.UPDATE`

  * `arr_update : 'a array * int * 'a -> unit`<br/>
    update a polymorphic array.<br/>
    `P.INLUPDATE`

  * `arr_unboxed_update : 'a array * int * 'a -> unit`<br/>
    update a polymorphic array with an unboxed value, which means that there is
    no store-list entry created for the update.<br/>
    `P.UNBOXEDUPDATE`

#### Sequence operations
Sequence values (*e.g.*, `string`, `'a array`, `RealVector.vector`, *etc*.)
are represented by a header consisting of a length (in elements) and a data
pointer to the raw sequence data.

  * `newArray0 : unit -> 'a`<br/>
    `P.NEW_ARRAY0`

  * `seq_length : 'a -> int`<br/>
    get the length field from a sequence header.<br/>
    `P.LENGTH`

  * `seq_data : 'a -> 'b`<br/>
    get the length field from a sequence header.<br/>
    `P.GET_SEQ_DATA`

  * `raw64Sub : 'a * int -> real64`<br/>
    gets an element from a packed tuple of 64-bit reals.  The only use of
    this function is in the implementation of the `Unsafe.Object.nth`
    function. <br/>
    `P.SUBSCRIPT_RAW64`

  * `recordSub : 'a * int -> 'b`<br/>
    gets an element from a record object. This is used by the `Unsafe.Object`
    and `SMLofNJ.Weak` structures to access the components of records. <br/>
    `P.P.SUBSCRIPT_REC`.

#### Byte-array and byte-vector operations
Operations on arrays/vectors of unsigned bytes.

  * `word8_vec_sub : word8vector * int -> word8`<br/>
    subscript from byte vector.<br/>
    `P.INLNUMSUBSCRIPTV(P.UINT 8)`

  * `word8_vec_unsafe_sub : word8vector * int -> word8`<br/>
    subscript from byte vector without bounds checking.<br/>
    `P.NUMSUBSCRIPTV(P.UINT 8)`

  * `word8_vec_unsafe_update : word8vector * int * word8 -> unit`<br/>
    destructive update of a vector without bounds checking.
    This operation is used to implement vector initialization.<br/>
    `P.NUMUPDATE(P.UINT 8)`

  * `word8_arr_sub : word8array * int -> word8`<br/>
    subscript from byte array.<br/>
    `P.INLNUMSUBSCRIPT(P.UINT 8)`

  * `word8_arr_update : word8array * int * word8 -> unit`<br/>
    update byte array.<br/>
    `P.INLNUMUPDATE(P.UINT 8)`

  * `word8_arr_unsafe_sub : word8array * int -> word8`<br/>
    subscript from byte array without bounds checking.<br/>
    `P.NUMSUBSCRIPT(P.UINT 8)`

  * `word8_arr_unsafe_update : word8array * int * word8 -> unit`<br/>
    update byte array without bounds checking.<br/>
    `P.NUMUPDATE(P.UINT 8)`

#### Char-array and char-vector operations
Operations on arrays/vectors of 8-bit characters.

  * `char_vec_sub : charvector * int -> char`<br/>
    subscript from byte vector.<br/>
    `P.INLNUMSUBSCRIPTV(P.UINT 8)`

  * `char_vec_unsafe_sub : charvector * int -> char`<br/>
    subscript from byte vector without bounds checking.<br/>
    `P.NUMSUBSCRIPTV(P.UINT 8)`

  * `char_vec_unsafe_update : charvector * int * char -> unit`<br/>
    destructive update of a vector without bounds checking.
    This operation is used to implement vector initialization.<br/>
    `P.NUMUPDATE(P.UINT 8)`

  * `char_arr_sub : chararray * int -> char`<br/>
    subscript from byte array.<br/>
    `P.INLNUMSUBSCRIPT(P.UINT 8)`

  * `char_arr_update : chararray * int * char -> unit`<br/>
    update byte array.<br/>
    `P.INLNUMUPDATE(P.UINT 8)`

  * `char_arr_unsafe_sub : chararray * int -> char`<br/>
    subscript from byte array without bounds checking.<br/>
    `P.NUMSUBSCRIPT(P.UINT 8)`

  * `char_arr_unsafe_update : chararray * int * char -> unit`<br/>
    update byte array without bounds checking.<br/>
    `P.NUMUPDATE(P.UINT 8)`

#### Real-array operations
Operations on arrays of 64-bit reals.  Currently the real vector type is
implemented using polymorphic vectors, but eventually we should support
a packed representation for it too.

  * `real64_arr_sub : real64array * int -> real64`<br/>
    subscript from byte array.<br/>
    `P.INLNUMSUBSCRIPT(P.UINT 8)`

  * `real64_arr_update : real64array * int * real64 -> unit`<br/>
    update byte array.<br/>
    `P.INLNUMUPDATE(P.UINT 8)`

  * `real64_arr_unsafe_sub : real64array * int -> real64`<br/>
    subscript from byte array without bounds checking.<br/>
    `P.NUMSUBSCRIPT(P.UINT 8)`

  * `real64_arr_unsafe_update : real64array * int * real64 -> unit`<br/>
    update byte array without bounds checking.<br/>
    `P.NUMUPDATE(P.UINT 8)`

### Numeric primops

#### Default tagged integer operations
These are the primitive operations on the default tagged integer
type (`Int.int`).

  * `int_add : int * int -> int`<br/>
    Signed integer addition with overflow checking.
    (`P.IARITH{oper=P.IADD, sz=<int-size>}`)

  * `int_sub : int * int -> int`<br/>
    Signed integer subtraction with overflow checking.
    (`P.IARITH{oper=P.ISUB, sz=<int-size>}`)

  * `int_mul : int * int -> int`<br/>
    `P.IARITH{oper=P.IMUL, sz=<int-size>}`

  * `int_div : int * int -> int`<br/>
    `P.INLDIV(P.INT <int-size>)`

  * `int_mod : int * int -> int`<br/>
    `P.INLMOD(P.INT <int-size>)`

  * `int_quot : int * int -> int`<br/>
    `P.INLQUOT(P.INT <int-size>)`

  * `int_rem : int * int -> int`<br/>
    `P.INLREM(P.INT <int-size>)`

  * `int_neg : word32 -> word32`<br/>
    `P.IARITH{oper=P.INEG, sz=<int-size>}`

  * `int_lt : int * int -> bool`<br/>
    `P.CMP{oper=P.LT, kind=P.INT <int-size>}`

  * `int_le : int * int -> bool`<br/>
    `P.CMP{oper=P.LTE, kind=P.INT <int-size>}`

  * `int_gt : int * int -> bool`<br/>
    `P.CMP{oper=P.GT, kind=P.INT <int-size>}`

  * `int_ge : int * int -> bool`<br/>
    `P.CMP{oper=P.GTE, kind=P.INT <int-size>}`

  * `int_eql : int * int -> bool`<br/>
    `P.CMP{oper=P.EQL, kind=P.INT <int-size>}`

  * `int_neq : int * int -> bool`<br/>
    `P.CMP{oper=P.NEQ, kind=P.INT <int-size>}`

  * `int_min : int * int -> int`<br/>
    `P.INLMIN (P.INT <int-size>)`

  * `int_max : int * int -> int`<br/>
    `P.INLMAX (P.INT <int-size>)`

  * `int_abs : int -> int`<br/>
    `P.INLABS (P.INT <int-size>)`

For the default integer type, we add some additional operations that help
simplify the **Basis Library** implementation.

  * `int_unsafe_add : int * int -> int`<br/>
    Signed integer addition *without* overflow checking.  This operation
    is used for index computations on sequences.<br/>
    `P.PURE_ARITH{oper=P.ADD, kind=P.UINT <int-size>}`

  * `int_unsafe_sub : int * int -> int`<br/>
    Signed integer subtraction *without* overflow checking.  This operation
    is used for index computations on sequences.<br/>
    `P.PURE_ARITH{oper=P.SUB, kind=P.UINT <int-size>}`

  * `int_orb : int * int -> int`<br/>
    `P.PURE_ARITH{oper=P.ORB, kind=P.UINT <int-size>}`

  * `int_xorb : int * int -> int`<br/>
    `P.PURE_ARITH{oper=P.XORB, kind=P.UINT <int-size>}`

  * `int_andb : int * int -> int`<br/>
    `P.PURE_ARITH{oper=P.ANDB, kind=P.UINT <int-size>}`

  * `int_notb : int -> int`<br/>
    `P.PURE_ARITH{oper=P.NOTB, sz=P.UINT <int-size>}`

  * `int_raw_rshift : int * word -> int`<br/>
    `P.PURE_ARITH{oper=P.RSHIFT, kind=P.UINT <int-size>}`

  * `int_raw_lshift : int * word -> int`<br/>
    `P.PURE_ARITH{oper=P.LSHIFT, kind=P.UINT <int-size>}`

  * `int_ltu : int * int -> bool`<br/>
    Unsigned comparison of integers (used for bounds checking).<br/>
    `P.CMP{oper=P.GTE, kind=P.UINT <int-size>}`

  * `int_geu : int * int -> bool`<br/>
    Unsigned comparison of integers (used for bounds checking).<br/>
    `P.CMP{oper=P.GTE, kind=P.UINT <int-size>}`

#### Default tagged word operations
These are the primitive operations on the default tagged word
type (`Word.word`).

  * `word_add : word * word -> word`<br/>
    `P.PURE_ARITH{oper=P.ADD, kind=P.UINT <int-size>}`

  * `word_sub : word * word -> word`<br/>
    `P.PURE_ARITH{oper=P.SUB, kind=P.UINT <int-size>}`

  * `word_mul : word * word -> word`<br/>
    `P.PURE_ARITH{oper=P.MUL, kind=P.UINT <int-size>}`

  * `word_div : word * word -> word`<br/>
    `P.INLQUOT(P.UINT <int-size>)`

  * `word_mod : word * word -> word`<br/>
    `P.INLREM(P.UINT <int-size>)`

  * `word_orb : word * word -> word`<br/>
    `P.PURE_ARITH{oper=P.ORB, kind=P.UINT <int-size>}`

  * `word_xorb : word * word -> word`<br/>
    `P.PURE_ARITH{oper=P.XORB, kind=P.UINT <int-size>}`

  * `word_andb : word * word -> word`<br/>
    `P.PURE_ARITH{oper=P.ANDB, kind=P.UINT <int-size>}`

  * `word_notb : word -> word`<br/>
    `P.PURE_ARITH{oper=P.NOTB, kind=P.UINT <int-size>}`

  * `word_neg : word -> word`<br/>
    `P.PURE_ARITH{oper=P.NEG, kind=P.UINT <int-size>}`

  * `word_rshift : word * word -> word`<br/>
    `P.INLRSHIFT(P.UINT <int-size>)`

  * `word_rshiftl : word * word -> word`<br/>
    `P.INLRSHIFTL(P.UINT <int-size>)`

  * `word_lshift : word * word -> word`<br/>
    `P.PURE_ARITH{oper=P.LSHIFT, kind=P.UINT <int-size>}`

  * `word_raw_rshift : word * word -> word`<br/>
    `P.INLLSHIFT(P.UINT <int-size>)`

  * `word_raw_rshiftl : word * word -> word`<br/>
    `P.PURE_ARITH{oper=P.RSHIFTL, kind=P.UINT <int-size>}`

  * `word_raw_lshift : word * word -> word`<br/>
    `P.PURE_ARITH{oper=P.RSHIFT, kind=P.UINT <int-size>}`

  * `word_gt : word * word -> bool`<br/>
    `P.CMP{oper=P.GT, kind=P.UINT <int-size>}`

  * `word_ge : word * word -> bool`<br/>
    `P.CMP{oper=P.GTE, kind=P.UINT <int-size>}`

  * `word_lt : word * word -> bool`<br/>
    `P.CMP{oper=P.LT, kind=P.UINT <int-size>}`

  * `word_le : word * word -> bool`<br/>
    `P.CMP{oper=P.LTE, kind=P.UINT <int-size>}`

  * `word_eql : word * word -> bool`<br/>
    `P.CMP{oper=P.EQL, kind=P.UINT <int-size>}`

  * `word_neq : word * word -> bool`<br/>
    `P.CMP{oper=P.NEQ, kind=P.UINT <int-size>}`

  * `word_min : word * word -> word`<br/>
    `P.INLMIN (P.UINT <int-size>)`

  * `word_max : word * word -> word`<br/>
    `P.INLMAX (P.UINT <int-size>)`

#### 32-bit integer operations
  * `int32_add : int32 * int32 -> int32`<br/>
    `P.IARITH{oper=P.IADD, sz=32}`

  * `int32_sub : int32 * int32 -> int32`<br/>
    `P.IARITH{oper=P.ISUB, sz=32}`

  * `int32_mul : int32 * int32 -> int32`<br/>
    `P.IARITH{oper=P.IMUL, sz=32}`

  * `int32_div : int32 * int32 -> int32`<br/>
    `P.IARITH{oper=P.IQUOT, sz=32}`

  * `int32_mod : int32 * int32 -> int32`<br/>
    `P.IARITH{oper=P.IREM, sz=32}`

  * `int32_quot : int32 * int32 -> int32`<br/>
    `P.IARITH{oper=P.IQUOT, sz=32}`

  * `int32_rem : int32 * int32 -> int32`<br/>
    `P.IARITH{oper=P.IREM, sz=32}`

  * `int32_neg : word32 -> word32`<br/>
    `P.IARITH{oper=P.INEG, sz=32}`

  * `int32_gt : int32 * int32 -> bool`<br/>
    `P.CMP{oper=P.GT, kind=P.INT 32}`

  * `int32_ge : int32 * int32 -> bool`<br/>
    `P.CMP{oper=P.GTE, kind=P.INT 32}`

  * `int32_lt : int32 * int32 -> bool`<br/>
    `P.CMP{oper=P.LT, kind=P.INT 32}`

  * `int32_le : int32 * int32 -> bool`<br/>
    `P.CMP{oper=P.LTE, kind=P.INT 32}`

  * `int32_eql : int32 * int32 -> bool`<br/>
    `P.CMP{oper=P.EQL, kind=P.INT 32}`

  * `int32_neq : int32 * int32 -> bool`<br/>
    `P.CMP{oper=P.NEQ, kind=P.INT 32}`

  * `int32_min : int32 * int32 -> int32`<br/>
    `P.INLMIN (P.INT 32)`

  * `int32_max : int32 * int32 -> int32`<br/>
    `P.INLMAX (P.INT 32)`

  * `int32_abs : int32 -> int32`<br/>
    `P.INLABS (P.INT 32)`

#### 8-bit word operations

  * `word8_mul : word8 * word8 -> word8`<br/>
    `P.PURE_ARITH{oper=P.MUL, kind=P.UINT 8}`

  * `word8_div : word8 * word8 -> word8`<br/>
    `P.PURE_ARITH{oper=P.QUOT, kind=P.UINT 8}`

  * `word8_mod : word8 * word8 -> word8`<br/>
    `P.PURE_ARITH{oper=P.REM, kind=P.UINT 8}`

  * `word8_add : word8 * word8 -> word8`<br/>
    `P.PURE_ARITH{oper=P.ADD, kind=P.UINT 8}`

  * `word8_sub : word8 * word8 -> word8`<br/>
    `P.PURE_ARITH{oper=P.SUB, kind=P.UINT 8}`

  * `word8_orb : word8 * word8 -> word8`<br/>
    `P.PURE_ARITH{oper=P.ORB, kind=P.UINT 8}`

  * `word8_xorb : word8 * word8 -> word8`<br/>
    `P.PURE_ARITH{oper=P.XORB, kind=P.UINT 8}`

  * `word8_andb : word8 * word8 -> word8`<br/>
    `P.PURE_ARITH{oper=P.ANDB, kind=P.UINT 8}`

  * `word8_notb : word8 -> word8`<br/>
    `P.PURE_ARITH{oper=P.NOTB, kind=P.UINT 8}`

  * `word8_neg : word8 -> word8`<br/>
    `P.PURE_ARITH{oper=P.NEG, kind=P.UINT 8}`

  * `word8_rshift : word8 * word -> word`<br/>
    `P.INLRSHIFT(P.UINT 8)`

  * `word8_rshiftl : word8 * word -> word`<br/>
    `P.INLRSHIFTL(P.UINT 8)`

  * `word8_lshift : word8 * word -> word`<br/>
    `P.INLLSHIFT(P.UINT 8)`

  * `word8_raw_rshift : word8 * word -> word`<br/>
    `P.PURE_ARITH{oper=P.RSHIFT, kind=P.UINT 8}`

  * `word8_raw_rshiftl : word8 * word -> word`<br/>
    `P.PURE_ARITH{oper=P.RSHIFTL, kind=P.UINT 8}`

  * `word8_raw_lshift : word8 * word -> word`<br/>
    `P.PURE_ARITH{oper=P.LSHIFT, kind=P.UINT 8}`

  * `word8_gt : word8 * word8 -> bool`<br/>
    `P.CMP{oper=P.GT, kind=P.UINT 8}`

  * `word8_ge : word8 * word8 -> bool`<br/>
    `P.CMP{oper=P.GTE, kind=P.UINT 8}`

  * `word8_lt : word8 * word8 -> bool`<br/>
    `P.CMP{oper=P.LT, kind=P.UINT 8}`

  * `word8_le : word8 * word8 -> bool`<br/>
    `P.CMP{oper=P.LTE, kind=P.UINT 8}`

  * `word8_eql : word8 * word8 -> bool`<br/>
    `P.CMP{oper=P.EQL, kind=P.UINT 8}`

  * `word8_neq : word8 * word8 -> bool`<br/>
    `P.CMP{oper=P.NEQ, kind=P.UINT 8}`

  * `word8_min : word8 * word8 -> word8`<br/>
    `P.INLMIN (P.UINT 8)`

  * `word8_max : word8 * word8 -> word8`<br/>
    `P.INLMAX (P.UINT 8)`

#### 32-bit word operations
These operations work on the boxed 32-bit word type on 32-bit
machines and are just wrappers for 63-bit tagged word operations
on 64-bit machines.

  * `word32_mul : word32 * word32 -> word32`<br/>
    `P.PURE_ARITH{oper=P.MUL, kind=P.UINT 32}`

  * `word32_div : word32 * word32 -> word32`<br/>
    `P.PURE_ARITH{oper=P.QUOT, kind=P.UINT 32}`

  * `word32_mod : word32 * word32 -> word32`<br/>
    `P.PURE_ARITH{oper=P.REM, kind=P.UINT 32}`

  * `word32_add : word32 * word32 -> word32`<br/>
    `P.PURE_ARITH{oper=P.ADD, kind=P.UINT 32}`

  * `word32_sub : word32 * word32 -> word32`<br/>
    `P.PURE_ARITH{oper=P.SUB, kind=P.UINT 32}`

  * `word32_orb : word32 * word32 -> word32`<br/>
    `P.PURE_ARITH{oper=P.ORB, kind=P.UINT 32}`

  * `word32_xorb : word32 * word32 -> word32`<br/>
    `P.PURE_ARITH{oper=P.XORB, kind=P.UINT 32}`

  * `word32_andb : word32 * word32 -> word32`<br/>
    `P.PURE_ARITH{oper=P.ANDB, kind=P.UINT 32}`

  * `word32_notb : word32 -> word32`<br/>
    `P.PURE_ARITH{oper=P.NOTB, kind=P.UINT 32}`

  * `word32_neg : word32 -> word32`<br/>
    `P.PURE_ARITH{oper=P.NEG, kind=P.UINT 32}`

  * `word32_rshift : word32 * word -> word`<br/>
    `P.INLRSHIFT(P.UINT 32)`

  * `word32_rshiftl : word32 * word -> word`<br/>
    `P.INLRSHIFTL(P.UINT 32)`

  * `word32_lshift : word32 * word -> word`<br/>
    `P.INLLSHIFT(P.UINT 32)`

  * `word32_raw_rshift : word32 * word -> word`<br/>
    `P.PURE_ARITH{oper=P.RSHIFT, kind=P.UINT 32}`

  * `word32_raw_rshiftl : word32 * word -> word`<br/>
    `P.PURE_ARITH{oper=P.RSHIFTL, kind=P.UINT 32}`

  * `word32_raw_lshift : word32 * word -> word`<br/>
    `P.PURE_ARITH{oper=P.LSHIFT, kind=P.UINT 32}`

  * `word32_gt : word32 * word32 -> bool`<br/>
    `P.CMP{oper=P.GT, kind=P.UINT 32}`

  * `word32_ge : word32 * word32 -> bool`<br/>
    `P.CMP{oper=P.GTE, kind=P.UINT 32}`

  * `word32_lt : word32 * word32 -> bool`<br/>
    `P.CMP{oper=P.LT, kind=P.UINT 32}`

  * `word32_le : word32 * word32 -> bool`<br/>
    `P.CMP{oper=P.LTE, kind=P.UINT 32}`

  * `word32_eql : word32 * word32 -> bool`<br/>
    `P.CMP{oper=P.EQL, kind=P.UINT 32}`

  * `word32_neq : word32 * word32 -> bool`<br/>
    `P.CMP{oper=P.NEQ, kind=P.UINT 32}`

  * `word32_min : word32 * word32 -> word32`<br/>
    `P.INLMIN (P.UINT 32)`

  * `word32_max : word32 * word32 -> word32`<br/>
    `P.INLMAX (P.UINT 32)`

#### 64-bit integer operations
**Note:** 64-bit integer operations are currently not supported in the compiler,
but we expect to add them in 110.88.

  * `int64_add : int64 * int64 -> int64`<br/>
    `P.IARITH{oper=P.IADD, sz=64}`

  * `int64_sub : int64 * int64 -> int64`<br/>
    `P.IARITH{oper=P.ISUB, sz=64}`

  * `int64_mul : int64 * int64 -> int64`<br/>
    `P.IARITH{oper=P.IMUL, sz=64}`

  * `int64_div : int64 * int64 -> int64`<br/>
    `P.IARITH{oper=P.IQUOT, sz=64}`

  * `int64_mod : int64 * int64 -> int64`<br/>
    `P.IARITH{oper=P.IREM, sz=64}`

  * `int64_quot : int64 * int64 -> int64`<br/>
    `P.IARITH{oper=P.IQUOT, sz=64}`

  * `int64_rem : int64 * int64 -> int64`<br/>
    `P.IARITH{oper=P.IREM, sz=64}`

  * `int64_neg : word32 -> word32`<br/>
    `P.IARITH{oper=P.INEG, sz=64}`

  * `int64_gt : int64 * int64 -> bool`<br/>
    `P.CMP{oper=P.GT, kind=P.INT 64}`

  * `int64_ge : int64 * int64 -> bool`<br/>
    `P.CMP{oper=P.GTE, kind=P.INT 64}`

  * `int64_lt : int64 * int64 -> bool`<br/>
    `P.CMP{oper=P.LT, kind=P.INT 64}`

  * `int64_le : int64 * int64 -> bool`<br/>
    `P.CMP{oper=P.LTE, kind=P.INT 64}`

  * `int64_eql : int64 * int64 -> bool`<br/>
    `P.CMP{oper=P.EQL, kind=P.INT 64}`

  * `int64_neq : int64 * int64 -> bool`<br/>
    `P.CMP{oper=P.NEQ, kind=P.INT 64}`

  * `int64_min : int64 * int64 -> int64`<br/>
    `P.INLMIN (P.INT 64)`

  * `int64_max : int64 * int64 -> int64`<br/>
    `P.INLMAX (P.INT 64)`

  * `int64_abs : int64 -> int64`<br/>
    `P.INLABS (P.INT 64)`

#### 64-bit word operations
**Note:** 64-bit word operations are currently not supported in the compiler,
but we expect to add them in 110.88.

  * `word64_mul : word64 * word64 -> word64`<br/>
    `P.PURE_ARITH{oper=P.MUL, kind=P.UINT 64}`

  * `word64_div : word64 * word64 -> word64`<br/>
    `P.PURE_ARITH{oper=P.QUOT, kind=P.UINT 64}`

  * `word64_mod : word64 * word64 -> word64`<br/>
    `P.PURE_ARITH{oper=P.REM, kind=P.UINT 64}`

  * `word64_add : word64 * word64 -> word64`<br/>
    `P.PURE_ARITH{oper=P.ADD, kind=P.UINT 64}`

  * `word64_sub : word64 * word64 -> word64`<br/>
    `P.PURE_ARITH{oper=P.SUB, kind=P.UINT 64}`

  * `word64_orb : word64 * word64 -> word64`<br/>
    `P.PURE_ARITH{oper=P.ORB, kind=P.UINT 64}`

  * `word64_xorb : word64 * word64 -> word64`<br/>
    `P.PURE_ARITH{oper=P.XORB, kind=P.UINT 64}`

  * `word64_andb : word64 * word64 -> word64`<br/>
    `P.PURE_ARITH{oper=P.ANDB, kind=P.UINT 64}`

  * `word64_notb : word64 -> word64`<br/>
    `P.PURE_ARITH{oper=P.NOTB, kind=P.UINT 64}`

  * `word64_neg : word64 -> word64`<br/>
    `P.PURE_ARITH{oper=P.NEG, kind=P.UINT 64}`

  * `word64_rshift : word64 * word -> word`<br/>
    `P.INLRSHIFT(P.UINT 64)`

  * `word64_rshiftl : word64 * word -> word`<br/>
    `P.INLRSHIFTL(P.UINT 64)`

  * `word64_lshift : word64 * word -> word`<br/>
    `P.INLLSHIFT(P.UINT 64)`

  * `word64_raw_rshift : word64 * word -> word`<br/>
    `P.PURE_ARITH{oper=P.RSHIFT, kind=P.UINT 64}`

  * `word64_raw_rshiftl : word64 * word -> word`<br/>
    `P.PURE_ARITH{oper=P.RSHIFTL, kind=P.UINT 64}`

  * `word64_raw_lshift : word64 * word -> word`<br/>
    `P.PURE_ARITH{oper=P.LSHIFT, kind=P.UINT 64}`

  * `word64_gt : word64 * word64 -> bool`<br/>
    `P.CMP{oper=P.GT, kind=P.UINT 64}`

  * `word64_ge : word64 * word64 -> bool`<br/>
    `P.CMP{oper=P.GTE, kind=P.UINT 64}`

  * `word64_lt : word64 * word64 -> bool`<br/>
    `P.CMP{oper=P.LT, kind=P.UINT 64}`

  * `word64_le : word64 * word64 -> bool`<br/>
    `P.CMP{oper=P.LTE, kind=P.UINT 64}`

  * `word64_eql : word64 * word64 -> bool`<br/>
    `P.CMP{oper=P.EQL, kind=P.UINT 64}`

  * `word64_neq : word64 * word64 -> bool`<br/>
    `P.CMP{oper=P.NEQ, kind=P.UINT 64}`

  * `word64_min : word64 * word64 -> word64`<br/>
    `P.INLMIN (P.UINT 64)`

  * `word64_max : word64 * word64 -> word64`<br/>
    `P.INLMAX (P.UINT 64)`

#### 64-bit real operations
  * `real64_add : real64 * real64 -> real64`<br/>
    `P.PURE_ARITH{oper=P.ADD, overflow=true, kind=P.FLOAT 64}`

  * `real64_sub : real64 * real64 -> real64`<br/>
    `P.PURE_ARITH{oper=P.SUB, overflow=true, kind=P.FLOAT 64}`

  * `real64_mul : real64 * real64 -> real64`<br/>
    `P.PURE_ARITH{oper=P.MUL, overflow=true, kind=P.FLOAT 64}`

  * `real64_div : real64 * real64 -> real64`<br/>
    `P.PURE_ARITH{oper=P.QUOT, overflow=true, kind=P.FLOAT 64}`

  * `real64_neg : word32 -> word32`<br/>
    `P.PURE_ARITH{oper=P.NEG, overflow=true, kind=P.FLOAT 64}`

  * `real64_gt : real64 * real64 -> bool`<br/>
    `P.CMP{oper=P.GT, kind=P.FLOAT 64}`

  * `real64_lt : real64 * real64 -> bool`<br/>
    `P.CMP{oper=P.LT, kind=P.FLOAT 64}`

  * `real64_le : real64 * real64 -> bool`<br/>
    `P.CMP{oper=P.LTE, kind=P.FLOAT 64}`

  * `real64_ge : real64 * real64 -> bool`<br/>
    `P.CMP{oper=P.GTE, kind=P.FLOAT 64}`

  * `real64_eql : real64 * real64 -> bool`<br/>
    `P.CMP{oper=P.EQL, kind=P.FLOAT 64}`

  * `real64_neq : real64 * real64 -> bool`<br/>
    `P.CMP{oper=P.NEQ, kind=P.FLOAT 64}`

  * `real64_sgn : real64 -> bool`<br/>
    `P.FSGN 64`

  * `real64_min : real64 * real64 -> real64`<br/>
    `P.INLMIN (P.FLOAT 64)`

  * `real64_max : real64 * real64 -> real64`<br/>
    `P.INLMAX (P.FLOAT 64)`

  * `real64_abs : real64 -> real64`<br/>
    `P.ARITH{oper=P.FABS, kind=P.FLOAT 64}`

  * `real64_sin : real64 -> real64`<br/>
    `P.PURE_ARITH{oper=P.FSIN, kind=P.FLOAT 64}`

  * `real64_cos : real64 -> real64`<br/>
    `P.PURE_ARITH{oper=P.FCOS, kind=P.FLOAT 64}`

  * `real64_tan : real64 -> real64`<br/>
    `P.PURE_ARITH{oper=P.FTAN, kind=P.FLOAT 64}`

  * `real64_sqrt : real64 -> real64`<br/>
    `P.PURE_ARITH{oper=P.FSQRT, kind=P.FLOAT 64}`


### Conversions

We use the following operation-prefixes for conversions between integer and
word types:

  * `unsigned_` -- for word to integer conversions where the resulting
    integer will be non-negative (*i.e.*, represent the same number as the
    argument).  These operations raise `Overflow` if the argument it not
    representable as an integer of the specified size.

  * `signed_` -- for word to integer conversions where the resulting
    integer will have the same bit representation as the argument.  These
    operations raise `Overflow` if the argument (interpreted as a signed
    2's complement number) is not representable as an integer of the specified
    size.

  * no prefix for integer-to-integer, integer-to-word, and word-to-word,
    conversions.  In the case of integer-to-integer conversions, the
    operation will raise `Overflow` if the argument is too large to
    represent in the result type.

For conversions between integer and word types, there are five basic
primitive operations (**TEST**, **TESTU**, **EXTEND**, **TRUNC**, and **COPY**),
which are described in the `conversions.md` file.

#### Conversions for tagged integers and words

  * `word32_to_word : word32 -> word`<br />
    Large word to word conversion (32-bit large word)<br/>
    `P.TRUNC(32, 31)`

  * `unsigned_word_to_word32 : word -> word32`<br/>
    Word to large word conversion (32-bit large word)<br/>
    `P.COPY(31, 32)`

  * `signed_word_to_word32 : word -> word32`<br/>
    Word to large word conversion (32-bit large word)<br/>
    `P.EXTEND(31, 32)`

  * `int_to_intinf : int -> intinf`<br/>
    `P.EXTEND_INF <int-size>`

  * `intinf_to_int : intinf -> int`<br/>
    `P.TEST_INF <int-size>`

  * `int_to_word : int -> word`<br />
    `P.COPY(<int-size>, <int-size>)`

  * `unsigned_word_to_int : word -> int`<br />
    `P.TESTU(<int-size>, <int-size>)`

  * `signed_word_to_int : word -> int`<br />
    `P.COPY(<int-size>, <int-size>)`

  * `unsigned_word_to_intinf : word -> intinf`<br/>
    `P.COPY_INF <int-size>`

  * `signed_word_to_intinf : word -> intinf`<br/>
    `P.EXTEND_INF <int-size>`

  * `intinf_to_word : intinf -> word`<br/>
    `P.TRUNC_INF <int-size>`

#### Conversions for 32-bit integers and words

  * `int32_to_int : int32 -> int`<br/>
    `P.TEST(32, 31)` (32-bit target) or
    `P.EXTEND(32, 63)` (64-bit target).

  * `word32_to_word : word32 -> word`<br/>
    `P.TRUNC(32, 31)` (32-bit target) or
    `P.COPY(32, 63)` (64-bit target).

  * `int_to_int32 : int -> int32`<br/>
    `P.EXTEND(31, 32)` (32-bit target) or
    `P.TEST(63, 32)` (64-bit target).

  * `word_to_word32 : word -> word32`<br/>
    `P.COPY(31, 32)` (32-bit target) or
    `P.TRUNC(63, 32)` (64-bit target).

  * `int32_to_intinf : int32 -> intinf`<br/>
    `P.EXTEND_INF 32`

  * `intinf_to_int32 : intinf -> int32`<br/>
    `P.TEST_INF 32`

  * `int_to_word32 : int -> word32`<br/>
    `P.EXTEND(31, 32)` (32-bit target) or
    `P.TRUNC(63, 32)` (64-bit target).

  * `unsigned_word32_to_int : word32 -> int`<br/>
    `P.TESTU(32, 31)` (32-bit target) or
    `P.COPY(32, 63)` (64-bit target).

  * `signed_word32_to_int : word32 -> int`<br/>
    `P.TEST(32, 31)` (32-bit target) or
    `P.EXTEND(32, 63)` (64-bit target).

  * `unsigned_word32_to_intinf : word32 -> intinf`<br/>
    `P.COPY_INF 32`

  * `signed_word32_to_intinf : word32 -> intinf`<br/>
    `P.EXTEND_INF 32`

  * `intinf_to_word32 : intinf -> word32`<br/>
    `P.TRUNC_INF 32`

Note that if the `LargeWord.word` type is 64-bits (which it should be), the
we have the following additional operations:

  * `word64_to_word32 : word64 -> word32`<br />
    Large word to word conversion<br/>
    `P.TRUNC(64, 32)`

  * `unsigned_word32_to_word64 : word32 -> word64`<br/>
    Unsigned word to large word conversion<br/>
    `P.COPY(32, 64)`

  * `signed_word32_to_word64 : word32 -> word64`<br/>
    Signed word to large word conversion<br/>
    `P.EXTEND(32, 64)`


#### Conversions for 64-bit integers and words

  * `int64_to_int : int64 -> int`<br/>
    `P.TEST(64, <int-sz>)`.

  * `word64_to_word : word64 -> word`<br/>
    `P.TRUNC(64, <int-sz>)`.

  * `int_to_int64 : int -> int64`<br/>
    `P.EXTEND(<int-sz>, 64)`.

  * `word_to_word64 : word -> word64`<br/>
    Large word to word conversion<br/>
    `P.COPY(<int-sz>, 64)`.

  * `signed_word64_to_word32 : word64 -> word32`<br/>
    Unsigned word to large word conversion<br/>
    `P.TRUNC(64, 32)`.

  * `unsigned_word64_to_word32 : word64 -> word32`<br/>
    Signed word to large word conversion<br/>
    `P.COPY(32, 64)`.

  * `word32_to_word64 : word32 -> word64`<br/>
    `P.EXTEND(32, 64)`.

  * `int64_to_intinf : int64 -> intinf`<br/>
    `P.EXTEND_INF 64`

  * `intinf_to_int64 : intinf -> int64`<br/>
    `P.TEST_INF 64`

  * `int_to_word64 : int -> word64`<br/>
    `P.EXTEND(31, 64)` (64-bit target) or
    `P.TRUNC(63, 64)` (64-bit target).

  * `unsigned_word64_to_int : word64 -> int`<br/>
    `P.TESTU(64, 31)` (64-bit target) or
    `P.COPY(64, 63)` (64-bit target).

  * `signed_word64_to_int : word64 -> int`<br/>
    `P.TEST(64, 31)` (64-bit target) or
    `P.EXTEND(64, 63)` (64-bit target).

  * `unsigned_word64_to_intinf : word64 -> intinf`<br/>
    `P.COPY_INF 64`

  * `signed_word64_to_intinf : word64 -> intinf`<br/>
    `P.EXTEND_INF 64`

  * `intinf_to_word64 : intinf -> word64`<br/>
    `P.TRUNC_INF 64`


#### Conversions for 8-bit words
**NOTE:** currently the compiler does not support 8-bit integers, so this set
of conversions is limited to just the `word8` type.
  * `word32_to_word8 : word32 -> word8`<br />
    Large word to word conversion (32-bit large word)<br/>
    `P.TRUNC(32, 8)`

  * `unsigned_word8_to_word32 : word8 -> word32`<br/>
    Unsigned word to large word conversion (32-bit large word)<br/>
    `P.COPY(8, 32)`

  * `signed_word8_to_word32 : word8 -> word32`<br/>
    Signed word to large word conversion (32-bit large word)<br/>
    `P.EXTEND(8, 32)`

  * `int_to_word8 : int -> word8`<br/>

  * `unsigned_word8_to_int : word8 -> int`<br/>

  * `signed_word8_to_int : word8 -> int`<br/>

  * `unsigned_word8_to_intinf : word8 -> intinf`<br/>
    `P.COPY_INF 8`

  * `signed_word8_to_intinf : word8 -> intinf`<br/>
    `P.EXTEND_INF 8`

  * `intinf_to_word8 : intinf -> word8`<br/>
    `P.TRUNC_INF 8`


#### Additional conversions for 32-bit targets

Additional conversions that are used in `system/smlnj/init/core-intinf.sml`
`system/smlnj/init/core-int64.sml`, and `system/smlnj/init/core-word64.sml`.

  * `trunc_int32_to_word : int32 -> word`<br/>
    `P.TRUNC(32, intSz)`

  * `copy_int32_to_word32 : int32 -> word32`<br/>
    `P.COPY(32, 32)`

  * `copy_word_to_int32 : word32 -> int32`<br/>
    `P.COPY(intSz, 32)`

  * `copy_word32_to_int32 : word32 -> int32`<br/>
    `P.COPY(32, 32)`

#### Additional conversions for 64-bit targets


#### Conversions to support 64-bit integers/words on 32-bit targets.
These operations are only present on 32-bit targets and are used to
get access to the concrete representation of 64-bit integer and word
values.

  * `int64_to_pair : int64 -> word32 * word32`<br/>
    `P.CVT64`

  * `int64_from_pair : word32 * word32 -> int64`<br/>
    `P.CVT64`

  * `word64_to_pair : word64 -> word32 * word32`<br/>
    `P.CVT64`

  * `word64_from_pair : word32 * word32 -> word64`<br/>
    `P.CVT64`


#### Conversions between integers and reals

  * `int_to_real64 : int -> real64`<br />
    `P.INT_TO_REAL{from=<int-size>, to=64}`

  * `int32_to_real64 : int32 -> real64`<br />
    `P.INT_TO_REAL{from=32, to=64}` (32-bit targets only)

  * `int64_to_real64 : int64 -> real64`<br />
    `P.INT_TO_REAL{from=64, to=64}` (64-bit targets only)

  * `floor_real64_to_int : real64 -> int`<br />
    `P.REAL_TO_INT{floor=true, from=64, to=<int-size>}`

  * `round_real64_to_int : real64 -> int`<br />
    `P.REAL_TO_INT{floor=false, from=64, to=<int-size>}`

Note: the real to integer conversions should be revised
to directly support the various rounding modes (floor,
ceiling, truncation, and round).

### Character comparisons

  * `char_lt : char * char -> bool`<br />
    `P.CMP{oper=P.LT, kind=P.UINT <int-size>}`

  * `char_lt : char * char -> bool`<br />
    `P.CMP{oper=P.LTE, kind=P.UINT <int-size>}`

  * `char_gt : char * char -> bool`<br />
    `P.CMP{oper=P.GT, kind=P.UINT <int-size>}`

  * `char_gt : char * char -> bool`<br />
    `P.CMP{oper=P.GTE, kind=P.UINT <int-size>}`

  * `char_eql : char * char -> bool`<br/>
    `P.CMP{oper=P.EQL, kind=P.UINT <int-size>}`

  * `char_neq : char * char -> bool`<br/>
    `P.CMP{oper=P.NEQ, kind=P.UINT <int-size>}`

### FFI support

The following primops work on raw memory addresses and are included to support
interaction with **C** code using the **NLFFI** infrastructure.

We use the type `raw_ptr` here to represent the type of a pointer to a **C**
object; it is a word type that is the same size as a machine address
(*i.e.*, `Word32.word32` or `Word64.word`).  Eventually, it should be made
abstract and be supported by the compiler.

Note also that the `RAW_LOAD` and `RAW_STORE` primops are used with different
numbers of arguments (*e.g.*, `RAW_LOAD` has one argument for `raw_load_int16`,
but two arguments for `raw_sub_int16`, where the extra argument is the offset).

  * `raw_ccall : raw_ptr * 'a * 'b -> 'c`<br />
    This primop is a call to a **C** function via a function pointer (the
    first argument).  The primop cannot be used without having `'a`, `'b`,
    and `'c` monomorphically instantiated.  In particular, `'a` will be
    the type of the ML argument list, `'c` will be the type of the result,
    and `'b` will be a type of fake arguments.  The idea is that `'b` is
    instantiated with an ML type that encodes the type of the actual
    **C** function in order to be able to generate code according to the **C**
    calling convention. <br/>
    In other words, 'b will be a completely ad-hoc encoding of a `CTypes.c_proto`
    value in ML types.  The encoding also contains information about
    calling conventions and reentrancy. <br />
    `P.RAW_CCALL NONE`

  * `raw_record : int -> 'a`<br />
    Allocates an uninitialized **C** object of the given size on the ML heap;
    the object will be word-size aligned.
    We use the `raw_sub_xxx` and `raw_update_xxx` primops (see below)
    to access and update the record.  The `'a` result type is to guarantee
    that the compiler will treat the record as a ML object, in case it passes
    thru a gc boundary. <br/>
    `P.RAW_RECORD { align64 = false }`

  * `raw_record64 : int -> 'a`<br />
    Allocates an uninitialized **C** object of the given size on the ML heap;
    the object will be 64-bit aligned. <br/>
    `P.RAW_RECORD { align64 = true }`

  * `raw_load_int8 : raw_ptr -> int32`<br />
    `P.RAW_LOAD(P.INT 8)`

  * `raw_load_word8 : raw_ptr -> word32`<br />
    `P.RAW_LOAD(P.UINT 8)`

  * `raw_load_int16 : raw_ptr -> int32`<br />
    `P.RAW_LOAD(P.INT 16)`

  * `raw_load_word16 : raw_ptr -> word32`<br />
    `P.RAW_LOAD(P.UINT 16)`

  * `raw_load_int32 : raw_ptr -> int32`<br />
    `P.RAW_LOAD(P.INT 32)`

  * `raw_load_word32 : raw_ptr -> word32`<br />
    `P.RAW_LOAD(P.UINT 32)`

  * `raw_load_float32 : raw_ptr -> real`<br />
    `P.RAW_LOAD(P.FLOAT 32)`

  * `raw_load_float64 : raw_ptr -> real`<br />
    `P.RAW_LOAD(P.FLOAT 32)`

  * `raw_store_int8 : raw_ptr * int32 -> unit`<br />
    `P.RAW_STORE(P.INT 8)`

  * `raw_store_word8 : raw_ptr * word32 -> unit`<br />
    `P.RAW_STORE(P.UINT 8)`

  * `raw_store_int16 : raw_ptr * int32 -> unit`<br />
    `P.RAW_STORE(P.INT 16)`

  * `raw_store_word16 : raw_ptr * word32 -> unit`<br />
    `P.RAW_STORE(P.UINT 16)`

  * `raw_store_int32 : raw_ptr * int32 -> unit`<br />
    `P.RAW_STORE(P.INT 32)`

  * `raw_store_word32 : raw_ptr * word32 -> unit`<br />
    `P.RAW_STORE(P.UINT 32)`

  * `raw_store_float32 : raw_ptr * real -> unit`<br />
    `P.RAW_STORE(P.FLOAT 32)`

  * `raw_store_float64 : raw_ptr * real -> unit`<br />
    `P.RAW_STORE(P.FLOAT 32)`

  * `raw_sub_int8 : 'a * raw_ptr -> int32`<br />
    `P.RAW_LOAD(P.INT 8)`

  * `raw_sub_word8 : 'a * raw_ptr -> word32`<br />
    `P.RAW_LOAD(P.UINT 8)`

  * `raw_sub_int16 : 'a * raw_ptr -> int32`<br />
    `P.RAW_LOAD(P.INT 16)`

  * `raw_sub_word16 : 'a * raw_ptr -> word32`<br />
    `P.RAW_LOAD(P.UINT 16)`

  * `raw_sub_int32 : 'a * raw_ptr -> int32`<br />
    `P.RAW_LOAD(P.INT 32)`

  * `raw_sub_word32 : 'a * raw_ptr -> word32`<br />
    `P.RAW_LOAD(P.UINT 32)`

  * `raw_sub_float32 : 'a * raw_ptr -> real`<br />
    `P.RAW_LOAD(P.FLOAT 32)`

  * `raw_sub_float64 : 'a * raw_ptr -> real`<br />
    `P.RAW_LOAD(P.FLOAT 32)`

  * `raw_update_int8 : 'a * raw_ptr * int32 -> unit`<br />
    `P.RAW_STORE(P.INT 8)`

  * `raw_update_word8 : 'a * raw_ptr * word32 -> unit`<br />
    `P.RAW_STORE(P.INT 8)`

  * `raw_update_int16 : 'a * raw_ptr * int32 -> unit`<br />
    `P.RAW_STORE(P.INT 8)`

  * `raw_update_word16 : 'a * raw_ptr * word32 -> unit`<br />
    `P.RAW_STORE(P.UINT 16)`

  * `raw_update_int32 : 'a * raw_ptr * int32 -> unit`<br />
    `P.RAW_STORE(P.INT 32)`

  * `raw_update_word32 : 'a * raw_ptr * word32 -> unit`<br />
    `P.RAW_STORE(P.UINT 32)`

  * `raw_update_float32 : 'a * raw_ptr * real -> unit`<br />
    `P.RAW_STORE(P.FLOAT 32)`

  * `raw_update_float64 : 'a * raw_ptr * real -> unit`<br />
    `P.RAW_STORE(P.FLOAT 64)`

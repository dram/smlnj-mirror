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

  * `compiler/ElabData/prim/primop.sml`<br/>
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
  * "`barr`" -- bytearray (used for arrays of `Word8.word` and `char`)
  * "`bvec`" -- bytevector (used for strings and vectors of `Word8.word`)
  * "`arr`" -- polymorphic arrays
  * "`vec`" -- polymorphic vectors

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
    `P.POLYEQL`

  * `<> : ''a * ''a -> bool`<br/>
    `P.POLYNEQ`

  * `ptreql : 'a * 'a -> bool`<br/>
    `P.PTREQL`

  * `ptrneq : 'a * 'a -> bool`<br/>
    `P.PTRNEQ`


#### Runtime hooks
  * `getvar : unit -> 'a`<br/>
    `P.GETVAR`

  * `setvar : 'a -> unit`<br/>
    `P.SETVAR`

  * `setpseudo : 'a * int -> unit`<br/>
    `P.SETPSEUDO`

  * `getpseudo : int -> 'a`<br/>
    `P.GETPSEUDO`

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

  * `setmark : 'a -> unit`<br/>
    `P.SETMARK`

  * `dispose : 'a -> unit`<br/>
    `P.DISPOSE`


#### Inline operations
  * `compose : ('b -> 'c) * ('a -> 'b) -> 'a -> 'c`<br/>
    `P.INLCOMPOSE`

  * `before : 'a * 'b -> 'a`<br/>
    `P.INLBEFORE`

  * `ignore : 'a -> unit`<br/>
    `P.INLIGNORE`

  * `identity : 'a -> 'a`<br/>
    `P.INLIDENTITY`

  * `length : 'a -> int`<br/>
    `P.LENGTH`

  * `objlength : 'a -> int`<br/>
    `P.OBJLENGTH`

  * `bool_not : bool -> bool`<br/>
    `P.INLNOT`


#### Bytearray and bytevector operations
Operations on byte/char array/vectors.  We renamed these to make it clear
which operations do bounds checking and which do not.

  * `bvec_unsafe_sub : 'a * int -> 'b`<br/>
    subscript from byte vector without bounds checking
    (`P.NUMSUBSCRIPT{kind=P.INT 8, checked=false, immutable=true}`)

  * `barr_unsafe_sub : 'a * int -> 'b`<br/>
    subscript from byte array without bounds checking
    (`P.NUMSUBSCRIPT{kind=P.INT 8, checked=false, immutable=false}`)

  * `barr_unsafe_update : 'a * int * 'b -> unit`<br/>
    update byte array without bounds checking
    (`P.NUMUPDATE{kind=P.INT 8, checked=false}`)

  * `bvec_sub : 'a * int -> 'b`<br/>
    subscript from byte vector
    (`P.NUMSUBSCRIPT{kind=P.INT 8, checked=true, immutable=true}`)

  * `barr_sub : 'a * int -> 'b`<br/>
    subscript from byte array
    (`P.NUMSUBSCRIPT{kind=P.INT 8, checked=true, immutable=false}`)

  * `barr_update : 'a * int * 'b -> unit`<br/>
    update byte array
    (`P.NUMUPDATE{kind=P.INT 8, checked=true}`)


#### Polymorphic array and vector
  * `mkarray : int * 'a -> 'a array`<br/>
    create a polymorphic array
    (`P.INLMKARRAY`)

  * `arr_unsafe_sub : 'a array * int -> 'a`<br/>
    subscript from polymorphic array without bounds checking
    (`P.SUBSCRIPT`)

  * `arr_sub : 'a array * int -> 'a`<br/>
    subscript from polymorphic array
    (`P.INLSUBSCRIPT`)

  * `vec_unsafe_sub : 'a vector * int -> 'a`<br/>
    subscript from polymorphic vector without bounds checking
    (`P.SUBSCRIPTV`)

  * `vec_sub : 'a vector * int -> 'a`<br/>
    subscript from polymorphic vector
    (`P.INLSUBSCRIPTV`)

  * `arr_unsafe_update : 'a array * int * 'a -> unit`<br/>
    update a polymorphic array without bounds checking
    (`P.UPDATE`)

  * `arr_update : 'a array * int * 'a -> unit`<br/>
    update a polymorphic array
    (`P.INLUPDATE`)

  * `arr_unboxed_update : 'a array * int * 'a -> unit`<br/>
    update a polymorphic array with an unboxed value, which means that there is
    no store-list entry created for the update.
    `P.UNBOXEDUPDATE`


#### Sequence operations
  * `newArray0 : unit -> 'a`<br/>
    `P.NEW_ARRAY0`

  * `seq_get_data : 'a -> 'b`<br/>
    `P.GET_SEQ_DATA`

  * `unsafe_record_sub : 'a * int -> 'b`<br/>
    `P.SUBSCRIPT_REC`

  * `raw64Sub : 'a * int -> real64`<br/>
    Unclear what purpose this primop serves
    `P.SUBSCRIPT_RAW64`


### Numeric primops

#### Default tagged integer operations
These are the primitive operations on the default tagged integer
type (`Int.int`).

  * `int_add : int * int -> int`<br/>
    `P.ARITH{oper=P.ADD, overflow=true, kind=P.INT <int-size>}`

  * `int_sub : int * int -> int`<br/>
    `P.ARITH{oper=P.SUB, overflow=true, kind=P.INT <int-size>}`

  * `int_mul : int * int -> int`<br/>
    `P.ARITH{oper=P.MUL, overflow=true, kind=P.INT <int-size>}`

  * `int_div : int * int -> int`<br/>
    `P.ARITH{oper=P.QUOT, overflow=true, kind=P.INT <int-size>}`

  * `int_mod : int * int -> int`<br/>
    `P.ARITH{oper=P.REM, overflow=true, kind=P.INT <int-size>}`

  * `int_quot : int * int -> int`<br/>
    `P.ARITH{oper=P.QUOT, overflow=true, kind=P.INT <int-size>}`

  * `int_rem : int * int -> int`<br/>
    `P.ARITH{oper=P.REM, overflow=true, kind=P.INT <int-size>}`

  * `int_orb : int * int -> int`<br/>
    `P.ARITH{oper=P.ORB, overflow=false, kind=P.INT <int-size>}`

  * `int_xorb : int * int -> int`<br/>
    `P.ARITH{oper=P.XORB, overflow=false, kind=P.INT <int-size>}`

  * `int_andb : int * int -> int`<br/>
    `P.ARITH{oper=P.ANDB, overflow=false, kind=P.INT <int-size>}`

  * `int_neg : word32 -> word32`<br/>
    `P.ARITH{oper=P.NEG, overflow=true, kind=P.INT <int-size>}`

  * `int_raw_rshift : int * word -> int`<br/>
    `P.ARITH{oper=P.RSHIFT, overflow=false, kind=P.INT <int-size>}`

  * `int_raw_lshift : int * word -> int`<br/>
    `P.ARITH{oper=P.LSHIFT, overflow=false, kind=P.INT <int-size>}`

  * `int_gt : int * int -> bool`<br/>
    `P.CMP{oper=P.GT, kind=P.INT <int-size>}`

  * `int_ge : int * int -> bool`<br/>
    `P.CMP{oper=P.GTE, kind=P.INT <int-size>}`

  * `int_lt : int * int -> bool`<br/>
    `P.CMP{oper=P.LT, kind=P.INT <int-size>}`

  * `int_le : int * int -> bool`<br/>
    `P.CMP{oper=P.LTE, kind=P.INT <int-size>}`

  * `int_eq : int * int -> bool`<br/>
    `P.CMP{oper=P.EQL, kind=P.INT <int-size>}`

  * `int_ne : int * int -> bool`<br/>
    `P.CMP{oper=P.NEQ, kind=P.INT <int-size>}`

  * `int_min : int * int -> int`<br/>
    `P.INLMIN (P.INT <int-size>)`

  * `int_max : int * int -> int`<br/>
    `P.INLMAX (P.INT <int-size>)`

  * `int_abs : word32 -> word32`<br/>
    `P.INLABS (P.INT <int-size>)`

#### Default tagged word operations
These are the primitive operations on the default tagged word
type (`Word.word`).

  * `word_mul : word * word -> word`<br/>
    `P.ARITH{oper=P.MUL, overflow=false, kind=P.INT <int-size>}`

  * `word_div : word * word -> word`<br/>
    `P.ARITH{oper=P.QUOT, overflow=false, kind=P.INT <int-size>}`

  * `word_mod : word * word -> word`<br/>
    `P.ARITH{oper=P.REM, overflow=false, kind=P.INT <int-size>}`

  * `word_add : word * word -> word`<br/>
    `P.ARITH{oper=P.ADD, overflow=false, kind=P.INT <int-size>}`

  * `word_sub : word * word -> word`<br/>
    `P.ARITH{oper=P.SUB, overflow=false, kind=P.INT <int-size>}`

  * `word_orb : word * word -> word`<br/>
    `P.ARITH{oper=P.ORB, overflow=false, kind=P.INT <int-size>}`

  * `word_xorb : word * word -> word`<br/>
    `P.ARITH{oper=P.XORB, overflow=false, kind=P.INT <int-size>}`

  * `word_andb : word * word -> word`<br/>
    `P.ARITH{oper=P.ANDB, overflow=false, kind=P.INT <int-size>}`

  * `word_notb : word -> word`<br/>
    `P.ARITH{oper=P.NOTB, overflow=false, kind=P.INT <int-size>}`

  * `word_neg : word -> word`<br/>
    `P.ARITH{oper=P.NEG, overflow=false, kind=P.INT <int-size>}`

  * `word_rshift : word * word -> word`<br/>
    `P.ARITH{oper=P.RSHIFT, overflow=false, kind=P.INT <int-size>}`

  * `word_rshiftl : word * word -> word`<br/>
    `P.ARITH{oper=P.RSHIFTL, overflow=false, kind=P.INT <int-size>}`

  * `word_lshift : word * word -> word`<br/>
    `P.ARITH{oper=P.LSHIFT, overflow=false, kind=P.INT <int-size>}`

  * `word_gt : word * word -> bool`<br/>
    `P.CMP{oper=P.GT, kind=P.UINT <int-size>}`

  * `word_ge : word * word -> bool`<br/>
    `P.CMP{oper=P.GTE, kind=P.UINT <int-size>}`

  * `word_lt : word * word -> bool`<br/>
    `P.CMP{oper=P.LT, kind=P.UINT <int-size>}`

  * `word_le : word * word -> bool`<br/>
    `P.CMP{oper=P.LTE, kind=P.UINT <int-size>}`

  * `word_eq : word * word -> bool`<br/>
    `P.CMP{oper=P.EQL, kind=P.UINT <int-size>}`

  * `word_ne : word * word -> bool`<br/>
    `P.CMP{oper=P.NEQ, kind=P.UINT <int-size>}`

  * `word_raw_rshift : word * word -> word`<br/>
    `P.INLRSHIFT(P.UINT <int-size>)`

  * `word_raw_rshiftl : word * word -> word`<br/>
    `P.INLRSHIFTL(P.UINT <int-size>)`

  * `word_raw_lshift : word * word -> word`<br/>
    `P.INLLSHIFT(P.UINT <int-size>)`

  * `word_min : word * word -> word`<br/>
    `P.INLMIN (P.UINT <int-size>)`

  * `word_max : word * word -> word`<br/>
    `P.INLMAX (P.UINT <int-size>)`

#### 8-bit word operations

#### 32-bit integer operations
  * `int32_add : int32 * int32 -> int32`<br/>
    `P.ARITH{oper=P.ADD, overflow=true, kind=P.INT 32}`

  * `int32_sub : int32 * int32 -> int32`<br/>
    `P.ARITH{oper=P.SUB, overflow=true, kind=P.INT 32}`

  * `int32_mul : int32 * int32 -> int32`<br/>
    `P.ARITH{oper=P.MUL, overflow=true, kind=P.INT 32}`

  * `int32_div : int32 * int32 -> int32`<br/>
    `P.ARITH{oper=P.QUOT, overflow=true, kind=P.INT 32}`

  * `int32_mod : int32 * int32 -> int32`<br/>
    `P.ARITH{oper=P.REM, overflow=true, kind=P.INT 32}`

  * `int32_quot : int32 * int32 -> int32`<br/>
    `P.ARITH{oper=P.QUOT, overflow=true, kind=P.INT 32}`

  * `int32_rem : int32 * int32 -> int32`<br/>
    `P.ARITH{oper=P.REM, overflow=true, kind=P.INT 32}`

  * `int32_orb : int32 * int32 -> int32`<br/>
    `P.ARITH{oper=P.ORB, overflow=false, kind=P.INT 32}`

  * `int32_xorb : int32 * int32 -> int32`<br/>
    `P.ARITH{oper=P.XORB, overflow=false, kind=P.INT 32}`

  * `int32_andb : int32 * int32 -> int32`<br/>
    `P.ARITH{oper=P.ANDB, overflow=false, kind=P.INT 32}`

  * `int32_neg : word32 -> word32`<br/>
    `P.ARITH{oper=P.NEG, overflow=true, kind=P.INT 32}`

  * `int32_raw_rshift : int32 * word -> int32`<br/>
    `P.ARITH{oper=P.RSHIFT, overflow=false, kind=P.INT 32}`

  * `int32_raw_lshift : int32 * word -> int32`<br/>
    `P.ARITH{oper=P.LSHIFT, overflow=false, kind=P.INT 32}`

  * `int32_gt : int32 * int32 -> bool`<br/>
    `P.CMP{oper=P.GT, kind=P.INT 32}`

  * `int32_ge : int32 * int32 -> bool`<br/>
    `P.CMP{oper=P.GTE, kind=P.INT 32}`

  * `int32_lt : int32 * int32 -> bool`<br/>
    `P.CMP{oper=P.LT, kind=P.INT 32}`

  * `int32_le : int32 * int32 -> bool`<br/>
    `P.CMP{oper=P.LTE, kind=P.INT 32}`

  * `int32_eq : int32 * int32 -> bool`<br/>
    `P.CMP{oper=P.EQL, kind=P.INT 32}`

  * `int32_ne : int32 * int32 -> bool`<br/>
    `P.CMP{oper=P.NEQ, kind=P.INT 32}`

  * `int32_min : int32 * int32 -> int32`<br/>
    `P.INLMIN (P.INT 32)`

  * `int32_max : int32 * int32 -> int32`<br/>
    `P.INLMAX (P.INT 32)`

  * `int32_abs : word32 -> word32`<br/>
    `P.INLABS (P.INT 32)`

#### 32-bit word operations

#### 64-bit integer operations
  * `int64_add : int64 * int64 -> int64`<br/>
    `P.ARITH{oper=P.ADD, overflow=true, kind=P.INT 64}`

  * `int64_sub : int64 * int64 -> int64`<br/>
    `P.ARITH{oper=P.SUB, overflow=true, kind=P.INT 64}`

  * `int64_mul : int64 * int64 -> int64`<br/>
    `P.ARITH{oper=P.MUL, overflow=true, kind=P.INT 64}`

  * `int64_div : int64 * int64 -> int64`<br/>
    `P.ARITH{oper=P.QUOT, overflow=true, kind=P.INT 64}`

  * `int64_mod : int64 * int64 -> int64`<br/>
    `P.ARITH{oper=P.REM, overflow=true, kind=P.INT 64}`

  * `int64_quot : int64 * int64 -> int64`<br/>
    `P.ARITH{oper=P.QUOT, overflow=true, kind=P.INT 64}`

  * `int64_rem : int64 * int64 -> int64`<br/>
    `P.ARITH{oper=P.REM, overflow=true, kind=P.INT 64}`

  * `int64_orb : int64 * int64 -> int64`<br/>
    `P.ARITH{oper=P.ORB, overflow=false, kind=P.INT 64}`

  * `int64_xorb : int64 * int64 -> int64`<br/>
    `P.ARITH{oper=P.XORB, overflow=false, kind=P.INT 64}`

  * `int64_andb : int64 * int64 -> int64`<br/>
    `P.ARITH{oper=P.ANDB, overflow=false, kind=P.INT 64}`

  * `int64_neg : word32 -> word32`<br/>
    `P.ARITH{oper=P.NEG, overflow=true, kind=P.INT 64}`

  * `int64_raw_rshift : int64 * word -> int64`<br/>
    `P.ARITH{oper=P.RSHIFT, overflow=false, kind=P.INT 64}`

  * `int64_raw_lshift : int64 * word -> int64`<br/>
    `P.ARITH{oper=P.LSHIFT, overflow=false, kind=P.INT 64}`

  * `int64_gt : int64 * int64 -> bool`<br/>
    `P.CMP{oper=P.GT, kind=P.INT 64}`

  * `int64_ge : int64 * int64 -> bool`<br/>
    `P.CMP{oper=P.GTE, kind=P.INT 64}`

  * `int64_lt : int64 * int64 -> bool`<br/>
    `P.CMP{oper=P.LT, kind=P.INT 64}`

  * `int64_le : int64 * int64 -> bool`<br/>
    `P.CMP{oper=P.LTE, kind=P.INT 64}`

  * `int64_eq : int64 * int64 -> bool`<br/>
    `P.CMP{oper=P.EQL, kind=P.INT 64}`

  * `int64_ne : int64 * int64 -> bool`<br/>
    `P.CMP{oper=P.NEQ, kind=P.INT 64}`

  * `int64_min : int64 * int64 -> int64`<br/>
    `P.INLMIN (P.INT 64)`

  * `int64_max : int64 * int64 -> int64`<br/>
    `P.INLMAX (P.INT 64)`

  * `int64_abs : word32 -> word32`<br/>
    `P.INLABS (P.INT 64)`

#### 64-bit word operations

#### 64-bit real operations

### Conversions
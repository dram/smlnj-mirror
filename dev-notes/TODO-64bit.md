## TODO list for 64-bit migration

NOTE: this is a revised design document that represents the state
of play as of October 2019; see `OLD-TODO-64bit.md` for the original
notes.

### Design

The approach that we are taking is that wherever possible, we use
generic names (*e.g.*, `int` or `word`) for the default tagged
integer type.

On 32-bit machines, we currently have `Int31.int` as the default integer
type, `Int32.int` as a boxed integer type, and `Int64.int` represented as
a pair of integers.  On 64-bit machines, the default integer type is
`Int63.int`, with `Int32.int` as an *unboxed* type and `Int64.int`
as a regular boxed type without the special representation.  Currently
we use the default tagged integer type for all smaller integer types
(*e.g.*, `Word8.word` is represented as `Word63.word.int` at runtime).

For the trapping `Int32.int` operations, we currently wrap them in
code that tests for overflow (see `base/system/smlnj/init/target64-inline.sml`).
Since the AMD64 does support native 32-bit arithmetic, we should
eventually take advantage of that, but to do it right requires tracking
tagged vs. untagged values in a way that is similar to how boxed
numbers are currently handled by FLINT.

The tricky part of smaller types is dealing with overflow detection.
For now, we use a somewhat inefficient implementation that wraps the
underlying tagged-integer arithmetic with bounds checks or masking
(see `base/system/smlnj/init/target64-inline.sml`).  For example,
`Word32` arithmetic on 64-bit targets is composed with a function

	fun mask32 w = Word63.andb(w, 0wxffffffff)

and `Int32` operations are wrapped with

	fun check32 n = if (n < ~0x80000000) orelse (0x7fffffff < n)
	      then raise Overflow
	      else n

We also need to use `check32` on the `TEST` conversions, and `mask32` on
`TRUNC` conversions.

Eventually, we should support untagged numbers in the compiler (similar
to the way that unboxed 32-bit integers are supported) and rely on the
machine-code generator (MLRISC or LLVM) to introduce the masking/overflow
checking.  This optimization requires tracking tagged vs. untagged values
in a way that is similar to how boxed numbers are currently handled by FLINT.

### Outstanding 64-bit issues

The following is a list of the known places in the implementation where
there are assumptions about the target word size (these have
been marked in the source code with the comment tag "`64BIT:`").
Some of these files are not actually used, so we have marked them
as **DONE**, even though they are not changed.

#### Runtime system

  * `runtime/gc/blast-gc.c` <br/>
    Merge the handling of `DTAG_raw` and `DTAG_raw64` on 64-bit machines.

  * `runtime/gc/minor-gc.c` <br/>
    Merge the handling of `DTAG_raw` and `DTAG_raw64` on 64-bit machines.

  * `runtime/c-libs/dl/dlclose.c` <br/>
    Use the abstract `c_pointer` type to represent runtime-system pointers.

  * `runtime/c-libs/dl/dlopen.c` <br/>
    Use the abstract `c_pointer` type to represent runtime-system pointers.

  * `runtime/c-libs/dl/dlsym.c` <br/>
    Use the abstract `c_pointer` type to represent runtime-system pointers.

#### Basis

  * `system/smlnj/init/pervasive.sml` <br/>
    The conversions from word to real and `intbound` are incorrect for 64-bit targets
    **[DONE; 110.89]**

  * `system/Basis/Implementation/math-common.sml` <br/>
    The `floor` function is 32-bit specific.
    **[DONE; 110.89]**

#### Compiler

  * `compiler/CodeGen/cpscompile/cps-c-calls.sml` <br/>
    Alignment adjustment for 64-bit values is only required on 32-bit targets.

  * `compiler/CodeGen/cpscompile/invokegc.sml` <br/>
    Check semantics to make sure the code makes sense for 64-bits.

  * `compiler/CodeGen/cpscompile/memDisambig.sml` <br/>
    this file is no longer used, but has 64-bit dependences. <br/>
    This file is no longer used in the simplified code generator.
    **[DONE; 110.89]**

  * `compiler/CodeGen/cpscompile/smlnj-pseudoOps.sml` <br/>
    Use 32-bit entries for jump tables on 64-bit targets.

  * `compiler/CodeGen/main/default-machinespec-fn.sml` <br/>
    Can simplify by assuming that the size of ML values will always be the same
    as the size of a native pointer.  This property holds now, since we no longer
    support the DEC Alpha.

  * `compiler/CPS/clos/closure.sml` <br/>
    raw untagged data is split into 32-bit and 64-bit records on 32-bit machines.

  * `compiler/CPS/convert/convert.sml` <br/>
    various assumptions about the size of boxed ints when converting
    raw C calls.

  * `compiler/CPS/main/new-literals.sml` </br>
    Remove compiler-bug workaround (once the compiler bug has been fixed).

  * `compiler/FLINT/opt/abcopt.sml` <br/>
    this file is no longer used, but has 64-bit dependences.
    **[DONE]**

  * `compiler/FLINT/reps/rttype.sml` <br/>
    there is a type code for 32-bit numbers (`tcode_int32`); it can probably
    be replaced by `tcode_void`.
    **[DONE]**

  * `compiler/MiscUtil/print/ppobj.sml` <br/>
    there is a mysterious test for `int32Tyc`/`word32Tyc` in the function
    `isUbxTy`.  What should this function do on 64-bit targets?

#### MLRISC

  * `amd64/mltree/amd64-gen.sml` <br/>
    Remove compiler-bug workaround (once the compiler bug has been fixed).

#### Other SML code

  * `nlffi` needs to be ported to 64-bit

  * the `Rand` and `Random` structures in the **SML/NJ Library** could be
    reimplemented to take advantage of 64-bit arithmetic.

### Known bugs


1. `Int32` arithmetic may not be generating `Overflow` in some cases (not sure if this
   is really a bug)

2. There are issues with `IntInf` literals.  For example:

	- 0x40000000:IntInf.int;
	val it = 9223372035781033984 : IntInf.int
	- Unsafe.IntInf.concrete it;
	val it = BI {digits=[0wx7FFFFFFFC0000000],negative=false} : Unsafe.IntInf.rep
	- Unsafe.IntInf.concrete(IntInf.<<(1, 0w30));
	val it = BI {digits=[0wx40000000],negative=false} : Unsafe.IntInf.rep
	- 0x3fffffff:IntInf.int;
	val it = 1073741823 : IntInf.int

    I've currently added workarounds to `MLRISC/amd64/mltree/amd64-gen.sml` and
    `base/compiler/CPS/main/new-literals.sml`.  The hack is to define the lower
    and upper-bounds for 32-bit values by converting from Word64.

3. `Word8.toIntX (Word8.~>> (Word8.fromInt ~128, Word.fromInt 8))` evaluates to
    `~257` instead of `~1`.
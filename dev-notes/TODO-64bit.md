## TODO list for 64-bit migration

NOTE: this is a revised design document that represents the state
of play as of May 2019; see `OLD-TODO-64bit.md` for the original
notes.

### Design

The approach that we are taking is that wherever possible, we use
generic names (*e.g.*, `int` or `word`) for the default tagged
integer type.

On 32-bit machines, we currently have `Int31.int` as the default integer
type, `Int32.int` as a boxed integer type, and `Int64.int` represented as
a pair of integers.  The plan is to make `Int63.int` the default integer
type on 64-bit machines, with `Int32.int` as an *unboxed* type and `Int64.int`
as a regular boxed type without the special representation.  Currently
we use the default tagged integer type for all smaller integer types
(*e.g.*, `Int8.int` is represented as `Int31.int` at runtime).

The tricky part of smaller types is dealing with overflow detection.
For now, we are going to use a somewhat inefficient implementation
that wraps the underlying tagged-integer arithmetic with bounds checks
or masking.  For example, Word32 arithmetic on 64-bit targets will be
composed with a function

	fun mask32 w = Word63.andb(w, 0wxffffffff)

and Int32 operations will be wrapped with

	fun check32 n = if (n < ~0x80000000) orelse (0x7fffffff < n)
	      then raise Overflow
	      else n

We will also need to use check32 on the TEST conversions, and mask32 on
TRUNC conversions.

Eventually, we should support untagged numbers in the compiler (similar
to the way that unboxed 32-bit integers are supported) and rely on the
machine-code generator (MLRISC or LLVM) to introduce the masking/overflow
checking.

### Outstanding 64-bit issues

The following is a list of the known places in the implementation where
there are assumptions about the target word size (these have
been marked in the source code with the comment tag "`64BIT:`").
Some of these files are not actually used, so we have marked them
as **DONE**, even though they are not changed.

  * `compiler/CodeGen/cpscompile/invokegc.sml` <br/>
    check semantics to make sure the code makes sense for 64-bits.

  * `compiler/CodeGen/cpscompile/limit.sml` <br/>
    various assumptions about the size of 64-bit floats and heap alignment. <br/>
    **[DONE; 110.92]**

  * `compiler/CodeGen/cpscompile/memAliasing.sml` <br/>
    assumption that `RK_RAW64BLOCK` records take twice as much memory. <br/>
    This file is no longer used in the simplified code generator.
    **[DONE; 110.89]**

  * `compiler/CodeGen/cpscompile/memDisambig.sml` <br/>
    this file is no longer used, but has 64-bit dependences. <br/>
    This file is no longer used in the simplified code generator.
    **[DONE; 110.89]**

  * `compiler/CodeGen/cpscompile/spill-new.sml` <br/>
    the `rkToCty` function may need to be changed.
    **[DONE; 110.89]**

  * `compiler/CodeGen/main/mlrisc-gen-fn.sml` (also see `mlriscGen.sml`)<br/>
    there may be issues with `RAWRECORD`.

  * `compiler/CodeGen/main/object-desc.sml` <br/>
    codes for the various array/vector headers need to be reworked (also in the
    runtime system)
    **[DONE; 110.90]**

  * `compiler/CodeGen/x86/x86CG.sml` <br/>
    may not be an issue, since the **x86** is a 32-bit target.

  * `compiler/CPS/clos/closure.sml` <br/>
    raw untagged data is split into 32-bit and 64-bit records on 32-bit machines.

  * `compiler/CPS/convert/convert.sml` <br/>
    various assumptions about the size of boxed ints when converting
    raw C calls.

  * `compiler/CPS/main/literals.sml` <br/>
    this file should be replaced with an implementation of the new literal
    encoding, which supports 64-bit integer data

  * `compiler/FLINT/reps/rttype.sml` <br/>
    there is a type code for 32-bit numbers (`tcode_int32`); it can probably
    be replaced by `tcode_void`.

  * `compiler/FLINT/opt/abcopt.sml` <br/>
    this file is no longer used, but has 64-bit dependences.
    **[DONE]**

  * `compiler/MiscUtil/print/ppobj.sml` <br/>
    there is a mysterious test for `int32Tyc`/`word32Tyc` in the function
    `isUbxTy`.

  * `compiler/Semant/prim/primop-bindings.sml` <br/>
    Will need to add target-specific conversions once we understand what is
    required.  Note that these will have to be added to the compiler **before**
    we can attempt to cross compile.

  * `system/Basis/Implementation/num-format.sml` <br/>
    should support formatting of 64-bit words and integers on all platforms
    **[DONE; 110.88]**

  * `system/Basis/Implementation/num-scan.sml` <br/>
    should support scanning of 64-bit words and integers on all platforms
    **[DONE; 110.88]**

  * `system/Basis/Implementation/Posix/posix-filesys.sml` <br/>
    the `statrep` type has both `Int32.int` and `int` fields
    **[DONE; 110.89]**

  * `system/Basis/Implementation/Posix/posix-io.sml` <br/>
    the `flock_rep` type has `Int.int` fields; also `lseek` probably should use the
    `Position.int` type for file offsets.
    **[DONE; 110.89]**

  * `system/Basis/Implementation/real64.sml` <br/>
    explicit `Word31.word` to `real` conversion
    **[DONE; 110.92]**

  * `system/Basis/Implementation/Target32Bit/word64.sml` <br/>
    should use 64-bit functions from `NumFormat` and `NumScan` (see above)
    **[DONE; 110.88]**

  * `system/Basis/Implementation/Unsafe/object.sml` <br/>
    lots of assumptions about the sizes and runtime representations of values.
    This file is a candidate for being moved to the target-specific
    directories.

  * `system/Basis/Implementation/Win32/win32-general.sml` <br/>
    the `HANDLE` type is 64-bits on 64-bit machines; use the abstract
    `c_pointer` type.
    **[DONE; 110.90]**

  * `system/smlnj/init/built-in32.sml` <br/>
    Need to switch `LargeWord` from `Word32` to `Word64` on all platforms.
    **[DONE; 110.89]**

  * `system/smlnj/init/target64-core-intinf.sml` <br/>
    **[DONE; 110.91]**

  * `system/smlnj/init/target64-core.sml` <br/>
    Needs some work.

  * `system/smlnj/init/pervasive.sml` <br/>
    explicit `Word31.word` and `Int32.int` to `real` conversions
    **[DONE; 110.92]**

  * `smlnj-lib/Util/random.sml` <br/>
    Uses the `Word31` structure.

  * `nlffi/lib/linkage-dlopen.sml` <br/>
    switch to using abstract `c_pointer` type for handles.

  * `nlffi/lib/linkage.sig` <br/>
    switch to using abstract `c_pointer` type for handles.

In addition to the above issues, there are a few more changes that we can make
to smooth the differences between the 32-bit and 64-bit targets.

  * bind `Position` to `Int64`.  While this will cost some performance, it
    addresses several outstanding open bugs.  This change will require an
    overhaul of the C-library interface.
    **[DONE; 110.89]**

  * bind `FixedInt` to `Int64` on all targets.
    **[DONE; 110.89]**

  * bind `LargeWord` to `Word64` on all targets.
    **[DONE; 110.89]**

### Runtime System

The runtime system is largely 64-bit clean, since it has been used on the
Alpha, but there are a few places where additional work is required.

  * `runtime/c-libs/dl/dlclose.c` <br/>
    Use the abstract `c_pointer` type to represent runtime-system pointers.

  * `runtime/c-libs/dl/dlopen.c` <br/>
    Use the abstract `c_pointer` type to represent runtime-system pointers.

  * `runtime/c-libs/dl/dlsym.c` <br/>
    Use the abstract `c_pointer` type to represent runtime-system pointers.

  * `runtime/c-libs/win32-filesys/win32-filesys.c` <br/>
    The `HANDLE` type will be 64-bits on 64-bit targets; use the abstract
    `c_pointer` type.
    **[DONE; 110.90]**

  * `runtime/c-libs/win32-io/win32-io.c` <br/>
    The `HANDLE` type will be 64-bits on 64-bit targets; use the abstract
    `c_pointer` type.
    **[DONE; 110.90]**

  * `runtime/c-libs/win32-process/win32-process.c` <br/>
    The `HANDLE` type will be 64-bits on 64-bit targets; use the abstract
    `c_pointer` type.
    **[DONE; 110.90]**

  * `runtime/gc/blast-gc.c` <br/>
    `DTAG_raw` and `DTAG_raw64` can be handled the same way on 64-bit targets.

  * `runtime/gc/build-literals.c` <br/>
    RAW32 padding on 64-bit targets

  * `runtime/gc/export-heap.c`
    need a BIBOP for mapping between memory and file addresses

  * `runtime/gc/import-heap.c`
    need a BIBOP for mapping between memory and file addresses

  * `runtime/gc/major-gc.c` <br/>
    Need modifications for the two-level BIBOP.
    **[DONE; 110.91]**

  * `runtime/gc/minor-gc.c` <br/>
    `DTAG_raw` and `DTAG_raw64` can be handled the same way on 64-bit targets.
    Also, need modifications for the two-level BIBOP.
    **[DONE; 110.91]**

  * `runtime/include/cntr.h` <br/>
    can use 64-bit integers for counters (might be able to do so on 32-bit machines
    too, when int64_t is available)
    **[DONE; 110.89]**

  * `runtime/mach-spec/AMD64-prim.asm` <br/>
    problems with the `assyntax64.h` macros
    **[DONE; 110.91]**


### Code Generation

Once the above issues have been addressed, we should be ready to work on
code generation for the AMD64 target.


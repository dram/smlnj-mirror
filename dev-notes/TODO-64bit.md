## TODO list for 64-bit migration

NOTE: this is a revised design document that represents the state
of play as of May 2019; see `OLD-TODO-64bit.md` for the original
notes.

### Design

The approach that we are taking is that wherever possible, we use
generic names (*e.g.*, `int` or `word`) for the default tagged
integer type.

On 32-bit machines, we currently have `Int31.int` as the default integer type, `Int32.int`
as a boxed integer type, and `Int64.int` represented as a pair of integers.
The plan is to make `Int63.int` the default integer type on 64-bit machines, with `Int32.int`
as an *unboxed* type and `Int64.int` as a regular boxed type without the special representation.
Currently we use the default tagged integer type for all smaller integer types
(*e.g.*, `Int8.int` is represented as `Int31.int` at runtime).

### Outstanding 64-bit issues

The following is a list of the known places in the implementation where
some change will be required to support 64-bit targets.  These have
been marked in the source code with the comment tag "`64BIT:`."

  * `compiler/CodeGen/cpscompile/memAliasing.sml` <br/>
    assumption that `RK_RAW64BLOCK` records take twice as much memory

  * `compiler/CodeGen/cpscompile/memDisambig.sml` <br/>
    this file is no longer used, but has 64-bit dependences

  * `compiler/CodeGen/cpscompile/spill-new.sml` <br/>
    the `rkToCty` function may need to be changed

  * `compiler/CodeGen/main/mlriscGen.sml` <br/>
    various code-generation patterns that match fixed sizes (*e.g.*, `31` or `32`).

  * `compiler/CodeGen/main/object-desc.sml` <br/>
    codes for the various array/vector headers need to be reworked (also in the
    runtime system)

  * `compiler/CodeGen/x86/x86CG.sml` <br/>
    may not be an issue, since the **x86** is a 32-bit target.

  * `compiler/CPS/clos/closure.sml` <br/>
    raw untagged data is split into 32-bit and 64-bit records on 32-bit machines.

  * `compiler/CPS/convert/convert.sml` <br/>
    various assumptions about the size of boxed ints

  * `compiler/CPS/main/literals.sml` <br/>
    this file should be replaced with an implementation of the new literal
    encoding, which supports 64-bit integer data

  * `compiler/FLINT/opt/abcopt.sml` <br/>
    this file is no longer used, but has 64-bit dependences

  * `compiler/MiscUtil/print/ppobj.sml` <br/>
    there is a mysterious test for `int32Tyc`/`word32Tyc` in the function
    `isUbxTy`.

  * `compiler/Semant/prim/primop-bindings.sml` <br/>
    Will need to add target-specific conversions once we understand what is required.

  * `system/Basis/Implementation/num-format.sml` <br/>
    should support formatting of 64-bit words and integers on all platforms

  * `system/Basis/Implementation/num-scan.sml` <br/>
    should support scanning of 64-bit words and integers on all platforms

  * `system/Basis/Implementation/Posix/posix-filesys.sml` <br/>
    the `statrep` type has both `Int32.int` and `int` fields

  * `system/Basis/Implementation/Posix/posix-io.sml` <br/>
    the `flock_rep` type has `Int.int` fields; also `lseek` probably should use the
    `Position.int` type for file offsets.

  * `system/Basis/Implementation/real64.sml` <br/>
    explicit `Word31.word` to `real` conversion

  * `system/Basis/Implementation/Target32Bit/word64.sml` <br/>
    should use 64-bit functions from `NumFormat` and `NumScan` (see above)

  * `system/Basis/Implementation/Unsafe/object.sml` <br/>
    lots of assumptions about the sizes and runtime representations of values.
    This file is a candidate for being moved to the target-specific
    directories.

  * `system/smlnj/init/built-in32.sml` <br/>
    Need to switch `LargeWord` from `Word32` to `Word64` on all platforms.

  * `system/smlnj/init/core-intinf.sml` <br/>
    Assumes 32-bit target.

  * `system/smlnj/init/core.sml` <br/>
    uses 32-bit integer equality to compare raw data pointers.

  * `system/smlnj/init/pervasive.sml` <br/>
    explicit `Word31.word` and `Int32.int` to `real` conversions

  * `runtime/gc/blast-gc.c` <br/>
    `DTAG_raw` and `DTAG_raw64` can be handled the same way on 64-bit targets.

  * `runtime/gc/build-literals.c` <br/>
    sequence headers

  * `runtime/gc/minor-gc.c` <br/>
    `DTAG_raw` and `DTAG_raw64` can be handled the same way on 64-bit targets.

  * `runtime/include/cntr.h` <br/>
    can use 64-bit integers for counters (might be able to do so on 32-bit machines
    too, when int64_t is available)

In addition to the above issues, there are a few more changes that we can make
to smooth the differences between the 32-bit and 64-bit targets.

  * bind `Position` to `Int64`.  While this will cost some performance, it
    addresses several outstanding open bugs.  This change will require an
    overhaul of the C-library interface.

  * bind `FixedInt` to `Int64` on all targets.

  * bind `LargeWord` to `Word64` on all targets.

### Code Generation

Once the above issues have been addressed, we should be ready to work on
code generation for the AMD64 target.


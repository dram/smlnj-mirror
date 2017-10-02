## TODO list for 64-bit migration

### Compiler issues

All paths are relative the the `base` module.

* **[DONE]**
  Add `amd64` as an architecture that CM knows about<br/>
  Files:
    - `cm/main/specific-symval-fn.sml`

* The assumption that 31-bit integers are the default `int` type is
  pervasive in the primitive operations.  A major example of this
  assumption are the primitive operations for converting between integer
  representations.

* The `Word8` operations are currently being implemented using special
  renamed versions of the `Int31` primitive operations (*e.g.*, `i31add_8`).<br/>
  Files:
    - `compiler/DEVNOTES/Primops/primop-list`
    - `compiler/FLINT/trans/primopmap.sml`
    - `compiler/Semant/statenv/prim.sml`
    - `compiler/Semant/statenv/primoptypemap.sml`

* For the 64-bit target, the `Int32.int` type can be represented as an unboxed
  tagged integer.  This means that the CPS `i32wrap` and `i32unwrap` operations
  should be generalized in some way.<br/>
  **Note:** there are existing CPS operations `iwrap` and `iunwrap` defined, but
  they are not implemented in the code generator and it is unclear what purpose
  they serve.  Perhaps they should be the generic boxing operations?</br>
  Files:
    - `compiler/FLINT/cps/cps.sig`
    - `compiler/FLINT/cps/cps.sml`
    - `compiler/FLINT/cps/convert.sml`
    - `compiler/FLINT/cps/cpstrans.sml`
    - `compiler/FLINT/cps/ppcps.sml`
    - `compiler/FLINT/cpsopt/contract.sml`
    - `compiler/FLINT/clos/cps-split.sml`
    - `compiler/CodeGen/cpscompile/limit.sml`
    - `compiler/CodeGen/cpscompile/memAliasing.sml`
    - `compiler/CodeGen/cpscompile/memDisambig.sml`
    - `compiler/CodeGen/cpscompile/spill-new.sml`
    - `compiler/CodeGen/cpscompile/mlriscGen.sml`

* The conversion from PLambda to FLINT has a 31-bit assumption
  Files:
    - `compiler/FLINT/plambda/flintnm.sml`

* The representation of FLINT assumes four kinds of integer literals: `INT`, `INT32`,
  `WORD`, and `WORD32`.  These are reduced to two kinds when converting to CPS (`INT`
  and `INT32`).  The FLINT representation of constructors in switch statements has
  a similar breakdown.  It is not clear if the word vs. int distinction is important
  for FLINT, but we should generalize the names to represent tagged vs. untagged integers
  and use `IntInf.int` to represent the actual values.  We may also want to add a
  size attribute, so that we can support heap-allocated literals (e.g., `Int64.int` on
  32-bit machines or `IntInf.int`) in the future.</br>
  Files:
    - `compiler/FLINT/cps/convert.sml`
    - `compiler/FLINT/flint/chkflint.sml`
    - `compiler/FLINT/flint/flint.sig`
    - `compiler/FLINT/flint/flint.sml`
    - `compiler/FLINT/flint/ppflint.sml`
    - `compiler/FLINT/opt/fcontract.sml`
    - `compiler/FLINT/opt/lift.sml`
    - `compiler/FLINT/opt/recover.sml`
    - `compiler/FLINT/plambda/chkplexp.sml`
    - `compiler/FLINT/plambda/flintnm.sml`
    - `compiler/FLINT/plambda/plambda.sig`
    - `compiler/FLINT/plambda/plambda.sml`
    - `compiler/FLINT/plambda/pplexp.sml`
    - `compiler/FLINT/plambda/reorder.sml`
    - `compiler/FLINT/plambda/rpplexp.sml`
    - `compiler/FLINT/trans/translate.sml`

* The `CPS.cty` datatype needs to be extended to include an `INT64t` constant
  (or perhaps an `INTt of int` constructor).<br/>
  **Note:** there is an interaction between this issue and the wrapping/unwrapping
  of 32-bit integers discussed above.<br/>
  Files:
    - `compiler/FLINT/cps/cps.sig`
    - `compiler/FLINT/cps/cps.sml`
    - `compiler/FLINT/cps/convert.sml`
    - `compiler/FLINT/cps/cpstrans.sml`
    - `compiler/FLINT/cpsopt/contract.sml`
    - `compiler/FLINT/cpsopt/contract.sml`
    - `compiler/FLINT/clos/infcnv.sml`
    - `compiler/FLINT/clos/cps-split.sml`
    - `compiler/CodeGen/cpscompile/cps-c-calls.sml`
    - `compiler/CodeGen/cpscompile/invokegc.sml`
    - `compiler/CodeGen/cpscompile/mlriscGen.sml`
    - `compiler/CodeGen/cpscompile/smlnj-gctype.sml`

* Support for generating switch code (`do_switch_gen`) assume 32-bit words.<br/>
  Files:
    - `base/compiler/FLINT/cps/convert.sml`
    - `base/compiler/FLINT/cps/switch.sml`

* Code generation from CPS assumes 32-bit words in a number of places.<br/>
  Files:
    - `compiler/CodeGen/cpscompile/cps-c-calls.sml`
    - `compiler/CodeGen/cpscompile/invokegc.sml`
    - `compiler/CodeGen/cpscompile/limit.sml`
    - `compiler/CodeGen/cpscompile/spill-new.sml`

* Should the default machine-spec word size be changed to 64?  Also, what (if any)
  relation is there between the `Target` module and the machine specification?<br/>
  Files:
    - `base/compiler/CodeGen/main/machspec.sml`

* The `base/compiler/CodeGen/amd64` code generator will probably need some cleanup,
  since it was written for a pseudo-64-bit implementation.

* To support cross compiling from 32-bit hosts to 64-bit targets, we will need to
  replace fixed-precision representations of literals with IntInf.int.  This change
  has already been made for the CPS representation, but it will also need to be
  done for the FLINT representation, and for some of the CPS-related passes.</br>
    - `base/compiler/FLINT/cps/switch.sml`
    - `base/compiler/FLINT/main/literals.sml`
    - `base/compiler/CodeGen/main/mlriscGen.sml`

### MLRISC issues

There are some issues with the current MLRISC support for AMD64.

* The binary instruction encoding in the `amd64.mdl` was never completed

* The instruction selection mechanism in `amd64/mltree/amd64.sml` is based on
  the register-poor `x86`, instead of a RISC-type architecture.  This choice
  may explain the poor floating-point performance for this architecture in
  the Manticore compiler.

### Basis Library issues

* Default integer types: `Int31.int` for 32-bit machines and `Int63.int` for 64-bit
  machines.

* Implementation of `IntInf` assumes 32-bit integers.<br/>
  Files:
    - system/smlnj/init/core-intinf.sml

* The `InlineT` module may need to be conditionally compiled based on word size,
  since it has `Int31` and `Word31` submodules.  Alternatively, we can change
  these to `TaggedInt` and `TaggedWord` modules that are resolved to 31-bit
  integers on 32-bit targets and 63-bit integers on 64-bit targets.<br/>
  Files:
    - system/smlnj/init/built-in.sml

### Runtime system issues

The runtime system has been 64-bit clean for many years, but it has never been
used for targets where the ML word size is 64-bits, so there could be some
problems.

To support a 64-bit address space, we will need to implement the multi-level
BIBOP support.  We should probably increase the size of the `BIBOP_PAGE_SZB`
to 256K (18 bits), but we will still need a 2-level table to cover a 48-bit
virtual address space.  An alternative might be some form of hashing.
*UPDATE* experiments show that the two-level table works best, but we
replace bibop tests in the minor-gc with address-range tests.

Object descriptors can be left as is for now (low 7 bits), which will allow
the length field to support much bigger objects.  We will want to add a
`SEQ_word64` kind tag for vectors/arrays.

### SML/NJ Library issues

There are a few library modules that assume 32-bit integers.

  * The various hash-table modules round up table size assuming that the
    size is a 31-bit integer.<br/>
    Files:
      - `Util/hash-set-fn.sml`
      - `Util/hash-table-rep.sml`
      - `Util/hash-table.sml`
      - `Util/hash-set-fn.sml`

  * The `Rand` and `Random` modules are 31-bit specific.<br/>
    Files:
      - `Util/rand.sml`
      - `Util/random.sml`

### General discussion

Ideally, we can devise a single way to parameterize the compiler on target sizes
from front to back.

It would also be nice to address support for 32-bit floats at the same time, since
many of the same parts of the compiler will need to be touched.

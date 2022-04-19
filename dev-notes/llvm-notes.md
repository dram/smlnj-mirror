# Notes on using LLVM for code generation in SML/NJ

This document describes the plan for replacing the **MLRISC** backend
with an **LLVM** backend.  It includes instructions for how to patch
**LLVM** to support the "Jump with Arguments" (**JWA**) calling convention that
was developed to support continuation-passing, closure-passing
style (see [Compiling with Continuations and LLVM](https://doi.org/10.4204/EPTCS.285.5)
for more details.

## Basic architecture

We want to preserve the "on-the-fly" code generation behavior that the
system currently uses.  To this end, we plan to link the runtime system
with a **LLVM** code generator.  We will use **ASDL** to communicate between
the **SML** backend and the runtime system code generator.

In more detail, we plan to replace the current code generator, which
translates a first-order variant of the **CPS** IR to **MLRISC** with
a two stage translation.  The first stage will translate the first-order
**CPS** to a simpler, and more explicit, control-flow-graph IR (**CFG**).
To validate this translation, we will write a **CFG** to **MLRISC**
code generator.  Eventually, this code generator will be replaced with
by an **LLVM** based code generator written in C++.

## CFG IR

The **MLRISC** code generator takes the first-order CPS IR and generates
**MLRISC** expression trees and statements that are then turned into
machine code by the **MLRISC** library.  My original plan was to use
ASDL to communicate the first-order CPS IR to the runtime and then
compile that into **LLVM** using a similar strategy as is currently done
for **MLRISC**.  This approach has a couple of drawbacks, however:

* The CPS IR includes a number of syntactic forms that are no longer
  present in the first-order form, but which would have to be handled
  by the C++ code.

* The translation to **MLRISC** relies on meta data that is outside the
  IR (*e.g.*, a mapping from function names to calling conventions).
  Either this meta data would have to be recomputed in the C++ code
  generator or transmitted to the runtime.

As an alternative, we propose to add a new, lower-level IR, called
**CFG** (for Control-Flow Graph) that will sit between the first-order
CPS IR and code generation.  This IR will make explicit features like
calling conventions, GC tests, tagged arithmetic, *etc*.  As a first
step, we propose to implement an **MLRISC** code generator for this IR.

The **CFG** IR has two main types:

* Expressions (`exp`), which are trees that define computations.  These roughly
  correspond to the **MLRISC** `mltree` type and the **LLVM** `Value`
  type.

* Statements (`stm`), which are extended basic blocks with operations
  for allocation, updates, and control flow.

## Code generation details

This section discusses how various aspects of the current **MLRISC**
code generator will map to **LLVM**.

### SML/NJ's Runtime Model

The **SML/NJ** system does not use a conventional stack for executing
**SML** code.  Instead, the runtime system allocates a single frame for
executing **SML** code and all internal function calls are implemented
as tail calls with an explicit return-continuation closure as an argument.
The frame allocated by the runtime system is large enough to support
the spilling of up to 1,024 variables.  It also contains some additional
linkage information and, on some architectures, virtual registers.

### Calling conventions

### Stack frame access

Because the generated code is not directly linked to the runtime system,
we use reserved locations in the stack frame.

*LLVM* provides the intrinsic function [`@llvm.read_register`](https://llvm.org/docs/LangRef.html#llvm-read-register-and-llvm-write-register-intrinsics)
that can be used to read the value of the stack pointer.  We may be able to use
this intrinsic to access those values that are in the stack frame.

### LLVM Intrinsics

There are a number of primitive operations that will have to be mapped to
**LLVM** intrinsics.  These include support for [overflow
detection](http://llvm.org/docs/LangRef.html#arithmetic-with-overflow-intrinsics)
in integer arithmetic and various floating-point operations.

## Patching LLVM

**LLVM** needs to be modified to support the **JWA** calling convention
that we use.  The modifications are fairly simple, and are
described for **LLVM** 10.0.x below, where `$LLVM` denotes the root of
the **LLVM** source tree.

There is some documentation about how to specify calling conventions in
["Writing an LLVM Backend"](https://llvm.org/docs/WritingAnLLVMBackend.html)

### `CallingConv.h`

The file `$LLVM/include/llvm/IR/CallingConv.h` assigns integer codes for
the various calling conventions.  In **LLVM** 10.0.x, the last number assigned
is `19`, so we use `20` for **JWA**.  Add the following code to the file just
before the first target-specific code (which will be `64`).

``` c++
    /// JWA - "Jump With Arguments" is a calling convention that requires the
    /// use of registers for parameter passing. It is designed for language
    /// implementations that do not use a stack, however, it will not warn
    /// if there are not enough registers for a given function. The lack of
    /// warning is needed in order to properly utilize musttail calls as
    /// jumps because they are picky about parameters.
    JWA = 20,
```

### Supporting `jwacc` in LLVM Assembler

To support using the symbolic name `jwacc` for the calling convention
in LLVM assembler (useful for debugging the code generator, but not
required for the *SML/NJ* compiler), we must modify the parser and
pretty printer for LLVM assembly code.

#### `AsmParser/LLTToken.h`

Add `kw_jwacc` as a token in `$LLVM/lib/AsmParser/LLTToken.h` (I added it following
`kw_tailcc`.

#### `AsmParser/LLLexer.cpp`

Add the following keyword definition

``` c++
  KEYWORD(jwacc);
```

to the file `$LLVM/lib/AsmParser/LLLexer.cpp`.

#### `AsmParser/LLParser.cpp`

Add the following case

``` c++
  case lltok::kw_jwacc:          CC = CallingConv::JWA; break;
```

to the function `LLParser::ParseOptionalCallingConv`
in the file `$LLVM/lib/AsmParser/LLLexer.cpp`.

#### `IR/AsmWriter.cpp`

The file `$LLVM/lib/IR/AsmWriter.cpp` implements printing of the
LLVM assembly code.  Modify it by adding the case

``` c++
  case CallingConv::JWA:           Out << "jwacc"; break;
```

to the function `PrintCallingConv`.

### `Target/X86`

To support the **JWA** convention on the `X86`, we need to modify a number
of files in the directory `$LLVM/lib/Target/X86/`.

#### `X86CallingConv.td`

The file `$LLVM/lib/Target/X86/X86CallingConv.td` describes the calling
conventions for the *x86* and *x86-64* (aka *amd64*) architectures
using **LLVM**'s [**TabgeGen**](https://llvm.org/docs/TableGen/) language.
We need to add several chunks of code to the file.  I added the first
just before the definition for `RetCC_X86_32`.

```
// The JWA calling convention for x86_64. Note that this is
// also used as the return convention in order to implement call/cc.
// True returns are not the norm. We _never_ use the stack.
def CC_X86_64_JWA : CallingConv<[

  // Promote i8/i16/i32 arguments to i64.
  CCIfType<[i8, i16, i32], CCPromoteToType<i64>>,

  // The only registers we skip are RAX and RSP.

  // registers are ordered according to SML/NJ convention as follows:
  // alloc, limit, store, link, clos, cont, misc0, ..., misc3, arg, ... misc6
  CCIfType<[i64],
    CCAssignToReg<[
        RDI, R14, R15,  // ALLOC, LIMIT, STORE
        R8, R9, RSI,    // LINK, CLOS, CONT
        RBX, RCX, RDX,  // MISC01-MISC2 (CALLEE SAVES)
        RBP, R10, R11,  // ARG, MISC3, MISC4,
        R12, R13        // MISC5, MISC6
    ]>>,

  // Use as many vector registers as possible!
  // NOTE: we are assuming that SSE is never disabled for JWA, since it
  // would break upstream assumptions in the SML/NJ compiler.
  CCIfType<[f32, f64, v16i8, v8i16, v4i32, v2i64, v4f32, v2f64],
    CCAssignToReg<[
      XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7,
      XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15]>>,

  // AVX (256-bit) vector registers
  CCIfType<[v32i8, v16i16, v8i32, v4i64, v8f32, v4f64],
    CCIfSubtarget<"hasAVX()",
      CCAssignToReg<[
	YMM0, YMM1, YMM2, YMM3, YMM4, YMM5, YMM6, YMM7,
	YMM8, YMM9, YMM10, YMM11, YMM12, YMM13, YMM14, YMM15]>>>,

  // AVX-512 (512-bit) vector registers
  CCIfType<[v64i8, v32i16, v16i32, v8i64, v16f32, v8f64],
    CCIfSubtarget<"hasAVX512()",
      CCAssignToReg<[
	ZMM0, ZMM1, ZMM2, ZMM3, ZMM4, ZMM5, ZMM6, ZMM7,
	ZMM8, ZMM9, ZMM10, ZMM11, ZMM12, ZMM13, ZMM14, ZMM15,
	ZMM16, ZMM17, ZMM18, ZMM19, ZMM20, ZMM21, ZMM22, ZMM23,
	ZMM24, ZMM25, ZMM26, ZMM27, ZMM28, ZMM29, ZMM30, ZMM31]>>>

]>;
```

The convention definition above specifies the decision procedure for assigning
each argument of a function, from left-to-right, based on the type of the
argument.  The first directive says to treat all `i8`/`i16`/`i32` values
as `i64`, since x86-64 general-purpose registers can handle any of these
values via their narrower aliases.  Then, the next directive says if the
argument can be treated as an i64, use the first available register from
the given list.  For calls that do not make use of, say, the standard
return-continuation register, an **LLVM** `undef` value can be passed in to
that argument position in the **LLVM** IR in order to effectively skip-over
that register during this convention decision procedure.

Because the JWA convention does not really return (instead, we use **JWA**
to invoke a return continuation), we delegate the return convention to
the calling convention.  The following lines need to be added to the
definition of the "root return-value convention for the X86-64 backend"
(`RetCC_X86_64`):

```
  // Handle JWA calls.
  CCIfCC<"CallingConv::JWA", CCDelegateTo<CC_X86_64_JWA>>,
```

The following line needs to be added to the definition of the
"root argument convention for the X86-64 backend" (`CC_X86_64`):

```
CCIfCC<"CallingConv::JWA", CCDelegateTo<CC_X86_64_JWA>>,
```

#### `X86FastISel.cpp`

In the file `$LLVM/lib/Target/X86/X86FastISel.cpp`, the function
`computeBytesPoppedByCalleeForSRet` needs to be modified to
recognize the **JWA** convention.

To the code
``` c++
  if (CC == CallingConv::Fast || CC == CallingConv::GHC ||
      CC == CallingConv::HiPE || CC == CallingConv::Tail)
    return 0;
```
add a test for **JWA** (`CC == CallingConv::JWA`).

#### `X86ISelLowering.cpp`

In the file `$LLVM/lib/Target/X86/X86ISelLowering.cpp`, we need to add
a check for **JWA** to the function `canGuaranteeTCO`.

``` c++
  return (CC == CallingConv::Fast || CC == CallingConv::GHC ||
          CC == CallingConv::X86_RegCall || CC == CallingConv::HiPE ||
          CC == CallingConv::HHVM || CC == CallingConv::Tail ||
          CC == CallingConv::JWA);
```

#### `X86RegisterInfo.cpp`

In the file `$LLVM/lib/Target/X86/X86RegisterInfo.cpp`, we need to add
cases for **JWA** to the method `getCalleeSavedRegs`:
``` c++
  case CallingConv::GHC:
  case CallingConv::HiPE:
  case CallingConv::JWA:
    return CSR_NoRegs_SaveList;
```
and to the method `getCallPreservedMask`
``` c++
  case CallingConv::GHC:
  case CallingConv::HiPE:
  case CallingConv::JWA:
    return CSR_NoRegs_RegMask;
```

### `Target/AArch64`

The basic approach to supporting the **AArch64** (aka **arm64**) target is similar
to the **X86**, but the details are different.  Both because of differences in
the number of target registers and because the **LLVM** code for the ``AArch64``
is writting in a different style (*e.g.*, conditionals instead of `switch`
statements for testing calling conventions).  As before, we need to modify a number
of files in the directory `$LLVM/lib/Target/AArch64/`.

#### `AArch64CallingConvention.td`

At the end of the file, add the following code:

```
//===----------------------------------------------------------------------===//
// JWA Calling Convention
//===----------------------------------------------------------------------===//

// The "Jump With Arguments" calling convention is designed to support the
// "continuation-passing, closure-passing" model used by SML/NJ and the Manticore
// compiler.  The convention is described in the paper "Compiling with Continuations
// and LLVM" (https://arxiv.org/abs/1805.08842v1).
//
let Entry = 1 in
def CC_AArch64_JWA : CallingConv<[
  CCIfType<[iPTR], CCBitConvertToType<i64>>,
  CCIfType<[i1, i8, i16, i32], CCPromoteToType<i64>>,
  //
  // SML/NJ argument order:
  //	alloc, limit, store, exn, var, link, clos, cont,
  //    misc0, ..., misc3, arg, misc4, .., misc17
  CCIfType<[i64],
    CCAssignToRegWithShadow<[
	X24, X25, X26, X27, X28,	// ALLOC, LIMIT, STORE, EXN, VAR
	X3, X2, X1,			// LINK, CLOS, CONT
	X4, X5, X6,			// MISC0-MISC2 (aka, CS0-CS2)
	X0, X7, X8, X9, X10, X11, X12,	// ARG, MISC3-MISC8
	X13, X14, X15, X16, X19, X20,	// MISC9-MISC14
	X21, X22, X23			// MISC15-MISC17
      ],
      [W24, W25, W26, W27, W28,	W3, W2, W1, W4, W5, W6,	W0, W7, W8, W9, W10, W11, W12,
	W13, W14, W15, W16, W19, W20, W21, W22, W23]>>,
  // there are 32 float/vector registers, but we only make 16 of them available
  // for parameter passing
  CCIfType<[f32], CCAssignToRegWithShadow<
    [S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15],
    [Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, Q11, Q12, Q13, Q14, Q15]>>,
  CCIfType<[f64], CCAssignToRegWithShadow<
    [D0, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12, D13, D14, D15],
    [Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, Q11, Q12, Q13, Q14, Q15]>>
]>;

// use the same convention for returns
let Entry = 1 in
def RetCC_AArch64_JWA : CallingConv<[
  CCDelegateTo<CC_AArch64_JWA>
]>;
```

Note that the `let Entry = 1` is necessary to make the function visible in the `llvm`
namespace; otherwise it will be marked as a `static` function.

#### `AArch64CallingConvention.h`

Add the following function prototypes:

``` c++
bool CC_AArch64_JWA(unsigned ValNo, MVT ValVT, MVT LocVT,
                    CCValAssign::LocInfo LocInfo, ISD::ArgFlagsTy ArgFlags,
                    CCState &State);
bool RetCC_AArch64_JWA(unsigned ValNo, MVT ValVT, MVT LocVT,
                         CCValAssign::LocInfo LocInfo, ISD::ArgFlagsTy ArgFlags,
                         CCState &State);
```

#### `AArch64FastISel.cpp`

In the method `AArch64FastISel::CCAssignFnForCall`, we add the following statement
before the final return:

``` c++
  if (CC == CallingConv::JWA)
    return CC_AArch64_JWA;
```

In the function `AArch64FastISel::selectRet`, replace the statement
``` c++
    CCAssignFn *RetCC = CC == CallingConv::WebKit_JS ? RetCC_AArch64_WebKit_JS
                                                     : RetCC_AArch64_AAPCS;
```
with
``` c++
    CCAssignFn *RetCC = CC == CallingConv::WebKit_JS ? RetCC_AArch64_WebKit_JS
                      : CC == CallingConv::JWA       ? RetCC_AArch64_JWA
                      : RetCC_AArch64_AAPCS;
```

#### `AArch64RegisterInfo.cpp`

In the method `AArch64RegisterInfo::getCalleeSavedRegs`, add the following test
following the similar code for the `GHC` convention.
``` c++
  if (MF->getFunction().getCallingConv() == CallingConv::JWA)
    // no callee-saves for JWA
    return CSR_AArch64_NoRegs_SaveList;
```

**NOTE**: it might be better to merge the `GHC` and `JWA` cases into a single
conditional.

In `AArch64RegisterInfo::getCallPreservedMask` method, change the `GHC` test to
the following statement:
``` c++
  if ((CC == CallingConv::GHC) || (CC == CallingConv::JWA))
    // This is academic because all GHC/JWA calls are (supposed to be) tail calls
    return SCS ? CSR_AArch64_NoRegs_SCS_RegMask : CSR_AArch64_NoRegs_RegMask;
```

Add the following assertion to the `AArch64RegisterInfo::getThisReturnPreservedMask`
method:

``` c++
  assert(CC != CallingConv::JWA && "should not be JWA calling convention.");
```

#### `AArch64ISelLowering.cpp`

There are several changes to the `$LLVM/lib/Target/AArch64/AArch64ISelLowering.cpp`
file.  In the method `AArch64TargetLowering::CCAssignFnForCall`, add the case
``` c++
  case CallingConv::JWA:
    return CC_AArch64_JWA;
```

Replace the body of `AArch64TargetLowering::CCAssignFnForReturn` with the following
`switch` statement:
``` c++
  switch (CC) {
  case CallingConv::WebKit_JS:
    return RetCC_AArch64_WebKit_JS;
  case CallingConv::JWA:
    return RetCC_AArch64_JWA;
  default:
    return RetCC_AArch64_AAPCS;
  }
```

Lastly, change the function `canGuaranteeTCO` to the following:
``` c++
static bool canGuaranteeTCO(CallingConv::ID CC) {
  return (CC == CallingConv::Fast) || (CC == CallingConv::JWA);
}
```

In LLVM 11 and earlier, there are a number of functions where the return calling-convention function is
computed by testing the calling convention:
`AArch64TargetLowering::LowerCallResult`,
`AArch64TargetLowering::CanLowerReturn`, and
`AArch64TargetLowering::LowerReturn`.  For these, the expression
``` c++
  CCAssignFn *RetCC = CCAssignFnForReturnCallConv == CallingConv::WebKit_JS
                          ? RetCC_AArch64_WebKit_JS
                          : RetCC_AArch64_AAPCS;
```
should be replaced by
``` c++
  CCAssignFn *RetCC = CCAssignFnForReturn (CallConv);
```
This change is not necessary for LLVM 12+.

#### `AArch64FrameLowering.cpp`

We add the following statement

``` c++
  // All calls are tail calls in JWA calling conv, and functions have no
  // prologue/epilogue.
  if (MF.getFunction().getCallingConv() == CallingConv::JWA)
    return;
```

in three places (following the similar code for the `GHC` calling convention):

  * in method `AArch64FrameLowering::emitPrologue`

  * in method `AArch64FrameLowering::emitEpilogue`

  * in method `AArch64FrameLowering::determineCalleeSaves`

### `SelectionDAGBuilder.cpp`

In Version 11.0.0 of *LLVM*, a change was made that breaks the use of the `naked`
attribute on functions
(see https://github.com/llvm/llvm-project/commit/4dba59689d008df7be37733de4bb537b2911d3ad[commit 4dba59689d008df7be37733de4bb537b2911d3ad]).
To work around this problem, we remove (or comment out) the following code from
the `SelectionDAGISel::LowerArguments` function in
`$LLVM/lib/CodeGen/SelectionDAG/SelectionDAGBuilder.cpp` file.

``` c++
  // In Naked functions we aren't going to save any registers.
  if (F.hasFnAttribute(Attribute::Naked))
    return;
```

## Building LLVM

Once the above edits have been made to the **LLVM** sources, you can use the
the `build-llvm.sh` script to build LLVM.  This script packages up the various
steps needed to build and install LLVM, but we outline the build process here.

There are three directories involved:

  * The LLVM source directory (`$LLVM_SRC`); currently this directory is `llvm-10.0.1.src`

  * A fresh build directory, (`$LLVM_BUILD`)

  * A fresh installation directory (`$LLVM_INSTALL`)

We also need to decide on the build type (`$BUILD_TYPE`), which can be either
`Release` or `Debug`. The latter is significantly larger, slower, and takes much
longer to build, but it is useful for development purposes.

We start by creating the build directory:

``` sh
mkdir $LLVM_BUILD
cd $LLVM_BUILD
```

Then we need to configure the build.  We use a bunch of options to try to reduce the
time it takes to build LLVM.

``` sh
CMAKE_DEFS="\
  -DCMAKE_BUILD_TYPE=$BUILD_TYPE \
  -DCMAKE_INSTALL_PREFIX=../$LLVM_INSTALL \
  -DLLVM_TARGETS_TO_BUILD=$TARGETS \
  -DLLVM_ENABLE_LIBXML2=OFF \
  -DLLVM_ENABLE_OCAMLDOC=OFF \
  -DLLVM_INCLUDE_BENCHMARKS=OFF \
  -DLLVM_INCLUDE_DOCS=OFF \
  -DLLVM_INCLUDE_GO_TESTS=OFF \
  -DLLVM_INCLUDE_TESTS=OFF \
  -DLLVM_TOOL_DSYMUTIL_BUILD=OFF \
  -DLLVM_TOOL_GOLD_BUILD=OFF \
  -DLLVM_TOOL_LLVM_AR_BUILD=OFF \
  -DLLVM_TOOL_LLVM_AS_BUILD=OFF \
  -DLLVM_TOOL_LLVM_AS_FUZZER_BUILD=OFF \
  -DLLVM_TOOL_LLVM_BCANALYZER_BUILD=OFF \
  -DLLVM_TOOL_LLVM_CAT_BUILD=OFF \
  -DLLVM_TOOL_LLVM_CFI_VERIFY_BUILD=OFF \
  -DLLVM_TOOL_LLVM_COV_BUILD=OFF \
  -DLLVM_TOOL_LLVM_CVTRES_BUILD=OFF \
  -DLLVM_TOOL_LLVM_CXXDUMP_BUILD=OFF \
  -DLLVM_TOOL_LLVM_CXXFILT_BUILD=OFF \
  -DLLVM_TOOL_LLVM_CXXMAP_BUILD=OFF \
  -DLLVM_TOOL_LLVM_C_TEST_BUILD=OFF \
  -DLLVM_TOOL_LLVM_DIFF_BUILD=OFF \
  -DLLVM_TOOL_LLVM_DIS_BUILD=OFF \
  -DLLVM_TOOL_LLVM_DWP_BUILD=OFF \
  -DLLVM_TOOL_LLVM_ELFABI_BUILD=OFF \
  -DLLVM_TOOL_LLVM_EXEGESIS_BUILD=OFF \
  -DLLVM_TOOL_LLVM_EXTRACT_BUILD=OFF \
  -DLLVM_TOOL_LLVM_GO_BUILD=OFF \
  -DLLVM_TOOL_LLVM_IFS_BUILD=OFF \
  -DLLVM_TOOL_LLVM_ISEL_FUZZER_BUILD=OFF \
  -DLLVM_TOOL_LLVM_ITANIUM_DEMANGLE_FUZZER_BUILD=OFF \
  -DLLVM_TOOL_LLVM_JITLINK_BUILD=OFF \
  -DLLVM_TOOL_LLVM_JITLISTENER_BUILD=OFF \
  -DLLVM_TOOL_LLVM_LINK_BUILD=OFF \
  -DLLVM_TOOL_LLVM_LIPO_BUILD=OFF \
  -DLLVM_TOOL_LLVM_LTO2_BUILD=OFF \
  -DLLVM_TOOL_LLVM_LTO_BUILD=OFF \
  -DLLVM_TOOL_LLVM_MCA_BUILD=OFF \
  -DLLVM_TOOL_LLVM_MC_ASSEMBLE_FUZZER_BUILD=OFF \
  -DLLVM_TOOL_LLVM_MC_BUILD=OFF \
  -DLLVM_TOOL_LLVM_MC_DISASSEMBLE_FUZZER_BUILD=OFF \
  -DLLVM_TOOL_LLVM_MICROSOFT_DEMANGLE_FUZZER_BUILD=OFF \
  -DLLVM_TOOL_LLVM_MODEXTRACT_BUILD=OFF \
  -DLLVM_TOOL_LLVM_MT_BUILD=OFF \
  -DLLVM_TOOL_LLVM_NM_BUILD=OFF \
  -DLLVM_TOOL_LLVM_OBJCOPY_BUILD=OFF \
  -DLLVM_TOOL_LLVM_OBJDUMP_BUILD=OFF \
  -DLLVM_TOOL_LLVM_OPT_FUZZER_BUILD=OFF \
  -DLLVM_TOOL_LLVM_OPT_REPORT_BUILD=OFF \
  -DLLVM_TOOL_LLVM_PDBUTIL_BUILD=OFF \
  -DLLVM_TOOL_LLVM_PROFDATA_BUILD=OFF \
  -DLLVM_TOOL_LLVM_RC_BUILD=OFF \
  -DLLVM_TOOL_LLVM_READOBJ_BUILD=OFF \
  -DLLVM_TOOL_LLVM_REDUCE_BUILD=OFF \
  -DLLVM_TOOL_LLVM_RTDYLD_BUILD=OFF \
  -DLLVM_TOOL_LLVM_SHLIB_BUILD=OFF \
  -DLLVM_TOOL_LLVM_SIZE_BUILD=OFF \
  -DLLVM_TOOL_LLVM_SPECIAL_CASE_LIST_FUZZER_BUILD=OFF \
  -DLLVM_TOOL_LLVM_SPLIT_BUILD=OFF \
  -DLLVM_TOOL_LLVM_STRESS_BUILD=OFF \
  -DLLVM_TOOL_LLVM_STRINGS_BUILD=OFF \
  -DLLVM_TOOL_LLVM_SYMBOLIZER_BUILD=OFF \
  -DLLVM_TOOL_LLVM_UNDNAME_BUILD=OFF \
  -DLLVM_TOOL_LLVM_XRAY_BUILD=OFF \
  -DLLVM_TOOL_LLVM_YAML_NUMERIC_PARSER_FUZZER_BUILD=OFF \
  -DLLVM_TOOL_LTO_BUILD=OFF \
  -DLLVM_TOOL_OBJ2YAML_BUILD=OFF \
  -DLLVM_TOOL_OPT_BUILD=OFF \
  -DLLVM_TOOL_OPT_VIEWER_BUILD=OFF \
  -DLLVM_TOOL_REMARKS_SHLIB_BUILD=OFF \
  -DLLVM_TOOL_SANCOV_BUILD=OFF \
  -DLLVM_TOOL_SANSTATS_BUILD=OFF \
  -DLLVM_TOOL_VERIFY_USELISTORDER_BUILD=OFF \
  -DLLVM_TOOL_VFABI_DEMANGLE_FUZZER_BUILD=OFF \
  -DLLVM_TOOL_XCODE_TOOLCHAIN_BUILD=OFF \
  -DLLVM_TOOL_YAML2OBJ_BUILD=OFF \
"
```

On **Linux** systems, you should add the option `-DLLVM_USE_LINKER=gold` to
the `CMAKE_DEFS` definition.

``` sh
cmake -G "Unix Makefiles" "$CMAKE_OPTS" ../llvm-src
```

``` sh
make -j $NPROC install
```

In the future, we may want to add `PowerPC`, `RISCV`, or `Sparc` as targets.

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
the spilling of up to 1,000 variables.  It also contains some additional
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

### `CallingConv.h`

The file `$LLVM/include/llvm/IR/CallingConv.h` assigns integer codes for
the various calling conventions.  In **LLVM** 10.0.x, the last number assigned
is `19`, so we use `20` for **JWA**.  Add the following code to the file just
before the first target-specific code (which will be `64`).

````C
    /// JWA - "Jump With Arguments" is a calling convention that requires the
    /// use of registers for parameter passing. It is designed for language
    /// implementations that do not use a stack, however, it will not warn
    /// if there are not enough registers for a given function. The lack of
    /// warning is needed in order to properly utilize musttail calls as
    /// jumps because they are picky about parameters.
    JWA = 20,
````

### `Target/X86`

To support the **JWA** convention on the `X86`, we need to modify a number
of files in the directory `$LLVM/lib/Target/X86/`.

#### `X86CallingConv.td`

The file `$LLVM/lib/Target/X86/X86CallingConv.td` describes the calling
conventions for the *x86* and *x86-64* (aka *amd64*) architectures
using **LLVM**'s [**TabgeGen**](https://llvm.org/docs/TableGen/) language.
We need to add several chunks of code to the file.  I added the first
just before the definition for `RetCC_X86_32`.

````
// The JWA calling convention for x86_64. Note that this is
// also used as the return convention in order to implement call/cc.
// True returns are not the norm. We _never_ use the stack.
def CC_X86_64_JWA : CallingConv<[

  // Promote i8/i16/i32 arguments to i64.
  CCIfType<[i8, i16, i32], CCPromoteToType<i64>>,

  // The only registers we skip are RBP and RSP.

  // NOTE(kavon): alloc, vproc, clos, retk, exh, stdArg, otherArgs

  CCIfType<[i64],
  CCAssignToReg<[RSI, R11, RDI, R8, R9, RAX,
                 RDX, RCX, R10, RBX, R12, R13, R14, R15]>>,


  // TODO(kavon): check if something breaks if the target
  // does not support SSE registers? should add a check for that.

  // Use as many vector registers as possible!
  CCIfType<[i64, f32, f64, v16i8, v8i16, v4i32, v2i64, v4f32, v2f64],
           CCAssignToReg<[XMM0, XMM1, XMM2, XMM3,
                          XMM4, XMM5, XMM6, XMM7,
                          XMM8, XMM9, XMM10, XMM11,
                          XMM12, XMM13, XMM14, XMM15]>>,

  // 256-bit vectors registers
  CCIfType<[i64, f32, f64, v32i8, v16i16, v8i32, v4i64, v8f32, v4f64],
           CCAssignToReg<[YMM0, YMM1, YMM2, YMM3,
                          YMM4, YMM5, YMM6, YMM7,
                          YMM8, YMM9, YMM10, YMM11,
                          YMM12, YMM13, YMM14, YMM15]>>,

  // 512-bit vector registers
  CCIfType<[i64, f32, f64, v64i8, v32i16, v16i32, v8i64, v16f32, v8f64],
           CCAssignToReg<[ZMM0, ZMM1, ZMM2, ZMM3,
                          ZMM4, ZMM5, ZMM6, ZMM7,
                          ZMM8, ZMM9, ZMM10, ZMM11,
                          ZMM12, ZMM13, ZMM14, ZMM15,
                          ZMM16, ZMM17, ZMM18, ZMM19,
                          ZMM20, ZMM21, ZMM22, ZMM23,
                          ZMM24, ZMM25, ZMM26, ZMM27,
                          ZMM28, ZMM29, ZMM30, ZMM31]>>

]>;
````

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

````
  // Handle JWA calls.
  CCIfCC<"CallingConv::JWA", CCDelegateTo<CC_X86_64_JWA>>,
````

The following line needs to be added to the definition of the
"root argument convention for the X86-64 backend" (`CC_X86_64`):

````
CCIfCC<"CallingConv::JWA", CCDelegateTo<CC_X86_64_JWA>>,
````

#### `X86FastISel.cpp`

In the file `$LLVM/lib/Target/X86/X86FastISel.cpp`, the function
`computeBytesPoppedByCalleeForSRet` needs to be modified to
recognize the **JWA** convention.

To the code
````
  if (CC == CallingConv::Fast || CC == CallingConv::GHC ||
      CC == CallingConv::HiPE || CC == CallingConv::Tail)
    return 0;
````
add a test for **JWA** (`CC == CallingConv::JWA`).

#### `X86ISelLowering.cpp`

In the file `$LLVM/lib/Target/X86/X86ISelLowering.cpp`, we need to add
a check for **JWA** to the function `canGuaranteeTCO`.

````
  return (CC == CallingConv::Fast || CC == CallingConv::GHC ||
          CC == CallingConv::X86_RegCall || CC == CallingConv::HiPE ||
          CC == CallingConv::HHVM || CC == CallingConv::Tail ||
	  CC == CallingConv::JWA);
````

#### `X86RegisterInfo.cpp`

In the file `$LLVM/lib/Target/X86/X86RegisterInfo.cpp`, we need to add
cases for **JWA** to the method `getCalleeSavedRegs`:
````
  case CallingConv::GHC:
  case CallingConv::HiPE:
  case CallingConv::JWA:
    return CSR_NoRegs_SaveList;
````
and to the method `getCallPreservedMask`
````
  case CallingConv::GHC:
  case CallingConv::HiPE:
  case CallingConv::JWA:
    return CSR_NoRegs_RegMask;
````

## Building LLVM

Once the above edits have been made to the **LLVM** sources, you can use the
following steps to build the **LLVM** library that will be linked with the
**SML/NJ** runtime system.

````
# make a directory to build LLVM in
#
mkdir llvm-build
cd llvm-build

# configure the build
#
CMAKE_OPTS="\
  -DLLVM_ENABLE_ASSERTIONS=ON \
  -DLLVM_OPTIMIZED_TABLEGEN=ON \
  -DLLVM_CCACHE_BUILD=OFF \
  -DCMAKE_INSTALL_PREFIX=../llvm \
  -DCMAKE_BUILD_TYPE=Release \
  -DLLVM_TARGETS_TO_BUILD=X86 \
  -DLLVM_INCLUDE_TOOLS=OFF \
  -DLLVM_TOOL_LLVM_CONFIG_BUILD=ON \
  -DLLVM_BUILD_LLVM_DYLIB=ON \
"

cmake -G "Unix Makefiles" "$CMAKE_OPTS" ../llvm-src

# build LLVM using $NPROC processors
#
make -j $NPROC install
````

On **Linux** systems, you should add the option `-DLLVM_USE_LINKER=gold` to
the `CMAKE_OPTS` definition.

In the future, we may want to add `AArch64` (64-bit ARM), `PowerPC`, `RISCV`,
and `Sparc` as targets.

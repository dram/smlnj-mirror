This branch is for developing an implementation of the Int64 and Word64
modules that allows the types to pass through the compiler to CPS.

The implementation strategy uses a combination of operations in the `Core`
structure (`system/smlnj/init/core.sml`) and CPS-to-CPS translation in
the `Num64Cnv` structure (`compiler/CPS/cpsopt/num64cnv.sml`).  For
an operation `foo` that is too complicated to fully expand in `Num64Cnv`, we
add an extra argument that is `Core.foo` to the primop when translating
to the PLambda IR (`compiler/FLINT/trans/transprim.sml`).

## Primops

A list of the 64-bit primops that are exported by the compiler can be
extracted from `dev-notes/primop-list.md`.  Here we list the internal
64-bit primops (`Primop.primop`) that the compiler must support.
We group them by where in the compilation pipeline they are handled.

### Expand in `TransPrim`

As with other types, inline operations are expanded in the `TransPrim`
structure (`compiler/FLINT/trans/transprim.sml`).

  * `P.INLDIV (P.INT 64)`
  * `P.INLMOD (P.INT 64)`
  * `P.INLQUOT (P.INT 64)`
  * `P.INLREM (P.INT 64)`
  * `P.INLMIN (P.INT 64)`
  * `P.INLMAX (P.INT 64)`
  * `P.INLABS (P.INT 64)`
  * `P.INLQUOT (P.UINT 64)`
  * `P.INLREM (P.UINT 64)`
  * `P.INLRSHIFT(P.UINT 64)`
  * `P.INLRSHIFTL(P.UINT 64)`
  * `P.INLLSHIFT(P.UINT 64)`
  * `P.INLMIN (P.UINT 64)`
  * `P.INLMAX (P.UINT 64)`

### Expand in `Num64Cnv`

The following operations will be implemented by a CPS-to-CPS translation
in the `Num64Cnv` structure (`compiler/CPS/cpsopt/num64cnv.sml`):

  * `P.IARITH{oper=P.IADD, sz=64}`
  * `P.IARITH{oper=P.ISUB, sz=64}`
  * `P.IARITH{oper=P.INEG, sz=64}`
  * `P.CMP{oper=P.GT, kind=P.INT 64}`
  * `P.CMP{oper=P.GTE, kind=P.INT 64}`
  * `P.CMP{oper=P.LT, kind=P.INT 64}`
  * `P.CMP{oper=P.LTE, kind=P.INT 64}`
  * `P.CMP{oper=P.EQL, kind=P.INT 64}`
  * `P.CMP{oper=P.NEQ, kind=P.INT 64}`
  * `P.PURE_ARITH{oper=P.ADD, kind=P.UINT 64}`
  * `P.PURE_ARITH{oper=P.SUB, kind=P.UINT 64}`
  * `P.PURE_ARITH{oper=P.NEG, kind=P.UINT 64}`
  * `P.PURE_ARITH{oper=P.ORB, kind=P.UINT 64}`
  * `P.PURE_ARITH{oper=P.XORB, kind=P.UINT 64}`
  * `P.PURE_ARITH{oper=P.ANDB, kind=P.UINT 64}`
  * `P.PURE_ARITH{oper=P.NOTB, kind=P.UINT 64}`
  * `P.PURE_ARITH{oper=P.RSHIFT, kind=P.UINT 64}`
  * `P.PURE_ARITH{oper=P.RSHIFTL, kind=P.UINT 64}`
  * `P.PURE_ARITH{oper=P.LSHIFT, kind=P.UINT 64}`
  * `P.CMP{oper=P.GT, kind=P.UINT 64}`
  * `P.CMP{oper=P.GTE, kind=P.UINT 64}`
  * `P.CMP{oper=P.LT, kind=P.UINT 64}`
  * `P.CMP{oper=P.LTE, kind=P.UINT 64}`
  * `P.CMP{oper=P.EQL, kind=P.UINT 64}`
  * `P.CMP{oper=P.NEQ, kind=P.UINT 64}`
  * `P.TRUNC(64, 31)`
  * `P.EXTEND(31, 64)`
  * `P.COPY(31, 64)`
  * `P.TRUNC(64, 32)`
  * `P.COPY(32, 64)`
  * `P.EXTEND(32, 64)`

### Operations that are implemented in `Core`

  * `P.IARITH{oper=P.IMUL, sz=64}`
  * `P.IARITH{oper=P.IDIV, sz=64}`
  * `P.IARITH{oper=P.IMOD, sz=64}`
  * `P.IARITH{oper=P.IQUOT, sz=64}`
  * `P.IARITH{oper=P.IREM, sz=64}`
  * `P.PURE_ARITH{oper=P.MUL, kind=P.UINT 64}`
  * `P.PURE_ARITH{oper=P.QUOT, kind=P.UINT 64}`
  * `P.PURE_ARITH{oper=P.REM, kind=P.UINT 64}`
  * `P.TEST(64, 31)`
  * `P.TESTU(64, 31)`
  * `P.EXTEND_INF 64`
  * `P.TEST_INF 64`
  * `P.COPY_INF 64`
  * `P.TRUNC_INF 64`

compiler/FLINT/kernel/README.txt

Clean up and simplification of the FLINT types implementation

Goals:
1. replace the proliferation of files by 2 or 3.
   Definition of basic types (lty.sml) + a single utilities module (ltyutil.sml).

2. Experiment with removing complexities like hash consing and (semi-)Nadathur closures.

Files (110.91)

lty.sig/sml
ltybasic.sig/sml
ltydef.sig/sml
ltyextern.sig/sml
ltykernel.sig/sml

ltydict.sml
ltykindchk.sml
pplty.sml
  prettyprinting

primtyc.sig/sml

Other relevant files:
  reps/typeoper.sml -- possibly only relevant to runtime type passing, may go away
  
ChangeLog

1. primtyc.sig/sml
   Changed name: "unboxed" ==> "boxedNumeric"
   Changed name: "ubxupd" ==> "unboxedUpdatePrimtyc"  (only used once in ltyextern.sml)   

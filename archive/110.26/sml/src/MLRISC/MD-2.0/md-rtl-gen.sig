(*
 * This takes a bunch of RTL and build a database that can be reused.
 *)
signature MD_RTL_GEN =
sig
   structure Term : MD_TERM
   include INSTRUCTIONS_RTL
      where type cell    = Term.cell
      and   type exp     = Term.exp
      and   type cond    = Term.cond
      and   type operand = Term.operand
      and   type stm     = Term.stm 
      and   type label   = Term.label
      and   type ty      = string * int option
   type rtl = Term.rtl
   val rtlReg    : string -> cell
   val rtlCCReg  : string -> cell
   val rtlLabel  : string -> label
   val rtlOpnd   : string -> operand
end

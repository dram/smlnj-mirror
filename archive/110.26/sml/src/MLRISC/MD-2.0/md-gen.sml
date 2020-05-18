(*
 * Generate machine code modules from machine description
 *)
functor MDGen
   (structure Comp       : MD_COMPILE
    structure Build      : MD_BUILD
    structure Cells      : MD_GEN_MODULE
    structure Shuffle    : MD_GEN_MODULE
    structure Instr      : MD_GEN_MODULE
    structure Asm        : MD_GEN_MODULE
    structure MC         : MD_GEN_MODULE
    structure RTL        : MD_RTL
    structure Props      : MD_GEN_MODULE
    structure Jumps      : MD_GEN_MODULE
    structure DelaySlots : MD_GEN_MODULE
    structure Dasm       : MD_GEN_MODULE
    structure RTLProps   : MD_GEN2_MODULE
    structure SSAProps   : MD_GEN2_MODULE
    structure Rewrite    : MD_GEN_MODULE
    structure SchedProps : MD_GEN2_MODULE
      sharing Comp = 
              Cells.Comp = 
              Shuffle.Comp =
              Instr.Comp = 
              Asm.Comp = 
              MC.Comp = 
              RTL.Comp = 
              Dasm.Comp = 
              Props.Comp =
              Jumps.Comp = 
              DelaySlots.Comp = 
              RTLProps.Comp =
              SSAProps.Comp =
              Rewrite.Comp =
              SchedProps.Comp
      sharing Build.Ast = Comp.Ast
      sharing RTL = RTLProps.RTL = SSAProps.RTL = SchedProps.RTL
   ) : MD_GEN =
struct

   (* Generate code! *)
   fun codegen md =
      (Cells.gen md;
       Instr.gen md;
       Shuffle.gen md;
       Asm.gen md;
       MC.gen md;
       Dasm.gen md;
       Props.gen md; 
       Jumps.gen md; 
       DelaySlots.gen md;
       Rewrite.gen md;
       let val rtl = RTL.compile md
       in  RTL.gen rtl;
           RTLProps.gen rtl;
           SSAProps.gen rtl;
           SchedProps.gen rtl
       end
      )

   fun gen file = 
       (print("[Processing "^file^"]\n");
        Comp.Error.init();
        codegen(Comp.compile(file,Build.load file)) (* build ast *)
       )

   fun exit() = if !Comp.Error.errorCount > 0 then 
                     OS.Process.failure 
                else OS.Process.success 

end

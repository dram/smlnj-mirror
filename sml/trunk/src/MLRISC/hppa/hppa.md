(*
 *  This is the new machine description language.
 *
 *)

architecture Hppa =
struct

   name "Hppa"

   version "1"

   superscalar

   big endian

   lowercase assembly

   instruction delayslot 4
   (*
    * On the HP, handling of delay slots is quite complicated:
    *
    *  For conditional branches:
    *  -------------------------
    *                                 Branch direction         
    *                      Forward                    Backward
    *  Nullify bit on    Nullify if branch taken   Nullify if branch not-taken
    *  Nullify bit off   Delay slot active         Delay slot active
    *
    *  For unconditional branches:
    *  ---------------------------
    *       
    *  Nullify bit on    Delay slot nullified
    *  Nullify bit off   Delay slot active       
    *)


   (* debug MC *)

   storage
     GP "r" = 32 cells of 32 bits in cellset called "register"
              assembly as (fn (r,_) => "%r"^Int.toString r)
              zero 0
   | FP "f" = 32 cells of 64 bits in cellset called "floating point register"
              assembly as (fn (f,_) => "%f"^Int.toString f)
              zero 0
   | CR "cr" = 32 cells of 32 bits called "control register"
               assembly as (fn (cr,_) => "%cr"^Int.toString cr)
   | CC "cc" = cells of 32 bits in cellset GP called "condition code register"
               assembly as "cc" 
   | MEM "m" = cells of 8 bits called "memory"
               assembly as (fn (r,_) => "m"^Int.toString r)
   | CTRL "ctrl" = cells of 8 bits called "control"
               assembly as (fn (r,_) => "ctrl"^Int.toString r)

   locations
       returnPtr = $GP[2]  
   and stackptrR = $GP[30]
   and asmTmpR   = $GP[29]
   and fasmTmp   = $FP[31]
   and sar       = $CR[11]

   (* 
    * RTL specification.
    *)
   structure RTL = 
   struct
      include "MD-2.0/basis.md"
      open Basis
      infix 1 ||
      infix 2 :=
      infix 3 << >> ~>>

      fun %  x = (operand x : #32 bits)
      fun %% l = (label l : #32 bits)

      rtl NOP{} = ()
      rtl COPY{dst,src} = $r[forall dst] := $r[forall src]
      rtl FCOPY{dst,src} = $f[forall dst] := $f[forall src]

      (* Integer loads *)
      (* On the HP, addressing modes can be scaled and/or autoincrement *)
      fun disp(r,i) = $r[r] + %i
      fun fdisp(r,d) = $r[r] + immed d
      fun indexed(r1,r2) = $r[r1] + $r[r2]
      fun scaled(r1,r2,scale) = $r[r1] << scale + $r[r2]
      fun autoinc(r,i) = $r[r] := $r[r] + i
      fun overflowtrap{} = ()

      fun byte x = (x : #8 bits)
      fun half x = (x : #16 bits)
      fun word x = (x : #32 bits)
      fun quad x = (x : #64 bits)

      rtl LDO{b,i,t} = $r[t] := $r[b] + %i
      rtl LDO2{i,t}  = $r[t] := %i
      rtl LDIL{i,t}  = $r[t] := %i << 11
      rtl MTCTL{r,t} = $cr[t] := $r[r]

      rtl LDW{r,i,t}       = $r[t] := $m[disp(r,i)]
      rtl LDH{r,i,t}       = $r[t] := zx(half $m[disp(r,i)])
      rtl LDB{r,i,t}       = $r[t] := zx(byte $m[disp(r,i)])
      rtl LDWX{r1,r2,t}    = $r[t] := $m[indexed(r1,r2)]
      rtl LDWX_S{r1,r2,t}  = $r[t] := $m[scaled(r1,r2,2)]
      rtl LDWX_M{r1,r2,t}  = $r[t] := $m[indexed(r1,r2)] || autoinc(r1,1)
      rtl LDWX_SM{r1,r2,t} = $r[t] := $m[scaled(r1,r2,2)] || autoinc(r1,4)
      rtl LDHX{r1,r2,t}    = $r[t] := zx(half $m[indexed(r1,r2)])
      rtl LDHX_S{r1,r2,t}  = $r[t] := zx(half $m[scaled(r1,r2,1)])
      rtl LDHX_M{r1,r2,t}  = $r[t] := zx(half $m[indexed(r1,r2)]) || autoinc(r1,1)
      rtl LDHX_SM{r1,r2,t} = $r[t] := zx(half $m[scaled(r1,r2,1)]) || autoinc(r1,2)
      rtl LDBX{r1,r2,t}    = $r[t] := zx(byte $m[indexed(r1,r2)])
      rtl LDBX_M{r1,r2,t}  = $r[t] := zx(byte $m[indexed(r1,r2)]) || autoinc(r1,1)

      (* Integer stores *) 
      rtl STW{b,d,r} = $m[disp(b,d)] := $r[r]
      rtl STH{b,d,r} = $m[disp(b,d)] := half(zx $r[r])
      rtl STB{b,d,r} = $m[disp(b,d)] := byte(zx $r[r])

      (* Integer opcodes *)
      rtl ADD{r1,r2,t}     = $r[t] := $r[r1] + $r[r2]
      rtl ADDL{r1,r2,t}    = $r[t] := $r[r1] + $r[r2]
      rtl ADDO{r1,r2,t}    = $r[t] := $r[r1] + $r[r2] || overflowtrap{}
      rtl SUB{r1,r2,t}     = $r[t] := $r[r1] - $r[r2]
      rtl SUBO{r1,r2,t}    = $r[t] := $r[r1] - $r[r2] || overflowtrap{}
      rtl SH1ADD{r1,r2,t}  = $r[t] := $r[r1] << 1 + $r[r2]
      rtl SH2ADD{r1,r2,t}  = $r[t] := $r[r1] << 2 + $r[r2]
      rtl SH3ADD{r1,r2,t}  = $r[t] := $r[r1] << 3 + $r[r2]
      rtl SH1ADDL{r1,r2,t} = $r[t] := $r[r1] << 1 + $r[r2]
      rtl SH2ADDL{r1,r2,t} = $r[t] := $r[r1] << 2 + $r[r2]
      rtl SH3ADDL{r1,r2,t} = $r[t] := $r[r1] << 3 + $r[r2]
      rtl SH1ADDO{r1,r2,t} = SH1ADD{r1,r2,t} || overflowtrap{}
      rtl SH2ADDO{r1,r2,t} = SH2ADD{r1,r2,t} || overflowtrap{}
      rtl SH3ADDO{r1,r2,t} = SH3ADD{r1,r2,t} || overflowtrap{}
      rtl OR{r1,r2,t}      = $r[t] := orb($r[r1], $r[r2])
      rtl AND{r1,r2,t}     = $r[t] := andb($r[r1], $r[r2])
      rtl XOR{r1,r2,t}     = $r[t] := xorb($r[r1], $r[r2])
      rtl ANDCM{r1,r2,t}   = $r[t] := andb($r[r1], notb($r[r2]))

      rtl ADDI{r,i,t}  = $r[t] := $r[r] + %i
      rtl ADDIO{r,i,t} = $r[t] := $r[r] + %i || overflowtrap{}
      rtl ADDIL{r,i,t} = $r[t] := $r[r] + %i
      rtl SUBI{r,i,t}  = $r[t] := $r[r] + %i
      rtl SUBIO{r,i,t} = $r[t] := $r[r] + %i || overflowtrap{}

      (* Shifts *)
      rtl extru extrs zdep : #n bits * #n bits * #n bits -> #n bits
      val sar = $cr[11]
      rtl VEXTRU{r, len, t}   = $r[t] := extru($r[r], sar, immed len)
      rtl VEXTRS{r, len, t}   = $r[t] := extrs($r[r], sar, immed len)
      rtl ZVDEP{r, len, t}    = $r[t] := zdep($r[r], sar, immed len)
      rtl EXTRU{r, p, len, t} = $r[t] := extru($r[r], immed p, immed len)
      rtl EXTRS{r, p, len, t} = $r[t] := extrs($r[r], immed p, immed len)
      rtl ZDEP{r, p, len, t}  = $r[t] := zdep($r[r], immed p, immed len)

      val comparisons = 
           [(==),  (<),   (<=),
           (ltu), (leu), (<>),
           (>=),  (>),   (gtu), (geu)] 

      (* COMCLR/LDO composite instruction:
       *  COMCLR,cc r1, r2, t1
       *  LDO       i(b), t2
       *)
      fun COMCLR_LDO cc {r1,r2,t1,i,b,t2} =
          if cc($r[r1],$r[r2]) then $r[t1] := 0 else $r[t2] := $r[b] + immed i
      rtl COMCLR_LDO_ ^^ [EQ,  LT,  LE, LTU, LEU, NE, GE, GT, GTU, GEU] =
          map COMCLR_LDO comparisons

      (* COMCLR/LDO composite instruction:
       *  COMCLR,cc r1, r2, t
       *  LDO       i(b), t
       *  This version assumes that t1 = t2.
       *)
      fun COMCLR_LDO2 cc {r1,r2,t1,i,b} =
          if cc($r[r1],$r[r2]) then $r[t1] := 0 else $r[t1] := $r[b] + immed i
      rtl COMCLR_LDO2_ ^^ [EQ,  LT,  LE, LTU, LEU, NE, GE, GT, GTU, GEU] =
          map COMCLR_LDO2 comparisons

      (* COMCLR/LDO composite instruction:
       *  COMCLR,cc r1, r2, %r0
       *  LDO       i(b), t
       *  This version assumes that t1 = %r0.
       *)
      fun COMCLR_LDO3 cc {r1,r2,t2,i,b} =
          if cc($r[r1],$r[r2]) then () else $r[t2] := $r[b] + immed i
      rtl COMCLR_LDO3_ ^^ [EQ,  LT,  LE, LTU, LEU, NE, GE, GT, GTU, GEU] =
          map COMCLR_LDO3 comparisons

      (* Integer branching instructions *)
      fun COMBT cmp {r1,r2,t} = if cmp($r[r1],$r[r2]) then Jmp(%%t) else ()
      fun COMBF cmp {r1,r2,t} = if cmp($r[r1],$r[r2]) then () else Jmp(%%t)
      fun COMIBT cmp {i,r2,t} = if cmp(immed i,$r[r2]) then Jmp(%%t) else ()
      fun COMIBF cmp {i,r2,t} = if cmp(immed i,$r[r2]) then () else Jmp(%%t)
      rtl COMBT_ ^^ [EQ,  LT,  LE, LTU, LEU, NE, GE, GT, GTU, GEU] =
          map COMBT comparisons
      rtl COMBF_ ^^ [EQ,  LT,  LE, LTU, LEU, NE, GE, GT, GTU, GEU] =
          map COMBF comparisons
      rtl COMIBT_ ^^ [EQ,  LT,  LE, LTU, LEU, NE, GE, GT, GTU, GEU] =
          map COMIBT comparisons
      rtl COMIBF_ ^^ [EQ,  LT,  LE, LTU, LEU, NE, GE, GT, GTU, GEU] =
          map COMIBF comparisons

      rtl B{lab} = Jmp(%% lab)
      rtl BV{x,b}  = Jmp($r[x] << 2 + $r[b])
           (* BB,< branch on bit set *)
      rtl BB_BSET{p,r,t} = 
            if andb($r[r],1 << (31 - immed p)) <> 0 then Jmp(%%t) else ()
           (* BB,>= branch on bit clear *)
      rtl BB_BCLR{p,r,t} = 
            if andb($r[r],1 << (31 - immed p)) == 0 then Jmp(%%t) else ()

      rtl BLE{d,b,defs,uses} = 
          Call($r[b] + %d)    || (* call *)
          $r[31] := ?         || (* return address *)
          $cellset[defs] := $cellset[uses]
             
      (* Floating point loads *)
      rtl FLDDS{b,d,t}    = $f[t] := $m[fdisp(b,d)]
      rtl FLDWS{b,d,t}    = $f[t] := $m[fdisp(b,d)]
      rtl FLDDX{b,x,t}    = $f[t] := $m[indexed(b,x)]
      rtl FLDDX_S{b,x,t}  = $f[t] := $m[scaled(b,x,3)]
      rtl FLDDX_M{b,x,t}  = $f[t] := $m[indexed(b,x)] || autoinc(b,8)
      rtl FLDDX_SM{b,x,t} = $f[t] := $m[scaled(b,x,3)] || autoinc(b,8)
      rtl FLDWX{b,x,t}    = $f[t] := $m[indexed(b,x)]
      rtl FLDWX_S{b,x,t}  = $f[t] := $m[scaled(b,x,2)]
      rtl FLDWX_M{b,x,t}  = $f[t] := $m[indexed(b,x)] || autoinc(b,4)
      rtl FLDWX_SM{b,x,t} = $f[t] := $m[scaled(b,x,2)] || autoinc(b,4)

      (* Floating point stores *)
      rtl FSTDS{b,d,r}    = $m[fdisp(b,d)] := $f[r] 
      rtl FSTWS{b,d,r}    = $m[fdisp(b,d)] := $f[r]
      rtl FSTDX{b,x,r}    = $m[indexed(b,x)] := $f[r] 
      rtl FSTDX_S{b,x,r}  = $m[scaled(b,x,3)] := $f[r]  
      rtl FSTDX_M{b,x,r}  = $m[indexed(b,x)] := $f[r] || autoinc(b,8)
      rtl FSTDX_SM{b,x,r} = $m[scaled(b,x,3)] := $f[r] || autoinc(b,8)
      rtl FSTWX{b,x,r}    = $m[indexed(b,x)] := $f[r] 
      rtl FSTWX_S{b,x,r}  = $m[scaled(b,x,2)] := $f[r]  
      rtl FSTWX_M{b,x,r}  = $m[indexed(b,x)] := $f[r] || autoinc(b,4)
      rtl FSTWX_SM{b,x,r} = $m[scaled(b,x,2)] := $f[r] || autoinc(b,4)

      (* Floating point binary operators *)
      rtl FADD_S{r1,r2,t} = $f[t] := fadd($f[r1], $f[r2])
      rtl FADD_D{r1,r2,t} = $f[t] := fadd($f[r1], $f[r2])
      rtl FADD_Q{r1,r2,t} = $f[t] := fadd($f[r1], $f[r2])
      rtl FSUB_S{r1,r2,t} = $f[t] := fsub($f[r1], $f[r2])
      rtl FSUB_D{r1,r2,t} = $f[t] := fsub($f[r1], $f[r2])
      rtl FSUB_Q{r1,r2,t} = $f[t] := fsub($f[r1], $f[r2])
      rtl FMPY_S{r1,r2,t} = $f[t] := fsub($f[r1], $f[r2])
      rtl FMPY_D{r1,r2,t} = $f[t] := fmul($f[r1], $f[r2])
      rtl FMPY_Q{r1,r2,t} = $f[t] := fmul($f[r1], $f[r2])
      rtl FDIV_S{r1,r2,t} = $f[t] := fdiv($f[r1], $f[r2])
      rtl FDIV_D{r1,r2,t} = $f[t] := fdiv($f[r1], $f[r2])
      rtl FDIV_Q{r1,r2,t} = $f[t] := fdiv($f[r1], $f[r2])
      rtl XMPYU{r1,r2,t}  = $f[t] := muls($f[r1], $f[r2]) 

      (* Floating point unary operators *)
      rtl cvtf2i : #n bits -> #m bits
      rtl FCPY_S{f,t} = $f[t] := $f[f]
      rtl FCPY_D{f,t} = $f[t] := $f[f]
      rtl FCPY_Q{f,t} = $f[t] := $f[f]
      rtl FABS_S{f,t} = $f[t] := fabs($f[f])
      rtl FABS_D{f,t} = $f[t] := fabs($f[f]) 
      rtl FABS_Q{f,t} = $f[t] := fabs($f[f])
      rtl FSQRT_S{f,t} = $f[t] := fsqrt($f[f])
      rtl FSQRT_D{f,t} = $f[t] := fsqrt($f[f])
      rtl FSQRT_Q{f,t} = $f[t] := fsqrt($f[f])
      rtl FRND_S{f,t}  = $f[t] := cvtf2i($f[f])
      rtl FRND_D{f,t}  = $f[t] := cvtf2i($f[f])
      rtl FRND_Q{f,t}  = $f[t] := cvtf2i($f[f])

      (* Floating point/fix point conversion operators *)
      rtl fcnvff_sd fcnvff_sq fcnvff_ds fcnvff_dq
          fcnvff_qs fcnvff_qd fcnvxf_s fcnvxf_d
          fcnvxf_q fcnvfx_s fcnvfx_d fcnvfx_q
          fcnvfxt_s fcnvfxt_d fcnvfxt_q  
           : #n bits -> #n bits

      rtl FCNVFF_SD{f,t} = $f[t] := fcnvff_sd $f[f]
      rtl FCNVFF_SQ{f,t} = $f[t] := fcnvff_sq $f[f]
      rtl FCNVFF_DS{f,t} = $f[t] := fcnvff_ds $f[f]
      rtl FCNVFF_DQ{f,t} = $f[t] := fcnvff_dq $f[f]
      rtl FCNVFF_QS{f,t} = $f[t] := fcnvff_qs $f[f]
      rtl FCNVFF_QD{f,t} = $f[t] := fcnvff_qd $f[f]
         (* fixed point -> floating point *)
      rtl FCNVXF_S{f,t} = $f[t] := fcnvxf_s $f[f]
      rtl FCNVXF_D{f,t} = $f[t] := fcnvxf_d $f[f]
      rtl FCNVXF_Q{f,t} = $f[t] := fcnvxf_q $f[f]
         (* floating point -> fixed point (use current rounding mode?) *)
      rtl FCNVFX_S{f,t} = $f[t] := fcnvfx_s $f[f]
      rtl FCNVFX_D{f,t} = $f[t] := fcnvfx_d $f[f]
      rtl FCNVFX_Q{f,t} = $f[t] := fcnvfx_q $f[f]
         (* floating point -> fixed point (and truncate) *)
      rtl FCNVFXT_S{f,t} = $f[t] := fcnvfxt_s $f[f]
      rtl FCNVFXT_D{f,t} = $f[t] := fcnvfxt_d $f[f]
      rtl FCNVFXT_Q{f,t} = $f[t] := fcnvfxt_q $f[f]

      (* Floating point branch *)
      fun FBRANCH cmp {f1,f2,t} =
          if cmp($f[f1],$f[f2]) then Jmp(%%t) else ()

      rtl FBRANCH_ ^^
          [?, !<=>, ==, ?=, !<>, !?>=, <, ?<,
           !>=, !?>, <=, ?<=, !>, !?<=, >, ?>,
           !<=, !?<, >=, ?>=, !<, !?=, <>, !=,
           !?, <=>, ?<>] =
          map FBRANCH
          [|?|, |!<=>|, |==|, |?=|, |!<>|, |!?>=|, |<|, |?<|,
           |!>=|, |!?>|, |<=|, |?<=|, |!>|, |!?<=|, |>|, |?>|,
           |!<=|, |!?<|, |>=|, |?>=|, |!<|, |!?=|, |<>|, |!=|,
           |!?|, |<=>|, |?<>|]
          
   end (* RTL *)

   structure Instruction = 
   struct
   
      datatype fmt! = SGL 0w0 | DBL 0w1 | QUAD 0w3 
   
      datatype loadi :Op! = LDW 0x12 (* p5-28 *)
                          | LDH 0x11 (* p5-29 *)
                          | LDB 0x10 (* p5-30 *) 
   
      datatype store :Op! = STW 0x1A (* p5-31 *) 
                          | STH 0x19 (* p5-32 *)
                          | STB 0x18 (* p5-33 *) 
   
          (* addressing mode
           * when the u bit is set, the index "x" is scaled by the size 
           * when the m bit is set, the base is also auto-incremented
           *)
   
      datatype load :ext4! = 
                           (* ext4, u, m *)
        LDWX    "ldwx"    (0w2,0w0,0w0) (* p5-36 *)
      | LDWX_S  "ldwx,s"  (0w2,0w1,0w0)
      | LDWX_M  "ldwx,m"  (0w2,0w0,0w1)
      | LDWX_SM "ldwx,sm" (0w2,0w1,0w1)
      | LDHX    "ldhx"    (0w1,0w0,0w0) (* p5-37 *)
      | LDHX_S  "ldhx,s"  (0w1,0w1,0w0)
      | LDHX_M  "ldhx,m"  (0w1,0w0,0w1)
      | LDHX_SM "ldhx,sm" (0w1,0w1,0w1)
      | LDBX    "ldbx"    (0w0,0w0,0w0) (* p5-38 *)
      | LDBX_M  "ldbx,m"  (0w0,0w0,0w1) 
   
      (* All branching is done with nullification *)
      datatype cmp! = COMBT 0wx20 
                    | COMBF 0wx22
   
      datatype cmpi! = COMIBT 0wx21
                     | COMIBF 0wx23
   
      datatype arith! = 
        ADD     0x18  (* p5-83 *)
      | ADDL    0x28  (* p5-84 *)
      | ADDO    0x38  (* p5-85 *)
      | SH1ADD  0x19  (* p5-88 *)
      | SH1ADDL 0x29  (* p5-89 *)
      | SH1ADDO 0x39  (* p5-90 *)
      | SH2ADD  0x1A  (* p5-91 *)
      | SH2ADDL 0x2A  (* p5-92 *)
      | SH2ADDO 0x3A  (* p5-93 *)
      | SH3ADD  0x1B  (* p5-94 *)
      | SH3ADDL 0x2B  (* p5-95 *)
      | SH3ADDO 0x3B  (* p5-96 *)
      | SUB     0x10  (* p5-97 *)
      | SUBO    0x30  (* p5-98 *)
      | OR      0x09  (* p5-105 *)  
      | XOR     0x0A  (* p5-106 *)
      | AND     0x08  (* p5-107 *)
      | ANDCM   0x00  (* p5-108 *)
   
      datatype arithi! = 
        ADDI  (0wx2d,0w0)
      | ADDIO (0wx2d,0w1)
      | ADDIL 
      | SUBI  (0wx25,0w0) 
      | SUBIO (0wx25,0w1) 
    
      datatype shiftv! = VEXTRU | VEXTRS | ZVDEP
    
      datatype shift! = EXTRU  | EXTRS | ZDEP
    
      datatype farith! =       (* sop, fmt *)
           FADD_S  "fadd,sgl"   (0w0, 0w0) 
         | FADD_D  "fadd,dbl"   (0w0, 0w1)  
         | FADD_Q  "fadd,quad"  (0w0, 0w3)
     
         | FSUB_S  "fsub,sgl"   (0w1, 0w0)
         | FSUB_D  "fsub,dbl"   (0w1, 0w1)
         | FSUB_Q  "fsub,quad"  (0w1, 0w3)
   
         | FMPY_S  "fmpy,sgl"   (0w2, 0w0)
         | FMPY_D  "fmpy,dbl"   (0w2, 0w1)
         | FMPY_Q  "fmpy,quad"  (0w2, 0w3)
   
         | FDIV_S  "fdiv,sgl"   (0w3, 0w0)
         | FDIV_D  "fdiv,dbl"   (0w3, 0w1)
         | FDIV_Q  "fdiv,quad"  (0w3, 0w3)
   
         | XMPYU    (* ok *)
   
      datatype funary! =      (* sop, fmt *)
           (* copy *)
           FCPY_S  "fcpy,sgl"    (0w2,0w0)
         | FCPY_D  "fcpy,dbl"    (0w2,0w1)
         | FCPY_Q  "fcpy,quad"   (0w2,0w3)
   
         | FABS_S  "fabs,sgl"    (0w3,0w0)
         | FABS_D  "fabs,dbl"    (0w3,0w1)
         | FABS_Q  "fabs,quad"   (0w3,0w3)
   
         | FSQRT_S  "fsqrt,sgl"  (0w4,0w0)
         | FSQRT_D  "fsqrt,dbl"  (0w4,0w1)
         | FSQRT_Q  "fsqrt,quad" (0w4,0w3)
     
           (* round float to integer *) 
         | FRND_S  "frnd,sgl"    (0w5,0w0)
         | FRND_D  "frnd,dbl"    (0w5,0w1)
         | FRND_Q  "frnd,quad"   (0w5,0w3)
   
       (* FCNVXF --- the source is the LHS single precision floating register *)
       datatype fcnv =                   (* sop, sf, df *)
            (* floating point -> floating point *)
           FCNVFF_SD "fcnvff,sgl,dbl"    (0w0,0w0,0w1)
         | FCNVFF_SQ "fcnvff,sgl,quad"   (0w0,0w0,0w3)
         | FCNVFF_DS "fcnvff,dbl,sgl"    (0w0,0w1,0w0)
         | FCNVFF_DQ "fcnvff,dbl,quad"   (0w0,0w1,0w3)
         | FCNVFF_QS "fcnvff,quad,sgl"   (0w0,0w3,0w0)
         | FCNVFF_QD "fcnvff,quad,dbl"   (0w0,0w3,0w1)
   
            (* fixed point -> floating point *)
         | FCNVXF_S  "fcnvxf,,sgl"       (0w1,0w0,0w0)
         | FCNVXF_D  "fcnvxf,,dbl"       (0w1,0w0,0w1) 
         | FCNVXF_Q  "fcnvxf,,quad"      (0w1,0w0,0w3)
   
            (* floating point -> fixed point (use current rounding mode?) *)
         | FCNVFX_S  "fcnvfx,sgl,"       (0w2,0w0,0w0)
         | FCNVFX_D  "fcnvfx,dbl,"       (0w2,0w1,0w0)
         | FCNVFX_Q  "fcnvfx,quad,"      (0w2,0w3,0w0)
   
            (* floating point -> fixed point (and truncate) *)
         | FCNVFXT_S "fcnvfxt,sgl,"      (0w3,0w0,0w0)
         | FCNVFXT_D "fcnvfxt,dbl,"      (0w3,0w1,0w0)
         | FCNVFXT_Q "fcnvfxt,quad,"     (0w3,0w3,0w0)
   
      datatype fstore! = FSTDS 
                       | FSTWS  
                                              (* Op, uid, u, m *)
      datatype fstorex! = FSTDX    "fstdx"    (0wxb,0w0,0w0,0w0)
                        | FSTDX_S  "fstdx,s"  (0wxb,0w0,0w1,0w0)
                        | FSTDX_M  "fstdx,m"  (0wxb,0w0,0w0,0w1)
                        | FSTDX_SM "fstdx,sm" (0wxb,0w0,0w1,0w1)
                        | FSTWX    "fstwx"    (0wx9,0w1,0w0,0w0)
                        | FSTWX_S  "fstwx,s"  (0wx9,0w1,0w1,0w0)
                        | FSTWX_M  "fstwx,m"  (0wx9,0w1,0w0,0w1)
                        | FSTWX_SM "fstwx,sm" (0wx9,0w1,0w1,0w1)
   
      (* FLDWX and FLDWS -- loads the RHS of the floating register *)
                                             (* Op, uid, u, m *)
      datatype floadx! = FLDDX    "flddx"    (0wxb,0w0,0w0,0w0)
                       | FLDDX_S  "flddx,s"  (0wxb,0w0,0w1,0w0)
                       | FLDDX_M  "flddx,m"  (0wxb,0w0,0w0,0w1)
                       | FLDDX_SM "flddx,sm" (0wxb,0w0,0w1,0w1)
                       | FLDWX    "fldwx"    (0wx9,0w1,0w0,0w0)
                       | FLDWX_S  "fldwx,s"  (0wx9,0w1,0w1,0w0)
                       | FLDWX_M  "fldwx,m"  (0wx9,0w1,0w0,0w1)
                       | FLDWX_SM "fldwx,sm" (0wx9,0w1,0w1,0w1)
                                       
      datatype fload! = FLDDS   
                      | FLDWS 
   
          (* page 5-5. fields for (c,f) *)
      datatype bcond! = EQ   "="   0w1
                      | LT   "<"   0w2
                      | LE   "<="  0w3
                      | LTU  "<<"  0w4
                      | LEU  "<<=" 0w5
                      | NE   "<>"   (* unimplemented *)
                      | GE   ">="   (* ... *)
                      | GT   ">"   
                      | GTU  ">>"  
                      | GEU  ">>=" 
   
         (* table 5-7 *)
      datatype bitcond! = BSET "<"  0w2  (* bit is 1 *)
                        | BCLR ">=" 0w6  (* bit is 0 *)
   
         (* table 6-13 *)
      datatype fcond [0..31] = 
         False_ "false?" | False "false" | ? | !<=> | == | EQT "=T" | ?= | !<> 
       | !?>= | < | ?< | !>= | !?> | <= | ?<= | !> 
       | !?<= | > | ?> | !<= | !?< | >= | ?>= 
       | !< | !?= | <> | != | NET "!=T" | !? | <=> | True_ "true?" | True "true"
   
      datatype scond = ALL_ZERO | LEFTMOST_ONE | LEFTMOST_ZERO | RIGHTMOST_ONE
                     | RIGHTMOST_ZERO 
   
      datatype field_selector = F 
                              | S
                              | D
                              | R 
                              | T 
                              | P
   
      datatype ea = 
          Direct of $GP
        | FDirect of $GP
        | Displace of {base: $GP, disp:int}
   
      datatype operand =
          REG of $GP   (* this is used only during instruction selection *)
        | IMMED of int ``<int>''  
        | LabExp of LabelExp.labexp * field_selector ``<labexp>''
        | HILabExp of LabelExp.labexp * field_selector ``<labexp>''
        | LOLabExp of LabelExp.labexp * field_selector ``<labexp>''

      datatype addressing_mode = 
        DISPea of C.cell * operand    (* displacement *)
      | INDXea of C.cell * C.cell       (* indexed *)
      | INDXSCALEDea of C.cell * C.cell (* indexed with scaling (b,x) *)

   end  (* Instruction *)

   (* ========================= Instruction Encoding =========================
    * 
    * HP has 41 different instruction formats.  
    * The instruction encoding is, for the lack of a better phrase, 
    * all fucked up.
    *
    * See Appendix C.
    *)
   instruction formats 32 bits
      (* sr=0 for load store, why? *)
     Load{Op:6,b:GP 5,t:GP 5,s:2=0,im14:signed 14}
   | Store{st:store 6,b:GP 5,r:GP 5,s:2=0,im14:signed 14}

         (* sr=3, m=0 no modify, cc=0 *)
   | IndexedLoad{Op:6,b:GP 5,x:GP 5,s:2=3,u:1,_:1=0,cc:2=0,ext4:4,m:1,t:GP 5}

   | ShortDispLoad{Op:6,b:GP 5,im5:signed 5,s:2,a:1,_:1=1,cc:2,ext4:4,m:1,t:GP 5}
   | ShoftDispShort{Op:6,b:5,r:5,s:2,a:1,_:1=1,cc:2,ext4:4,m:1,im5:signed 5}

   | LongImmed{Op:6,r:GP 5,im21:signed 21}

   | Arith{Op:6=0x2,r2:GP 5,r1:GP 5,c:3=0,f:1=0,a:arith 6,_:1=0,t:GP 5}
   | Arithi{Op:6,r:GP 5,t:GP 5,c:3=0,f:1=0,e:1,im11:signed 11}

   | Extract{Op:6,r:GP 5,t:GP 5,c:3=0,ext3:3,p:int 5,clen:int 5}

   | Deposit{Op:6,t:GP 5,r:GP 5,c:3=0,ext3:3,cp:int 5,clen:int 5}

   | Shift{Op:6,r2:GP 5,r1:GP 5,c:3=0,ext3:3,cp:5,t:GP 5}
   | ConditionalBranch{Op:6,r2:GP 5,r1:GP 5,c:bcond 3,w1:11,n:bool 1,w:1}
   | ConditionalBranchi{Op:6,r2:GP 5,im5:5,c:bcond 3,w1:11,n:bool 1,w:1}
   | BranchExternal{Op:6,b:GP 5,w1:5,s:3,w2:11,n:bool 1,w:1}
   | BranchAndLink{Op:6,t:GP 5,w1:5,ext3:3,w2:11,n:bool 1,w:1}
   | BranchVectored{Op:6,t:GP 5,x:GP 5,ext3:3,_:11=0,n:bool 1,w:1=0}
   | Break{Op:6,im13:signed 13,ext8:8,im5:signed 5}
   | BranchOnBit{Op:6=0x31,p:int 5,r:GP 5,c:3,w1:11,n:bool 1,w:1}

   | MoveToControlReg{Op:6,t:CR 5,r:GP 5,rv:3,ext8:8,_:5=0}
   | Compare{Op:6=0wx2,r2:GP 5,r1:GP 5,c:3,f:1,ext:6,_:1=0,t:GP 5}  


     (* floating point loads and stores *)
   | CoProcShort{Op:6,b:GP 5,im5:5,s:2,a:1,_:1=1,cc:2=0,
                 ls:1,uid:3,m:1=0,rt:FP 5}
   | CoProcIndexed{Op:6,b:GP 5,x:GP 5,s:2,u:1,_:1=0,cc:2=0,
                   ls:1,uid:3,m:1,rt:FP 5}

        (* OR r0,r0,r0 *)
   | NOP{Op:6=0x2,r2:5=0,r1:5=0,c:3=0,f:1=0,a:6=0x9,_:1=0,t:5=0}

   | Nop{nop} = if nop then NOP{} else ()

     (* floating point ops *)
   | FloatOp0Maj0C{Op:6=0x0C,r:FP 5,_:5=0,sop:3,fmt:2,_:6=0,t:FP 5}
   | FloatOp1Maj0C{Op:6=0x0C,r:FP 5,_:4=0,sop:2,df:2,sf:2,_:2=1,_:4=0,t:FP 5}
   | FloatOp2Maj0C{Op:6=0x0C,r1:FP 5,r2:FP 5,sop:3,fmt:2,_:2=2,_:3=0,n:1,c:5}
   | FloatOp3Maj0C{Op:6=0x0C,r1:FP 5,r2:FP 5,sop:3,fmt:2,_:2=3,_:3=0,n:1,t:FP 5}

   | FloatOp0Maj0E{Op:6=0x0E,r:FP 5,_:5=0,sop:3,fmt:2,_:3=0,r2:1,t2:1,_:1=0,
                   t:FP 5}
   | FloatOp1Maj0E{Op:6=0x0E,r:FP 5,_:4=0,sop:2,df:2,sf:2,_:2=1,_:1=0,r2:1,t2:1,
                   _:1=0,t:FP 5}
   | FloatOp2Maj0E{Op:6=0x0E,r1:FP 5,r2:FP 5,sop:3,r22:1,f:1,_:2=2,_:1=0,
                   r11:1,_:2=0,c:5}
   | FloatOp3Maj0E{Op:6=0x0E,r1:FP 5,r2:FP 5,sop:3,r22:1,f:1,_:2=3,_:1=0,
                   r11:1,_:2=0,t:FP 5}
   | FloatMultiOp{Op:6=0x0E,rm1:5,rm2:5,ta:5,ra:5,f:1,tm:5}

     (* page 6-62 *)
   | FTest{Op:6=0x0C,r1:5=0,r2:5=0,sop:3=1,_:2=0,_:2=2,_:3=0,_:1=1,c:5=0}

   structure Assembly = 
   struct
      fun emit_n false = () | emit_n true = emit ",n"
      fun emit_nop false = () | emit_nop true = emit "\n\tnop"
   end

   (*
    * Various utility functions for emitting assembly code
    *)
   structure MC =
   struct
      fun opn opnd = 
      let fun hi21 n  = (itow n) >> 0w11
          fun hi21X n = (itow n) ~>> 0w11
          fun lo11 n  = (itow n) && 0wx7ff 
          (* BUG: should respect the field selectors instead of ignoring them *)
      in  case opnd of
            I.HILabExp(lexp, _) => hi21X(LabelExp.valueOf lexp)
          | I.LOLabExp(lexp, _) => lo11(LabelExp.valueOf lexp)
          | I.LabExp(lexp, _)   => itow(LabelExp.valueOf lexp)
          | I.IMMED i           => itow i 
          | I.REG _             => error "REG"
      end

     (* compute displacement address *)
     fun disp lab = itow((Label.addrOf lab) - !loc - 8) ~>> 0w2
     fun low_sign_ext_im14 n = ((n &&0wx1fff) << 0w1)||((n && 0wx2000) >> 0w13)
     fun low_sign_ext_im11 n = ((n && 0wx3ff) << 0w1)||((n &&  0wx400) >> 0w10)
     fun low_sign_ext_im5 n  = ((n &&   0wxf) << 0w1)||((n &&   0wx10) >>  0w4)

     fun assemble_3 n = 
     let val w1 = (n && 0w4) >> 0w2
         val w2 = (n && 0w3) << 0w1
     in  w1 || w2 end 

     fun assemble_12 n = 
     let val w = (n && 0wx800) >> 0w11
         val w1 = ((n && 0wx3ff) << 0w1) || ((n && 0wx400) >> 0w10)
     in  (w1, w) end

     fun assemble_17 n = 
     let val w = (n && 0wx10000) >> 0w16
         val w1 = (n && 0wxf800) >> 0w11
         val w2 =  (((n && 0wx3ff) << 0w1) || ((n && 0wx400) >> 0w10))
     in (w, w1, w2) end

     fun assemble_21 disp = 
     let val w =
          (((disp && 0wx000003) << 0w12) ||
          ((disp && 0wx00007c) << 0w14) ||
          ((disp && 0wx000180) << 0w7) ||
          ((disp && 0wx0ffe00) >> 0w8) ||
          ((disp && 0wx100000) >> 0w20))
     in  w end 

     fun branchLink(Op,t,lab,ext3,n) =
     let val (w,w1,w2) = assemble_17(disp lab)
     in  BranchAndLink{Op,t,w1,w2,w,ext3,n} end

     fun bcond(cmp,bc,r1,r2,n,t,nop) =
     let val (w1,w) = assemble_12(disp t)
     in  ConditionalBranch{Op=emit_cmp cmp,c=bc,r1,r2,n,w,w1}; Nop{nop} end

     fun bcondi(cmpi,bc,i,r2,n,t,nop) = 
     let val (w1,w) = assemble_12(disp t)
     in  ConditionalBranchi{Op=emit_cmpi cmpi,c=bc,
                            im5=low_sign_ext_im5(itow i),r2,n,w,w1}; Nop{nop}
     end
     fun branchOnBit(bc,r,p,n,t,nop) = 
     let val (w1,w) = assemble_12(disp t)
     in  BranchOnBit{p=p,r=r,c=emit_bitcond bc,w1=w1,n=n,w=w}; Nop{nop} 
     end

     fun cmpCond cond =
        case cond of
          I.EQ   => (0w1,0w0)
        | I.LT   => (0w2,0w0)
        | I.LE   => (0w3,0w0)
        | I.LTU  => (0w4,0w0)
        | I.LEU  => (0w5,0w0)
        | I.NE   => (0w1,0w1)
        | I.GE   => (0w2,0w1)
        | I.GT   => (0w3,0w1)
        | I.GTU  => (0w4,0w1)
        | I.GEU  => (0w5,0w1)

   end (* MC *)

   (* FLDWS, FLDWX = define the R half of the FP register.
    * FSTWS = uses the R half of the FP register.
    *)
   instruction 
      LOADI of {li:loadi, r: $GP, i:operand, t: $GP, mem:Region.region}
        asm: ``<li>\t<i>(<r>), <t><mem>'' 
        mc:  Load{Op=emit_loadi li,b=r,im14=low_sign_ext_im14(opn i),t=t}
        rtl: [[ li ]]

    | LOAD of {l:load, r1: $GP, r2: $GP, t: $GP, mem:Region.region}
        asm: ``<l>\t<r2>(<r1>), <t><mem>''
        mc:  let val (ext4,u,m) = emit_load l
             in  IndexedLoad{Op=0w3,b=r1,x=r2,ext4,u,t,m} 
             end
        rtl: [[ l ]]

    | STORE of {st:store,b: $GP,d:operand,r: $GP, mem:Region.region}
        asm: ``<st>\t<r>, <d>(<b>)<mem>''
        mc:  Store{st,b=b,im14=low_sign_ext_im14(opn d),r=r}
        rtl: [[ st ]]

    | ARITH of {a:arith,r1: $GP, r2: $GP, t: $GP}
        asm: ``<a>\t<r1>, <r2>, <t>''
        mc:  Arith{a,r1,r2,t}
        rtl: [[ a ]]

    | ARITHI  of {ai:arithi, i:operand, r: $GP, t: $GP}
        asm: ``<ai>\t<i>, <r>, <t>''
        mc:  (case ai of
                I.ADDIL => LongImmed{Op=0wxa,r=r,im21=assemble_21(opn i)}
              | _ => let val (Op,e) = emit_arithi ai
                     in  Arithi{Op,r,t,im11=low_sign_ext_im11(opn i),e}
                     end
             )
        rtl: [[ ai ]]

      (* This is a composite instruction. 
       * The effect is the same as t <- if r1 cc r2 then i+b else 0
       *   if t1 = t2
       * COMCLR,cc r1, r2, t1
       * LDO       i(b),  t2 
       *)
    | COMCLR_LDO of {cc:bcond, r1: $GP, r2: $GP, t1 : $GP, 
                     i:int, b: $GP, t2: $GP}
        asm: (``comclr,<cc>\t<r1>, <r2>, <t1>\n\t'';
              ``ldo\t<i>(<b>), <t2>''
             )
        mc: let val (c,f) = cmpCond cc
            in  Compare{r1,r2,t=t1,c,f,ext=0wx22};
                Load{Op=0wx0d,b,im14=low_sign_ext_im14(itow i),t=t2}
            end
	rtl: if t1 = t2 then [[ "COMCLR_LDO2_" cc ]]
	     else if t1 = 0 then [[ "COMCLR_LDO3_" cc ]]
             else [[ "COMCLR_LDO_" cc ]]

    | SHIFTV  of {sv:shiftv, r: $GP, len:int, t: $GP}
        asm: ``<sv>\t<r>, <len>, <t>''
        mc:  (case sv of
               I.VEXTRU => Extract{Op=0wx34,r,t,ext3=0w4,p=0,clen=32-len}
             | I.VEXTRS => Extract{Op=0wx34,r,t,ext3=0w5,p=0,clen=32-len}
             | I.ZVDEP  => Deposit{Op=0wx35,t,r,ext3=0w0,cp=0,clen=32-len}
             )
        rtl: [[ sv ]]

    | SHIFT   of {s:shift, r: $GP,  p:int,  len:int, t: $GP}
        asm: ``<s>\t<r>, <p>, <len>, <t>''
        mc:  (case s of
               I.EXTRU => Extract{Op=0wx34,r,t,ext3=0w6,p=p,clen=32-len}
             | I.EXTRS => Extract{Op=0wx34,r,t,ext3=0w7,p=p,clen=32-len}
             | I.ZDEP  => Deposit{Op=0wx35,t,r,ext3=0w2,cp=31-p,clen=32-len}
             )
        rtl: [[ s ]]

    | BCOND   of {cmp: cmp, bc:bcond,r1: $GP,r2: $GP,n:bool,nop:bool,
                  t:Label.label, f:Label.label}
        asm: ``<cmp>,<bc><n>\t<r1>, <r2>, <t><nop>''
        mc:  bcond(cmp,bc,r1,r2,n,t,nop)
	rtl: [[ cmp "_" bc ]]
        padding: nop = true
        nullified: n = true
             when nullified then
             if branching forwards then delayslot when not branching 
                                   else delayslot when branching
           else delayslot
        delayslot candidate: false

    | BCONDI  of {cmpi: cmpi, bc:bcond, i:int,  r2: $GP, n:bool, nop:bool,
                  t:Label.label, f:Label.label}
        asm: ``<cmpi>,<bc><n>\t<i>, <r2>, <t><nop>''
        mc: bcondi(cmpi,bc,i,r2,n,t,nop)
	rtl: [[ cmpi "_" bc ]]
        padding: nop = true
        nullified:  n = true
            when nullified then
              if branching forwards then delayslot when not branching 
                                    else delayslot when branching
           else delayslot
        delayslot candidate: false

         (* bc must be either < or >= *)
    | BB of {bc:bitcond,r: $GP, p:int, n:bool, nop:bool,
             t:Label.label, f:Label.label}
        asm: ``bb,<bc><n>\t<r>, <p>, <t><nop>''
        mc: branchOnBit(bc,r,p,n,t,nop)
	rtl: [[ "BB_" bc ]]
        padding: nop = true
        nullified: n = true
           when nullified then
             if branching forwards then delayslot when not branching 
                                   else delayslot when branching
           else delayslot
        delayslot candidate: false

    | B of {lab:Label.label, n:bool}
        asm: ``b<n>\t<lab>''
        mc:  branchLink(0wx3a,0,lab,0w0,n)
	rtl: [[ "B" ]]
        nullified: n = true
        delayslot candidate: false

    | BE of {b: $GP, d:operand, sr:int, n:bool, labs: Label.label list}
        asm: ``be<n>\t<d>(<sr>,<b>)''
        mc:  let val (w,w1,w2) = assemble_17(opn d)
             in  BranchExternal{Op=0wx38,b=b,w1=w1,s=assemble_3(itow sr),
                                w2=w2,n=n,w=w}
                end
        nullified: n = true
        delayslot candidate: false

    | BV of {x: $GP, b: $GP, labs: Label.label list, n:bool}
        asm: ``bv<n>\t<x>(<b>)''
        mc: BranchVectored{Op=0wx3a,t=b,x=x,ext3=0w6,n=n}
	rtl: [[ "BV" ]]
        nullified: n = true
        delayslot candidate: false

    | BLR of {x: $GP, t: $GP, labs: Label.label list, n:bool}
        asm: ``blr<n>\t<x>(<t>)''
        mc:  BranchVectored{Op=0wx3a,t=t,x=x,ext3=0w2,n=n}
        nullified: n = true
        delayslot candidate: false

    | BL of {x:operand,t: $GP, defs: C.cellset, uses:C.cellset, 
             mem:Region.region, n:bool}
        asm: ``bl<n>\t<x>), <t><mem><emit_defs(defs)><emit_uses(uses)>''
        (* not implemented *) 
        nullified: n = true
        delayslot candidate: false

    | BLE of {d:operand,b: $GP, sr:int, t: $GP,
              defs: C.cellset, uses:C.cellset, mem:Region.region}
        asm: ``ble\t<d>(<emit_int sr>,<b>)<mem><
               emit_defs(defs)><emit_uses(uses)>''
        mc:  (case (d,t) of
               (I.IMMED 0,31) =>
                 BranchExternal{Op=0wx39,b=b,w1=0w0,s=assemble_3(itow sr),
                                w2=0w0,n=true,w=0w0}
             | _ => error "BLE: not implemented"
             )
	rtl: [[ "BLE" ]]
        nullified: false
        delayslot candidate: false

      (* BLE implicitly defines %r31. The destination register t 
       * is assigned in the delay slot.
       *)
    | LDIL of {i:operand,  t: $GP}
        asm: ``ldil\t<i>, <t>''
        mc:  LongImmed{Op=0wx8,r=t,im21=assemble_21(opn i)}
	rtl: [[ "LDIL" ]]

    | LDO of {i:operand,  b: $GP, t: $GP}
        asm: ``ldo\t<i>(<b>), <t>''
        mc:  Load{Op=0wx0d,b,im14=low_sign_ext_im14(opn i),t=t}
	rtl: if b = 0 then [[ "LDO2" ]] else [[ "LDO" ]]

    | MTCTL of {r: $GP, t: $CR}
        asm: ``mtctl\t<r>, <t>''
        mc:  MoveToControlReg{Op=0w0,t,r,rv=0w0,ext8=0wxc2}
	rtl: [[ "MTCTL" ]]

    | FSTORE  of {fst:fstore,b: $GP, d:int, r: $FP,mem:Region.region}
        asm: ``<fst>\t<d>(<b>), <r><mem>''
        mc: (case fst of
              I.FSTDS => CoProcShort{Op=0wxb,b,im5=low_sign_ext_im5(itow d),
                                     s=0w0,a=0w0,ls=0w1,uid=0w0,rt=r}
            | I.FSTWS => CoProcShort{Op=0wx9,b,im5=low_sign_ext_im5(itow d),
                                     s=0w0,a=0w0,ls=0w1,uid=0w1,rt=r}
            )
        rtl: [[ fst ]]

    | FSTOREX of {fstx:fstorex, b: $GP, x: $GP,r: $FP,mem:Region.region}
        asm: ``<fstx>\t<x>(<b>), <r><mem>''
        mc:  let val (Op,uid,u,m) = emit_fstorex fstx   
             in  CoProcIndexed{Op=Op,b,x,s=0w0,u,m,ls=0w1,uid=uid,rt=r}
             end
        rtl: [[ fstx ]]

    | FLOAD   of {fl:fload, b: $GP, d:int, t: $FP, mem:Region.region}
        asm: ``<fl>\t<d>(<b>), <t><mem>''
        mc:  (case fl of
               I.FLDDS => CoProcShort{Op=0wxb,b,im5=low_sign_ext_im5(itow d),
                                      s=0w0,a=0w0,ls=0w0,uid=0w0,rt=t}
             | I.FLDWS => CoProcShort{Op=0wx9,b,im5=low_sign_ext_im5(itow d),
                                      s=0w0,a=0w0,ls=0w0,uid=0w1,rt=t}
             )
        rtl: [[ fl ]]

    | FLOADX of {flx:floadx, b: $GP, x: $GP, t: $FP, mem:Region.region}
        asm: ``<flx>\t<x>(<b>), <t><mem>''
        mc:  let val (Op,uid,u,m) = emit_floadx flx
             in  CoProcIndexed{Op=Op,b,x,s=0w0,u,m,ls=0w0,uid=uid,rt=t}
             end
        rtl: [[ flx ]]

    | FARITH of {fa:farith,r1: $FP, r2: $FP,t: $FP}
        asm: ``<fa>\t<r1>, <r2>, <t>''
        mc:  (case fa of
               I.XMPYU => FloatOp3Maj0E{sop=0w2,f=0w1,r1,r2,t,r11=0w0,r22=0w0}
             | _ => let val (sop,fmt) = emit_farith fa 
                    in  FloatOp3Maj0C{sop,r1,r2,t,n=0w0,fmt} end
             )
	rtl: [[ fa ]]

    | FUNARY of {fu:funary,f: $FP, t: $FP}
        asm: ``<fu>\t<f>, <t>''
        mc:  let val (sop,fmt) = emit_funary fu
             in  FloatOp0Maj0C{r=f,t=t,sop=sop,fmt=fmt}
             end
	rtl: [[ fu ]]

    | FCNV of {fcnv:fcnv, f: $FP, t: $FP}
        asm: ``<fcnv>\t<f>, <t>''
        mc:  let val (sop,sf,df) = emit_fcnv fcnv
             in  FloatOp1Maj0E{r=f,t=t,sop=sop,sf=sf,df=df,r2=0w1,t2=0w0}
             end
	rtl: [[ fcnv ]]

 (* The following three instructions have been replaced by FBRANCH.
    This make life much easier for instruction schedulers.
    | FCMP    of fcond * int * int
    | FTEST
    | FBCC    of {t:Label.label, f:Label.label, n:bool}
 *)
    | FBRANCH of {cc:fcond, fmt:fmt, f1: $FP, f2: $FP,
                  t:Label.label, f:Label.label, n:bool, long:bool}
        asm: (``fcmp,<fmt>,<cc>\t<f1>, <f2>\n\t'';
              ``ftest\n\t'';
              ``b<n>\t<t>''
             )
         (* fmt = 1 means double precision; will have to extend later *)
        mc: (FloatOp2Maj0C{r1=f1,r2=f2,sop=0w0,fmt=emit_fmt fmt,
                       n=0w0,c=emit_fcond cc};
             FTest{};
             branchLink(0wx3a,0,t,0w0,n) (* B,n t *)
            )
	rtl: [[ "FBRANCH_" cc ]]
        nullified: n
        delayslot candidate: false

    | BREAK   of {code1:int, code2:int}
        asm: ``break\t<code1>, <code2>''
        delayslot candidate: false

    | NOP
        asm: ``nop''
        mc: NOP{}
	rtl: [[ "NOP" ]]

    | COPY of {dst: $GP list, src: $GP list, 
               impl:instruction list option ref, tmp: ea option}
        asm: emitInstrs (Shuffle.shuffle{regmap,tmp,src,dst})
	rtl: [[ "COPY" ]]

    | FCOPY of {dst: $FP list, src: $FP list, 
                impl:instruction list option ref, tmp: ea option}
        asm: emitInstrs (Shuffle.shufflefp{regmap,tmp,src,dst})
	rtl: [[ "FCOPY" ]]

    | ANNOTATION of {i:instruction, a:Annotations.annotation}
        asm: (emitInstr i; comment(Annotations.toString a))
        mc:  emitInstr i
	rtl: [[ #i ]]
end

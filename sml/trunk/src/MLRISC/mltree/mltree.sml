(* mltree.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 *)

functor MLTreeF(structure Constant  : CONSTANT
                structure Region    : REGION
                structure Stream    : INSTRUCTION_STREAM
                structure Extension : MLTREE_EXTENSION
               ) : MLTREE =
struct
  structure Constant = Constant
  structure PseudoOp = Stream.P
  structure Stream = Stream
  structure Region = Region
  structure Basis  = MLTreeBasis
  structure Extension = Extension
  structure I = MachineInt  

  type ty  = Basis.ty
  type fty = Basis.fty
  type var = CellsBasis.cell (* variable *)
  type src = var (* source variable *)
  type dst = var (* destination variable *)
  type reg = var (* physical register *)
  type an  = Annotations.annotation

  datatype cond = datatype Basis.cond
  datatype fcond = datatype Basis.fcond
  datatype rounding_mode = datatype Basis.rounding_mode
  datatype ext = datatype Basis.ext

  (* Statements/effects.  These types are parameterized by the statement
   * extension type.  Unfortunately, this has to be made polymorphic to make
   * it possible for recursive type definitions to work. 
   *)
  datatype stm =
      (* assignment *)
      MV      of ty * dst * rexp   
    | CCMV    of dst * ccexp
    | FMV     of fty * dst * fexp	

      (* parallel copies *)
    | COPY    of ty * dst list * src list   
    | FCOPY   of fty * dst list * src list

      (* control flow *)
    | JMP     of rexp * controlflow 
    | BCC     of ccexp * Label.label
    | CALL    of {funct:rexp, targets:controlflow,
                  defs:mlrisc list, uses:mlrisc list,
                  region: Region.region} 
    | RET     of controlflow 
    | IF      of ccexp * stm * stm   

      (* memory update: ea, data *)
    | STORE  of ty * rexp * rexp * Region.region 
    | FSTORE of fty * rexp * fexp * Region.region 

      (* control dependence *)
    | REGION of stm * ctrl

    | SEQ    of stm list   (* sequencing *)
    | DEFINE of Label.label   (* define local label *)

    | ANNOTATION of stm * an
    | EXT of sext  (* extension *)

      (* RTL operators:
       * The following are used internally 
       * for describing instruction semantics.
       * The frontend must not use these.
       *)
    | PHI    of {preds:int list,block:int}
    | ASSIGN of ty * rexp * rexp
    | SOURCE 
    | SINK  
    | RTL    of {hash:word, attribs:Basis.attribs ref, e:stm}

  and rexp = 
      REG    of ty * reg            

      (* sizes of constants are inferred by context *)
    | LI     of I.machine_int                 
    | LABEL  of Label.label
    | CONST  of Constant.const
    | LABEXP of rexp

    | NEG    of ty * rexp                      
    | ADD    of ty * rexp * rexp    
    | SUB    of ty * rexp * rexp    

      (* signed multiplication etc. *)
    | MULS   of ty * rexp * rexp    
    | DIVS   of ty * rexp * rexp    
    | QUOTS  of ty * rexp * rexp    
    | REMS   of ty * rexp * rexp    

      (* unsigned multiplication etc. *)
    | MULU   of ty * rexp * rexp    
    | DIVU   of ty * rexp * rexp    
    | REMU   of ty * rexp * rexp    

      (* trapping versions of above. These are all signed *)
    | NEGT   of ty * rexp                       
    | ADDT   of ty * rexp * rexp    
    | SUBT   of ty * rexp * rexp    
    | MULT   of ty * rexp * rexp    
    | DIVT   of ty * rexp * rexp    
    | QUOTT  of ty * rexp * rexp    
    | REMT   of ty * rexp * rexp    

      (* bit operations *)
    | ANDB   of ty * rexp * rexp    
    | ORB    of ty * rexp * rexp    
    | XORB   of ty * rexp * rexp    
    | EQVB   of ty * rexp * rexp
    | NOTB   of ty * rexp           

    | SRA    of ty * rexp * rexp    (* value, shift *) 
    | SRL    of ty * rexp * rexp    
    | SLL    of ty * rexp * rexp    

      (* type promotion/conversion *)
    | SX     of ty * ty * rexp  (* toTy, fromTy *) 
    | ZX     of ty * ty * rexp  (* toTy, fromTy *)
    | CVTF2I of ty * rounding_mode * fty * fexp 

      (* 
       * COND(ty,cc,e1,e2):
       * Evaluate into either e1 or e2, depending on cc.  
       * Both e1 and e2 are allowed to be evaluated eagerly.
       *)
    | COND of ty * ccexp * rexp * rexp  

      (* integer load *)
    | LOAD of ty * rexp * Region.region 

      (* predication *)
    | PRED of rexp * ctrl 

    | LET of stm * rexp

    | REXT of ty * rext

    | MARK of rexp * an

    | OP    of ty * oper * rexp list 
    | ARG   of ty * rep ref * string 
    | $     of ty * CellsBasis.cellkind * rexp
    | PARAM of int 
    | BITSLICE of ty * (int * int) list * rexp
    | ???   

  and rep  = REP of string

  and oper = OPER of Basis.misc_op
 
  and fexp =
      FREG   of fty * src
    | FLOAD  of fty * rexp * Region.region 

    | FADD   of fty * fexp * fexp
    | FMUL   of fty * fexp * fexp
    | FSUB   of fty * fexp * fexp 
    | FDIV   of fty * fexp * fexp
    | FABS   of fty * fexp 
    | FNEG   of fty * fexp
    | FSQRT  of fty * fexp
    | FCOND  of fty * ccexp * 
                fexp * fexp
    | FCOPYSIGN of fty * fexp (*sign*) * fexp (*magnitude*)

    | CVTI2F of fty * ty * rexp  (* from signed integer *)
    | CVTF2F of fty * fty * fexp (* float to float conversion *)

    | FPRED of fexp * ctrl
 
    | FEXT of fty * fext

    | FMARK of fexp * an

  and ccexp =
      CC     of Basis.cond * src                        
    | FCC    of Basis.fcond * src                       
    | TRUE                                              
    | FALSE                                             
    | NOT    of ccexp                     
    | AND    of ccexp * ccexp   
    | OR     of ccexp * ccexp   
    | XOR    of ccexp * ccexp   
    | EQV    of ccexp * ccexp   
    | CMP    of ty * Basis.cond * rexp * rexp
    | FCMP   of fty * Basis.fcond * fexp * fexp
    | CCMARK of ccexp * an
    | CCEXT  of ty * ccext

  and mlrisc = 
      CCR of ccexp 
    | GPR of rexp 
    | FPR of fexp 

  withtype controlflow = Label.label list (* control flow info *)
       and ctrl   = var                   (* control dependence info *)
       and ctrls  = ctrl list
       and sext   = (stm, rexp, fexp, ccexp) Extension.sx
       and rext   = (stm, rexp, fexp, ccexp) Extension.rx
       and fext   = (stm, rexp, fexp, ccexp) Extension.fx
       and ccext  = (stm, rexp, fexp, ccexp) Extension.ccx
       and labexp = rexp

  (*
   * Instruction streams
   *)
  type ('i,'cellset) stream = ('i, an list, 'cellset) Stream.stream 

  (* 
   * Extension mechanism
   *)

  datatype ('instr,'cellset,'operand,'addressing_mode) reducer =
    REDUCER of
    { reduceRexp    : rexp -> reg,
      reduceFexp    : fexp -> reg,
      reduceCCexp   : ccexp -> reg,
      reduceStm     : stm * an list -> unit,
      operand       : rexp -> 'operand,
      reduceOperand : 'operand -> reg,
      addressOf     : rexp -> 'addressing_mode,
      emit          : 'instr * an list -> unit,
      instrStream   : ('instr,'cellset) stream,
      mltreeStream  : (stm,mlrisc list) stream
    }

  (*
   * Useful type abbreviations for working for MLTree.
   *)
  type rewriter =  (* rewriting functions *)
    {stm:stm->stm, rexp:rexp->rexp, fexp:fexp->fexp, ccexp:ccexp->ccexp}
  type 'a folder = (* aggregation functions *)
    {stm:stm*'a->'a, rexp:rexp*'a->'a, fexp:fexp*'a->'a, ccexp:ccexp*'a->'a}
  type hasher =    (* hashing functions *)
    {stm:stm->word, rexp:rexp->word, fexp:fexp->word, ccexp:ccexp->word}
  type equality =  (* comparison functions *)
    {stm:stm * stm->bool, rexp:rexp * rexp->bool, 
     fexp:fexp * fexp->bool, ccexp:ccexp * ccexp->bool}
  type printer =   (* pretty printing functions *)
    {stm:stm->string, rexp:rexp->string, fexp:fexp->string, ccexp:ccexp->string,
     dstReg : ty * var -> string, srcReg : ty * var -> string}

end (* MLTREE *)


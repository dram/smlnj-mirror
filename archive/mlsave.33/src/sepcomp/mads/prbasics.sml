(* prbasics.sml *)

structure PrBasics : PR_BASICS =
struct 
  structure B = Basics
(*  local            compiler bug: local not implemented *)
    structure Print: BASICS' =
    struct
    type access = string
    type 'a arraykey = string
    type 'a arrayconts = string
    type bool= string
    type fixity = string
    type int = string
    type label = string
    type 'a list = string
    type lvar= string
    type 'a option= string
    type polysign = string
    type 'a ref= string
    type sharespec = string
    type spath = string
    type stamp = string
    type string = string
    type symbol = string
    type 'a intmap = string

(* internal types *)
    type binding = string
    type bool3  = string
    type conrep= string
    type datacon  = string
    type fixityVar = string
    type Functor= string
    type functorVar   = string
    type ind_var = string
    type rowty= string
    type signatureVar= string
    type stampInfo= string
    type strkind= string
    type Structure= string
    type structureVar = string
    type thinning= string
    type trans= string
    type tvkind = string
    type ty = string
    type tyckind= string
    type tycon= string
    type tyfun= string
    type tyvar  = string
    type var= string
                  (* `CONSTRUCTORS' *)

(*    type bool3  *)

      fun c0 str = str
      and c1 str s1 = str ^ "(" ^ s1 ^ ")"
      and c2 str (s1,s2) = str ^ "(" ^ s1 ^ "," ^ s2 ^ ")"
      and c3 str (s1,s2,s3) = str ^ "(" ^ s1 ^ "," ^ s2 ^ "," ^ s3 ^ ")"
      and c4 str (s1,s2,s3,s4) = 
             str ^ "(" ^ s1 ^ "," ^ s2 ^ "," ^ s3 ^ "," ^ s4 ^")"
      and c5 str (s1,s2,s3,s4,s5) = 
             str ^ "(" ^ s1 ^ "," ^ s2 ^ "," ^ s3 ^ "," ^ s4 ^ "," ^ s5 ^ ")"
      and c6 str (s1,s2,s3,s4,s5,s6) = 
             str ^ "(" ^ s1 ^ "," ^ s2 ^ "," ^ s3 ^ "," ^ s4 ^ "," ^ s5 ^ ","
                 ^ s6 ^")"

      val yes: bool3 = c0 "YES"
      and no: bool3 = c0 "NO"
      and maybe: bool3 = c0 "MAYBE"


(*    type fixity *)

      val Nonfix: fixity = c0 "NONfix"
      and Infix: int * int -> fixity = c2 "INfix"


(*    type conrep *)

      val undecided: conrep = c0 "UNDECIDED"
      and tagged: int -> conrep = c1 "TAGGED"
      and constant: int -> conrep = c1 "CONSTANT"
      and transparent: conrep = c0 "TRANSPARENT"
      and transu: conrep = c0 "TRANSU"
      and transb: conrep = c0 "TRANSB"
      and REF: conrep = c0 "REF"
      and variable: access -> conrep = c1 "VARIABLE"


(*    type tvkind *)

      val ibound: int -> tvkind = c1 "IBOUND"
      and meta: int * int * bool -> tvkind = c3 "META"
      and instantiated: ty -> tvkind = c1 "INSTANTIATED"
      and ubound: symbol * int * bool -> tvkind = c3 "UBOUND"

(*    type datacon  *)

      val datacon: symbol * bool * ty * conrep * conrep list -> datacon
                 = c5 "DATACON"
(*    type tyckind *)

      val abstyc: tyckind = c0 "ABStyc"
      and deftyc: tyfun -> tyckind = c1 "DEFtyc"
      and datatyc: datacon list -> tyckind = c1 "DATAtyc"
      and recordtyc: symbol list -> tyckind = c1 "RECORDtyc"
      and undeftyc: symbol list option -> tyckind = c1 "UNDEFtyc"

(*    type tycon *)

      val tycon: int * int * bool3 ref * symbol list * tyckind -> tycon
               = c5 "TYCON"
      and indtyc: int list -> tycon = c1 "INDtyc"

(*    type ty *)

      val varty: tvkind ref -> ty = c1 "VARty"
      and conty: tycon ref * ty list -> ty = c2 "CONty"
      and flexrecordty: rowty ref -> ty = c1 "FLEXRECORDty"
      and polyty: (int * bool) list * tyfun -> ty = c2 "POLYty"
      and undefty: ty = c0 "UNDEFty"
      and errorty: ty = c0 "ERRORty"

(*    type rowty *)

      val Open: (symbol * ty) list -> rowty = c1 "OPEN"
      and closed: ty -> rowty = c1 "CLOSED"

(*    type tyfun *)
      val tyfun: int * ty -> tyfun = c2 "TYFUN"

(*    type var *)

      val valvar: access * symbol * ty ref -> var = c3 "VALvar"
      and ovldvar: symbol * ind_var list ref * tyfun-> var = c3 "OVLDvar"
      and unknownvar: symbol -> var = c1 "UNKNOWNvar"

(*    type strkind *)
      val strkind: symbol list -> strkind = c1 "STRkind"
      and sigkind: ((symbol list * symbol list)list *
                   (symbol list * symbol list)list) *
                   binding list * int * int          -> strkind
                 = fn ((s1,s2),s3,s4,s5)=> c5 "SIGkind" (s1,s2,s3,s4,s5)

(*    type Structure *)
      val strstr: int * int * (string * binding) intmap * 
                  tycon arraykey * Structure arraykey * strkind -> Structure
                = c6 "STRstr"
      and indstr: int -> Structure = c1 "INDstr"

(*    type  Functor *)
      val Functor: symbol * Structure * Structure * int -> Functor
                 = c4 "FUNCTOR"
(*    type signatureVar *)
      val sigvar: symbol * Structure -> signatureVar = c2 "SIGvar"

(*    type structureVar *)
      val strvar: symbol * access * Structure -> structureVar
                = c3 "STRvar"
(*    type functorVar    *)
      val fctvar: symbol * access * Functor -> functorVar= c3 "FCTvar"

(*    type fixityVar    *)
      val fixvar: symbol * fixity -> fixityVar =
                  c2 "FIXvar"

(*    type binding  *)
      val varbind: var -> binding = c1 "VARbind"
      and conbind: datacon -> binding = c1 "CONbind"
      and tycbind: tycon ref -> binding = c1 "TYCbind"
      and tyvbind: tvkind ref -> binding = c1 "TYVbind"
      and sigbind: signatureVar -> binding = c1 "SIGbind"
      and strbind: structureVar -> binding = c1 "STRbind"
      and fctbind: functorVar -> binding = c1 "FCTbind"
      and fixbind: fixityVar -> binding = c1 "FIXbind"


(*    type trans *)
      val valtrans: access -> trans = c1 "VALtrans"
      and thintrans: access * lvar * trans list -> trans = c3 "THINtrans"
      and contrans: datacon -> trans = c1 "CONtrans"


    end (* Print *)
(*  in (* result components of  PR_BASICS *) compiler bug: local not impl *)
(*  end;                                     compiler bug: local not impl *)

  structure Generators: GENERATORS=
  struct
      structure B = B
      structure B' = Print
              (* the following  functions are called just once for 
                 each ref in the source structure *)
      val new_bool3_ref: B.bool3 ref -> B'.bool3 B'.ref =
          let val i = ref 0
           in fn _ => (inc i; "bool3r" ^ makestring (!i))
          end
      val new_tvkind_ref: B.tvkind ref -> B'.tvkind B'.ref =
          let val i = ref 0
           in fn _ => (inc i; "tvkindr" ^ makestring (!i))
          end
      val new_tycon_ref: B.tycon ref -> B'.tycon B'.ref =
          let val i = ref 0
           in fn _ => (inc i; "tyconr" ^ makestring (!i))
          end
      val new_rowty_ref: B.rowty ref -> B'.rowty B'.ref =
          let val i = ref 0
           in fn _ => (inc i; "rowtyr" ^ makestring (!i))
          end
      val new_ty_ref: B.ty ref -> B'.ty B'.ref =
          let val i = ref 0
           in fn _ => (inc i; "tyr" ^ makestring (!i))
          end
      val new_indvarlist_ref: {indicator:B.ty, variant: B.var}list ref->
                               B'.ind_var B'.list B'.ref =
          let val i = ref 0
           in fn _ => (inc i; "indvarlistr" ^ makestring (!i))
          end
      val new_bindinglist_ref: B.binding list ref -> 
                               B'.binding B'.list B'.ref =
          let val i = ref 0
           in fn _ => (inc i; "bindinglistr" ^ makestring (!i))
          end
      val new_tycon_array: B.tycon array -> B'.tycon B'.arraykey =
          let val i = ref 0
           in fn _ => (inc i; "tycona" ^ makestring (!i))
          end
      val new_Structure_array: B.Structure array ->
                               B'.Structure B'.arraykey =
          let val i = ref 0
           in fn _ => (inc i; "Structurea" ^ makestring (!i))
          end

  end;  (*  Generators *)

  structure Assemblers: ASSEMBLERS =
  struct
      fun asmb_list(l: string list): string =
          "[" ^ asmb_conts l ^ "]"
      and asmb_conts [] = ""
        | asmb_conts [last] = last
        | asmb_conts (first::second::rest) = first ^ "," ^ 
              asmb_conts (second::rest)

            structure B = Basics
            structure B': BASICS' = Print

            (* assembling target values *)
            (* lists : *)
            val asmb_binding_list: B'.binding list -> B'.binding B'.list
                = asmb_list
            and asmb_conrep_list: B'.conrep list -> B'.conrep B'.list
                = asmb_list
            and asmb_datacon_list: B'.datacon list -> B'.datacon B'.list
                = asmb_list
            and asmb_indvar_list: B'.ind_var list -> B'.ind_var B'.list
                = asmb_list
            and asmb_intXstringXbinding_list: 
                 (B'.int * (B'. string * B'.binding)) list ->
                 (B'.int * (B'.binding * B'.binding)) B'.list
                = asmb_list o (map (fn (s1,(s2,s3))=> 
                             implode["(",s1,",",s2,",",s3,")"]))
            and asmb_int_list: B'.int list -> B'.int B'.list
                = asmb_list
            and asmb_int_x_bool_list: (B'.int * B'.bool) list -> 
                                      (B'.int * B'.bool) B'.list
                = asmb_list o (map (fn (s1,s2)=> "("^s1^","^s2^")"))
            and asmb_symblistXsymblist_list: 
                 (B'.symbol B'.list * B'.symbol B'.list) list -> 
                 (B'.symbol B'.list * B'.symbol B'.list) B'.list
                = asmb_list o (map (fn (s1,s2)=> "("^s1^","^s2^")"))
            and asmb_symbol_list: B'.symbol list -> B'.symbol B'.list
                = asmb_list
            and asmb_symb_x_ty_list: (B'.symbol * B'.ty) list -> 
                                     (B'.symbol * B'.ty)  B'.list
                = asmb_list o (map (fn (s1,s2)=> "("^s1^","^s2^")"))
            and asmb_trans_list: B'.trans list -> B'.trans B'.list
                = asmb_list
            and asmb_ty_list: B'.ty list -> B'.ty B'.list
                = asmb_list

            (* options :  *)
            fun asmb_option(opt: string option): string =
            case opt of NONE => "NONE"
                      | SOME str => "SOME(" ^ str ^ ")"

            val asmb_symbol_list_option: B'.symbol B'.list option ->
                                        B'.symbol B'.list B'.option
                = asmb_option
            and asmb_int_option: B'.int option -> B'.int B'.option
                = asmb_option
            and asmb_etc_option: (B'.lvar * B'.trans B'.list) option ->
                                 (B'.lvar * B'.trans B'.list) B'.option
                = asmb_option o (fn SOME(s1,s2)=>SOME ("("^s1^","^s2^")")
                                  | NONE => NONE)

            (* arrays:  *)
            val asmb_tycon_array: B'.tycon list -> B'.tycon B'.arrayconts
                = asmb_list
            val asmb_Structure_array: B'.Structure list ->
                                              B'.Structure B'.arrayconts
                = asmb_list

            (* misc *)
            val asmb_ind_var: B'.ty * B'.var -> B'.ind_var =
                (fn (s1,s2) => "(" ^ s1 ^ "," ^ s2 ^ ")")

            val asmb_symtable :(B'.int * (B'.string * B'.binding)) B'.list -> 
                               (B'.string * B'.binding) B'.intmap
                = (fn s=>s)

   end; (* Assemblers *)

  structure ExternalReps: EXTERNAL_REPS=
  struct
            structure B = B
            structure B' = Print

            val access_rep: B.Access.access -> B'.access=
                B.Access.pr_access
            val bool_rep: bool -> B'.bool =
                (fn true => "TRUE"
                  | false => "FALSE")
            val int_rep: int -> B'.int =
                 makestring
            val lvar_rep: B.Access.lvar -> B'.lvar =
                B.Access.pr_lvar
            val string_rep: string -> B'.string = (fn s => s)
            val symbol_rep: B.Symbol.symbol -> B'.symbol =
                fn s => implode[B.Symbol.name s ,"(", 
					 makestring(B.Symbol.number s), ")"]
            val stampInfo_rep: B.stampInfo -> B'.stampInfo =
                B.pr_stampInfo

  end; (* ExternalReps *)



 
 structure PrintB : HOM =
   HOM(structure B = B
       structure B' = Print
       structure Generators = Generators
       structure Assemblers = Assemblers
       structure ExternalReps = ExternalReps
      );

                    (* RESULT *)

   type dir = PrintB.dir
   val emptydir = PrintB.emptydir
   val pr_binding = PrintB.eval_binding
   val pr_int = PrintB.eval_int
   val pr_lvar = PrintB.eval_lvar
   val pr_stampInfo = PrintB.eval_stampInfo
   fun pr_strenv strenv dir = let val ((s1,s2),dir) = PrintB.eval_strenv 
                                                      strenv dir
                               in ( "{s= "^s1^", t= "^s2^"}", dir)
                              end
   val pr_symbol = PrintB.eval_symbol
   val pr_symtable = PrintB.eval_symtable
   val pr_ty = PrintB.eval_ty

   fun pr_dir(dir:dir):string =
       let fun pr_list(l: (string*string) list): string =
                    "\n\n[" ^ pr_conts l ^ "\n]"
           and pr_conts [] = ""
             | pr_conts [(l,r)] = "\n\n(" ^ l ^ ",\n" ^ r ^ ")"
             | pr_conts ((l,r)::second::rest) = 
        "\n\n(" ^ l ^ ",\n" ^ r ^ ")," ^ pr_conts (second::rest)
           val bool3 = PrintB.bool3_store dir
           and tvkind = PrintB.tvkind_store dir
           and tycon= PrintB.tycon_store dir
           and rowty = PrintB.rowty_store dir
           and ty  = PrintB.ty_store dir
           and ind_var_list = PrintB.ind_var_list_store dir
           and binding_list = PrintB.binding_list_store dir
           and tycona = PrintB.tycona_store dir
           and Structurea = PrintB.Structure_store dir;
            
           fun lengths (l:'a list):string = makestring(length l)
       in
          "\nbool3r:        " ^ lengths bool3
        ^ "\ntvkindr:       " ^ lengths tvkind
        ^ "\ntyconr:        " ^ lengths tycon
        ^ "\nrowtyr:        " ^ lengths rowty
        ^ "\nty:            " ^ lengths ty
        ^ "\nind_var_listr: " ^ lengths ind_var_list
        ^ "\nbinding_listr: " ^ lengths binding_list
        ^ "\ntycona:        " ^ lengths tycona
        ^ "\nStructurea:    " ^ lengths Structurea
        ^ "\n\n"
        ^ pr_list bool3
        ^ pr_list tvkind
        ^ pr_list tycon
        ^ pr_list rowty
        ^ pr_list ty
        ^ pr_list ind_var_list
        ^ pr_list binding_list
        ^ pr_list tycona
        ^ pr_list Structurea
      end (* pr_dir *)

end;   (* PR_BASICS *)

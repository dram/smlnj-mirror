(* hom.sig *)

signature HOM=
  sig 
    structure B:  BASICS sharing B=Basics
    structure B': BASICS'           

    type dir
    val emptydir: dir
    
    val eval_binding: B.binding -> dir -> B'.binding * dir
    and eval_int: int -> dir -> B'.int * dir
    and eval_lvar: B.Access.lvar -> dir -> B'.lvar * dir
    and eval_stampInfo: B.stampInfo -> dir -> B'.stampInfo * dir
    and eval_strenv: {s: B.Structure array, t: B.tycon array} -> dir ->
                     (B'.Structure B'.arraykey * B'.tycon B'.arraykey)*dir
    and eval_symbol: B.Symbol.symbol -> dir -> B'.symbol * dir
    and eval_symtable: (string * B.binding) B.Intmap.intmap -> dir ->
                        (B'.string * B'.binding) B'.intmap * dir
    and eval_ty: B.ty -> dir -> B'.ty * dir
    
   (* listing of resulting target store *)

   val bool3_store: dir -> (B'.bool3 B'.ref * B'.bool3) list
   val tvkind_store: dir -> (B'.tvkind B'.ref * B'.tvkind) list
   val tycon_store: dir -> (B'.tycon B'.ref * B'.tycon) list
   val rowty_store: dir -> (B'.rowty B'.ref * B'.rowty) list
   val ty_store: dir -> (B'.ty B'.ref * B'.ty) list
   val ind_var_list_store: dir -> 
                           (B'.ind_var B'.list B'.ref * B'.ind_var B'.list) list
   val binding_list_store: dir -> 
                           (B'.binding B'.list B'.ref * B'.binding B'.list) list
   val tycona_store: dir -> (B'.tycon B'.arraykey * B'.tycon B'.arrayconts) list
   val Structure_store: dir -> (B'.Structure B'.arraykey *
                                B'.Structure B'.arrayconts) list
  end

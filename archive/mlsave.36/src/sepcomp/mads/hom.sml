(* hom.sml   traversal of static information implemented as
   a homomorphism  *)

signature GENERATORS=
sig
            structure B: BASICS sharing B=Basics
            structure B': BASICS'
              (* the following  functions are called just once for 
                 each ref in the source structure *)
            val new_bool3_ref: B.bool3 ref -> B'.bool3 B'.ref
            val new_tvkind_ref: B.tvkind ref -> B'.tvkind B'.ref
            val new_tycon_ref: B.tycon ref -> B'.tycon B'.ref
            val new_rowty_ref: B.rowty ref -> B'.rowty B'.ref
            val new_ty_ref: B.ty ref -> B'.ty B'.ref
            val new_indvarlist_ref: {indicator:B.ty, variant: B.var}list ref->
                                    B'.ind_var B'.list B'.ref
            val new_bindinglist_ref: B.binding list ref -> 
                                     B'.binding B'.list B'.ref

            val new_tycon_array: B.tycon array -> B'.tycon B'.arraykey
            val new_Structure_array: B.Structure array ->
                                              B'.Structure B'.arraykey
end;

signature ASSEMBLERS=
sig
            structure B: BASICS sharing B=Basics
            structure B': BASICS'

            (* assembling target values *)

            (* lists *)

            val asmb_binding_list: B'.binding list -> B'.binding B'.list
            and asmb_conrep_list: B'.conrep list -> B'.conrep B'.list
            and asmb_datacon_list: B'.datacon list -> B'.datacon B'.list
            and asmb_indvar_list: B'.ind_var list -> B'.ind_var B'.list
(* from 24           and asmb_intXsymbolXbindinglistref_list:
                   (B'.int * (B'.symbol * B'.binding B'.list B'.ref)) list ->
                   (B'.int * (B'.symbol * B'.binding B'.list B'.ref)) B'.list
*)
(* from edinb. upd. of 24           and asmb_intXbinding_list:
                  (B'.int * B'.binding)list ->
                  (B'.int * B'.binding)B'.list
*)
            and asmb_intXstringXbinding_list:
                   (B'.int * (B'.string * B'.binding)) list ->
                   (B'.int * (B'.string * B'.binding)) B'.list

            and asmb_int_list: B'.int list -> B'.int B'.list
            and asmb_int_x_bool_list: 
                 (B'.int * B'.bool)list -> (B'.int * B'.bool)B'.list
            and asmb_symblistXsymblist_list: 
                 (B'.symbol B'.list * B'.symbol B'.list) list ->
                 (B'.symbol B'.list * B'.symbol B'.list) B'.list
            and asmb_symbol_list: B'.symbol list -> B'.symbol B'.list
            and asmb_trans_list: B'.trans list -> B'.trans B'.list   
            and asmb_symb_x_ty_list: 
                (B'.symbol * B'.ty)list -> (B'.symbol * B'.ty)B'.list
            and asmb_ty_list: B'.ty list -> B'.ty B'.list
            
            (* options *)

            val asmb_etc_option:
                  (B'.lvar * B'.trans B'.list) option ->
                  (B'.lvar * B'.trans B'.list) B'.option 
            val asmb_symbol_list_option: 
                  B'.symbol B'.list option -> B'.symbol B'.list B'.option
            val asmb_int_option:
                  B'.int option -> B'.int B'.option
            val asmb_tycon_array: B'.tycon list -> B'.tycon B'.arrayconts
            val asmb_Structure_array: B'.Structure list ->
                                              B'.Structure B'.arrayconts
            val asmb_ind_var: B'.ty * B'.var -> B'.ind_var
            val asmb_symtable :(B'.int * (B'.string *B'.binding)) B'.list ->
                                (B'.string * B'.binding) B'.intmap
end;

signature EXTERNAL_REPS=
sig
            structure B: BASICS sharing B=Basics
            structure B': BASICS'

            (* representation of externals *)

            val access_rep: B.Access.access -> B'.access
            val bool_rep: bool -> B'.bool
            val int_rep: int -> B'.int
            val lvar_rep: B.Access.lvar -> B'.lvar
            val string_rep: string -> B'.string
            val symbol_rep: B.Symbol.symbol -> B'.symbol
            val stampInfo_rep: B.stampInfo -> B'.stampInfo

end;

functor HOM(structure B': BASICS'             (* target *)
            structure Generators: GENERATORS
            structure Assemblers: ASSEMBLERS
            structure ExternalReps: EXTERNAL_REPS
              sharing B' = Generators.B' = Assemblers.B' = ExternalReps.B'
           ): HOM =

struct
  open Generators Assemblers ExternalReps
  structure B = Basics
  structure B' = B'
  datatype ('a, 'b) rng = PENDING of 'a
                        | COMPLETE of 'a * 'b;
	(* Each directory is an association list which, when complete,
           maps references of a particular type to pairs of particular
           type as follows: each source algebra reference is mapped to 
           a pair consisting of the target algebra representation of 
           the source reference and the target algebra representation 
           of the contents of the source reference.
             In other words, each directory is the representation of
           that reachable part of the store whose locations have
           the same type. *)

  type ('a,'b) rdir = ('a ref * ('b B'.ref, 'b)rng)list    (* references *)
  type dir1 = (B.bool3, B'.bool3)rdir
  type dir2 = (B.tvkind, B'.tvkind)rdir
  type dir3 = (B.tycon, B'.tycon)rdir
  type dir4 = (B.rowty, B'.rowty)rdir
  type dir5 = (B.ty,B'.ty)rdir
  type dir6 = ({indicator:B.ty, variant:  B.var}list, B'.ind_var B'.list)rdir
  type dir7 = (B.binding list, B'.binding B'.list)rdir
  (* arrays *)
  type ('a,'b) adir = ('a array * ('b B'.arraykey , 'b B'.arrayconts)rng)list 
  type dir8 = (B.tycon, B'.tycon)adir
  type dir9 = (B.Structure, B'.Structure)adir

  type dir = dir1 * dir2 * dir3 * dir4 * 
             dir5 * dir6 * dir7 * dir8 * dir9;

  val emptydir = ([],[],[],[],[],[],[],[],[]): dir;

  type 'a rstore = ('a B'.ref * 'a)list
   and 'a astore = ('a B'.arraykey * 'a B'.arrayconts)list

  type store' = B'.bool3 rstore *
                B'.tvkind rstore *
                B'.tycon rstore *
                B'.rowty rstore *
                B'.ty rstore *
                B'.ind_var B'.list rstore *
                B'.binding B'.list rstore *
                B'.tycon astore *
                B'.Structure astore

  fun extract_ref(PENDING(r))= r
    | extract_ref(COMPLETE(r,_))=r

  exception ListStore;
  fun listStore(dir1,dir2,dir3,dir4,dir5,dir6,dir7,dir8,dir9):store' = 
    let fun f l = fold (fn((_,COMPLETE pair),l)=> pair::l
                         |((_,PENDING _),l)=> raise ListStore) l []
     in (f dir1, f dir2, f dir3, f dir4, f dir5, f dir6, f dir7,
         f dir8, f dir9)
    end;

  exception Lookup
  fun lookup [] x' = raise Lookup
    | lookup ((x,y)::rest) x' = if x = x' then y else lookup rest x'
      
  fun update [] (x,y) = [(x,y)]
    | update ((x',y')::rest) (x,y) =
       if x=x' then (x,y)::rest
       else (x',y'):: update rest (x,y)

  (* combinators *)

  fun o1 f (x1,x2,x3,x4,x5,x6,x7,x8,x9)a = (f x1 a, x2, x3, x4, x5, x6,
                                            x7, x8, x9)
  fun o2 f (x1,x2,x3,x4,x5,x6,x7,x8,x9)a = (x1, f x2 a, x3, x4, x5, x6,
                                            x7, x8, x9)
  fun o3 f (x1,x2,x3,x4,x5,x6,x7,x8,x9)a = (x1, x2, f x3 a, x4, x5, x6,
                                            x7, x8, x9)
  fun o4 f (x1,x2,x3,x4,x5,x6,x7,x8,x9)a = (x1, x2, x3, f x4 a, x5, x6,
                                            x7, x8, x9)
  fun o5 f (x1,x2,x3,x4,x5,x6,x7,x8,x9)a = (x1, x2, x3, x4, f x5 a, x6,
                                            x7, x8, x9)
  fun o6 f (x1,x2,x3,x4,x5,x6,x7,x8,x9)a = (x1, x2, x3, x4, x5, f x6 a,
                                            x7, x8, x9)
  fun o7 f (x1,x2,x3,x4,x5,x6,x7,x8,x9)a = (x1, x2, x3, x4, x5, x6,
                                            f x7 a, x8, x9)
  fun o8 f (x1,x2,x3,x4,x5,x6,x7,x8,x9)a = (x1, x2, x3, x4, x5, x6,
                                            x7, f x8 a, x9)
  fun o9 f (x1,x2,x3,x4,x5,x6,x7,x8,x9)a = (x1, x2, x3, x4, x5, x6,
                                            x7, x8, f x9 a)
  and pi1 f (x1,x2,x3,x4,x5,x6,x7,x8,x9) a = f x1 a
  and pi2 f (x1,x2,x3,x4,x5,x6,x7,x8,x9) a = f x2 a
  and pi3 f (x1,x2,x3,x4,x5,x6,x7,x8,x9) a = f x3 a
  and pi4 f (x1,x2,x3,x4,x5,x6,x7,x8,x9) a = f x4 a
  and pi5 f (x1,x2,x3,x4,x5,x6,x7,x8,x9) a = f x5 a
  and pi6 f (x1,x2,x3,x4,x5,x6,x7,x8,x9) a = f x6 a
  and pi7 f (x1,x2,x3,x4,x5,x6,x7,x8,x9) a = f x7 a
  and pi8 f (x1,x2,x3,x4,x5,x6,x7,x8,x9) a = f x8 a
  and pi9 f (x1,x2,x3,x4,x5,x6,x7,x8,x9) a = f x9 a

  fun ID x = x	(* for applying target algebra operations *)
  fun MK0 con dir = (con,dir)
  fun MK1(nodefcn, dirtransformer1) dir=
      let val (arg1rep,dir1)= dirtransformer1 dir
       in
          (nodefcn(arg1rep),dir1)
      end
  fun MK2(nodefcn, dirtransformer1, dirtransformer2) dir=
      let val (arg1rep,dir1)= dirtransformer1 dir
          val (arg2rep,dir2)= dirtransformer2 dir1
       in
          (nodefcn(arg1rep,arg2rep),dir2)
      end
  fun MK3(nodefcn, dirtransformer1, dirtransformer2, dirtransformer3) dir=
      let val (arg1rep,dir1)= dirtransformer1 dir
          val (arg2rep,dir2)= dirtransformer2 dir1
          val (arg3rep,dir3)= dirtransformer3 dir2
       in
          (nodefcn(arg1rep,arg2rep,arg3rep),dir3)
      end

  fun MK4(nodefcn, dirtransformer1, dirtransformer2, 
                   dirtransformer3, dirtransformer4) dir=
      let val (arg1rep,dir1)= dirtransformer1 dir
          val (arg2rep,dir2)= dirtransformer2 dir1
          val (arg3rep,dir3)= dirtransformer3 dir2
          val (arg4rep,dir4)= dirtransformer4 dir3
       in
          (nodefcn(arg1rep,arg2rep,arg3rep,arg4rep),dir4)
      end

  fun MK5(nodefcn, dirtransformer1, dirtransformer2, 
                   dirtransformer3, dirtransformer4, dirtransformer5) dir=
      let val (arg1rep,dir1)= dirtransformer1 dir
          val (arg2rep,dir2)= dirtransformer2 dir1
          val (arg3rep,dir3)= dirtransformer3 dir2
          val (arg4rep,dir4)= dirtransformer4 dir3
          val (arg5rep,dir5)= dirtransformer5 dir4
       in
          (nodefcn(arg1rep,arg2rep,arg3rep,arg4rep,arg5rep),dir5)
      end

  fun MK6(nodefcn, dirtransformer1, dirtransformer2, dirtransformer3,
                   dirtransformer4, dirtransformer5, dirtransformer6) dir=
      let val (arg1rep,dir1)= dirtransformer1 dir
          val (arg2rep,dir2)= dirtransformer2 dir1
          val (arg3rep,dir3)= dirtransformer3 dir2
          val (arg4rep,dir4)= dirtransformer4 dir3
          val (arg5rep,dir5)= dirtransformer5 dir4
          val (arg6rep,dir6)= dirtransformer6 dir5
       in
          (nodefcn(arg1rep,arg2rep,arg3rep,arg4rep,arg5rep,arg6rep),dir6)
      end


  fun eval_ref  (* generic application of the homomorphism to all
                   types of references *)
       (pi_ :((('_a ref* ('b,'c)rng)list -> 
                '_a ref->
                         ('b,'c)rng)  -> dir 
            -> '_a ref  -> ('b,'c)rng),
                        (* projection *)
        o_ : ((('_a ref* ('b,'c)rng)list -> 
               ('_a ref* ('b,'c)rng)->
               ('_a ref* ('b,'c)rng)list )  -> dir 
            -> ('_a ref *('b,'c)rng) -> dir),
                        (* coordinate composition *) 
       ref_maker: '_a ref -> 'b, 
                        (* creation of a new (target) reference *)
       eval_conts: '_a -> dir -> ('c * dir),
                        (* function to evaluate contents of reference *)
       r: '_a ref)     (* reference *)
       (dir:dir)        (* directory*)
      =
    let val r' = extract_ref(pi_ lookup dir r)
        in (r',dir)
    end
    handle Lookup =>
    let val new = ref_maker r
        val dir1 = o_ update dir (r,PENDING new)
        val (conts_rep,dir2)= eval_conts (!r) dir1
     in (new, o_ update dir2(r,COMPLETE(new,conts_rep)))
    end

  fun eval_array  (* generic application of the homomorphism to all
                   types of arrays *)
       (pi_ :((('_a array * ('b,'c)rng)list -> 
                '_a array->
                         ('b,'c)rng)  -> dir 
            -> '_a array  -> ('b,'c)rng) ,
                        (* projection *)
        o_ : ((('_a array* ('b,'c)rng)list -> 
               ('_a array* ('b,'c)rng)->
               ('_a array* ('b,'c)rng)list )  -> dir 
            -> ('_a array *('b,'c)rng) -> dir) ,
                        (* coordinate composition *) 
       array_maker  : '_a array -> 'b  ,
                        (* creation of a new (target) array *)
       array_rep    : 'd list -> 'c,
       eval_conts (*: '_a -> dir -> ('d * dir)*) ,
                        (* function to evaluate an array element *)
       a :'_a array )     (* array *)
       (dir:dir)        (* directory*)
      =
    let val a' = extract_ref(pi_ lookup dir a)
        in (a',dir)
    end
    handle Lookup =>
    let val arraykey= array_maker a
        val dir1 = o_ update dir (a,PENDING arraykey)
        fun loop(n,dir) = 
            if n >= 0 then
               let val (conts',dir') = eval_conts (a sub n) dir
                   val (l'',dir'')=loop(n-1,dir')
                in 
                   (conts'::l'', dir'')
               end
            else ([],dir)
        val (l,dir2)  = loop(Array.length a - 1,dir1)
        val result = array_rep l
     in (arraykey, o_ update dir2(a,COMPLETE(arraykey,result)))
    end


  fun eval_list(eval: 'a -> dir -> ('b * dir))  (* evaluation of one element *)
               (asmb_list: 'b list -> 'b B'.list)
               (l : 'a list)  (* list to be evaluated *)
               (dir:dir)  : 'b B'.list * dir  =
      let fun loop(dir,[]) = ([],dir)
            | loop(dir, first::rest)=
               let val (b, dir') = eval first dir
                   val (bs, dir'') = loop(dir',rest)
                in (b::bs, dir'')
               end
          val (list,dir') = loop(dir,l)
       in
         (asmb_list list, dir')
      end

  fun eval_option (eval: 'a -> dir -> ('b * dir)) (* evaluation of contents *)
                  (asmb_option: 'b option -> 'b B'.option)
                  (opt : 'a option)
                  (dir: dir) : 'b B'.option * dir =
     
      let val (b_opt,dir') = 
             case opt of
                 NONE => (NONE, dir)
               | SOME a => let val (b,dir') =eval a dir
                           in (SOME b, dir')
                           end
       in
          (asmb_option b_opt, dir')
      end


                       (* REPRESENTATION OF EXTERNALS *)

  fun eval_bool b = MK0(bool_rep b)
  and eval_int i = MK0(int_rep i)
  and eval_string str = MK0(string_rep str)
  and eval_symbol symbol = MK0(symbol_rep symbol)
  and eval_access a = MK0(access_rep a)
  and eval_lvar lvar = MK0(lvar_rep lvar)
  and eval_stampInfo si = MK0(stampInfo_rep si)
                       (* EVALUATION FUNCTIONS *)

  fun eval_bool3_ref r = eval_ref(pi1,o1,new_bool3_ref,eval_bool3,r)

  and eval_tvkind_ref r = eval_ref(pi2,o2,new_tvkind_ref,eval_tvkind,r)

  and eval_tycon_ref r = eval_ref(pi3,o3,new_tycon_ref,eval_tycon,r)

  and eval_rowty_ref r = eval_ref(pi4,o4,new_rowty_ref,eval_rowty,r)

  and eval_ty_ref r = eval_ref(pi5,o5,new_ty_ref,eval_ty,r)

  and eval_indvarlistref r = eval_ref(pi6,o6,new_indvarlist_ref,
                                      eval_indvarlist,r)
 
  and eval_bindinglistref r = eval_ref(pi7,o7,new_bindinglist_ref,
                                       eval_bindinglist,r)

  and eval_tycon_array a = eval_array(pi8,o8,new_tycon_array,
                                   asmb_tycon_array, eval_tycon,a)

  and eval_Structure_array a = eval_array(pi9,o9,new_Structure_array, 
                                   asmb_Structure_array, eval_Structure,a)

  and eval_bool3 B.YES = MK0 B'.yes
    | eval_bool3 B.NO = MK0 B'.no
    | eval_bool3 B.MAYBE = MK0 B'.maybe

  and eval_fixity B.NONfix = MK0 B'.Nonfix
    | eval_fixity (B.INfix(i,j)) = MK2(B'.Infix, eval_int i, eval_int j)

  and eval_conrep B.UNDECIDED = MK0 B'.undecided
    | eval_conrep (B.TAGGED i) = MK1(B'.tagged, eval_int i)
    | eval_conrep (B.CONSTANT i) = MK1(B'.constant, eval_int i)
    | eval_conrep B.TRANSPARENT = MK0(B'.transparent)
    | eval_conrep B.TRANSU = MK0(B'.transu)
    | eval_conrep B.TRANSB = MK0(B'.transb)
    | eval_conrep B.REF = MK0 (B'.REF)
    | eval_conrep (B.VARIABLE(access))= MK1(B'.variable, eval_access access)

  and eval_tvkind (B.IBOUND i) = MK1(B'.ibound ,eval_int i)
    | eval_tvkind (B.META{depth,weakness,eq})=
                                      MK3(B'.meta,eval_int depth, 
                                          eval_int weakness, eval_bool eq)
    | eval_tvkind (B.INSTANTIATED ty) = MK1(B'.instantiated,eval_ty ty)
    | eval_tvkind (B.UBOUND{name,weakness,eq})=
                                      MK3(B'.ubound, eval_symbol name,
                                          eval_int weakness, eval_bool eq)
  and eval_datacon (B.DATACON{name,const,typ,rep,sign})=
         MK5(B'.datacon, eval_symbol name, eval_bool const,
             eval_ty typ, eval_conrep rep, eval_conrep_list sign)
  
  and eval_conrep_list sign = eval_list eval_conrep asmb_conrep_list sign
  
  and eval_tyckind (B.ABStyc)= MK0(B'.abstyc)
    | eval_tyckind(B.DEFtyc(tyfun))= MK1(B'.deftyc, eval_tyfun tyfun)
    | eval_tyckind(B.DATAtyc(datacon_list))= MK1(B'.datatyc,
         eval_list eval_datacon asmb_datacon_list datacon_list)
    | eval_tyckind(B.RECORDtyc symbol_list)= MK1(B'.recordtyc,
         eval_list eval_symbol asmb_symbol_list symbol_list)
    | eval_tyckind(B.UNDEFtyc symbol_list_option)= MK1(B'.undeftyc,
         eval_option (eval_list eval_symbol asmb_symbol_list) 
         asmb_symbol_list_option
         symbol_list_option)

  and eval_tycon(B.TYCON{stamp,arity,eq,path,kind})=
      MK5(B'.tycon, eval_int stamp, eval_int arity,
          eval_bool3_ref eq, eval_list eval_symbol asmb_symbol_list path,
          eval_tyckind kind)
    | eval_tycon(B.INDtyc int_list)= 
      MK1(B'.indtyc, eval_list eval_int asmb_int_list int_list)

  and eval_ty(B.VARty(tvkind_ref))=
           MK1(B'.varty, eval_tvkind_ref tvkind_ref)
    | eval_ty(B.CONty(tycon_ref, ty_list))=
           MK2(B'.conty, eval_tycon_ref  tycon_ref,
               eval_list eval_ty asmb_ty_list ty_list)
    | eval_ty(B.FLEXRECORDty(rowty_ref))=
           MK1(B'.flexrecordty, eval_rowty_ref  rowty_ref)
    | eval_ty(B.POLYty{sign, tyfun})=
           MK2(B'.polyty, eval_sign sign, eval_tyfun tyfun)
    | eval_ty(B.UNDEFty) = MK0(B'.undefty)
    | eval_ty(B.ERRORty) = MK0(B'.errorty)
    

  and eval_sign(weak_eq_list)= eval_list eval_int_x_bool 
                               asmb_int_x_bool_list weak_eq_list
  and eval_int_x_bool{weakness,eq} =  MK2(ID, eval_int weakness, eval_bool eq)

  and eval_rowty(B.OPEN(symbol_x_ty_list))=
           MK1(B'.Open, eval_list eval_symbol_x_ty asmb_symb_x_ty_list
                        symbol_x_ty_list)
    | eval_rowty(B.CLOSED ty)=
           MK1(B'.closed, eval_ty ty)

  and eval_symbol_x_ty (symb,ty) = MK2(ID, eval_symbol symb, eval_ty ty)

  and eval_tyfun(B.TYFUN{arity,body}) = 
           MK2(B'.tyfun, eval_int arity, eval_ty body)

  and eval_var(B.VALvar{access,name,vtype})=
           MK3(B'.valvar, eval_access access, eval_symbol name,
               eval_ty_ref  vtype)
    | eval_var(B.OVLDvar{name,options,scheme})=
           MK3(B'.ovldvar, eval_symbol name, eval_indvarlistref options,
               eval_tyfun scheme)
    | eval_var(B.UNKNOWNvar(symbol))=
           MK1(B'.unknownvar, eval_symbol symbol)

  and eval_indvarlist l = eval_list eval_indvar asmb_indvar_list l
  and eval_indvar{indicator,variant} = 
           MK2(asmb_ind_var, eval_ty indicator, eval_var variant)
  
  and eval_strkind(B.STRkind{path})=
         MK1(B'.strkind, eval_list eval_symbol asmb_symbol_list path)
    | eval_strkind(B.SIGkind{share,bindings,stampcounts as {s,t}})=
         MK4(B'.sigkind, eval_sharespec share, 
             eval_list eval_binding asmb_binding_list bindings,
             eval_int s, eval_int t)

  and eval_Structure(B.STRstr{stamp,sign,table,env as {t,s},kind})=
          MK6(B'.strstr, 
                 (eval_int stamp),
                 (eval_int  sign),
                 (eval_symtable table),
                 (eval_tycon_array t),
                 (eval_Structure_array s),
                 (eval_strkind kind))
    | eval_Structure(B.INDstr i) = MK1(B'.indstr, eval_int i)

  and eval_Functor(B.FUNCTOR{paramName,param,body,tycCount})=
          MK4(B'.Functor, eval_symbol paramName, eval_Structure param,
                 eval_Structure body, eval_int tycCount)

  and eval_signatureVar(B.SIGvar{name,binding})=
       MK2(B'.sigvar, eval_symbol name, eval_Structure binding)

  and eval_structureVar(B.STRvar{name,access,binding})=
       MK3(B'.strvar, eval_symbol name, eval_access access, 
           eval_Structure binding)

  and eval_functorVar(B.FCTvar{name,access,binding})=
       MK3(B'.fctvar, eval_symbol name, eval_access access, 
           eval_Functor binding)

  and eval_fixityVar(B.FIXvar{name,binding})=
       MK2(B'.fixvar, eval_symbol name, eval_fixity binding)

  and eval_binding(B.VARbind var)= MK1(B'.varbind, eval_var var)
    | eval_binding(B.CONbind datacon) = MK1(B'.conbind, eval_datacon datacon)
    | eval_binding(B.TYCbind tycon_ref)=
           MK1(B'.tycbind, eval_tycon_ref tycon_ref)
    | eval_binding(B.TYVbind tyvar) = MK1(B'.tyvbind, eval_tvkind_ref tyvar)
    | eval_binding(B.SIGbind signatureVar) = 
           MK1(B'.sigbind, eval_signatureVar signatureVar)
    | eval_binding(B.STRbind structureVar) =
           MK1(B'.strbind, eval_structureVar structureVar)
    | eval_binding(B.FCTbind functorVar)= 
           MK1(B'.fctbind, eval_functorVar functorVar)
    | eval_binding(B.FIXbind fixity)= MK1(B'.fixbind, eval_fixityVar fixity)

  and eval_binder b = eval_symbol_times_binding (b)

  and eval_symbol_times_binding(symbol,binding)=
        MK2(ID, eval_symbol symbol, eval_binding binding)

  and eval_symtable symtable d : ((B'.string * B'.binding) B'.intmap * dir)= 
      let val (b,dir') = 
                  (eval_list eval_intXstringXbinding
                             asmb_intXstringXbinding_list
                             (B.Intmap.intMapToList symtable)) d
       in (asmb_symtable b, dir')
      end

  and eval_intXstringXbinding(int,(string,binding)) = 
      MK2(ID,eval_int int, 
             MK2(ID,eval_string string,
	         eval_binding binding))

  and eval_bindinglist bl = eval_list eval_binding asmb_binding_list bl

  and eval_strenv strenv=
      let val {s,t} =  strenv
      in  MK2(ID, eval_Structure_array s, eval_tycon_array t)
      end

  and eval_trans(B.VALtrans(access))= MK1(B'.valtrans, eval_access access)
    | eval_trans(B.THINtrans(access,lvar,trans_list)) =
           MK3(B'.thintrans, eval_access access, eval_lvar lvar, 
               eval_list eval_trans asmb_trans_list trans_list)
    | eval_trans(B.CONtrans(datacon))=
           MK1(B'.contrans, eval_datacon datacon)

  and eval_thinning thinning=
      let val etc_option = thinning
       in eval_option eval_etc asmb_etc_option etc_option
      end

  and eval_etc(lvar,trans_list)=
      MK2(ID, eval_lvar lvar, eval_list eval_trans asmb_trans_list trans_list)

  and eval_sharespec {s,t} =
           MK2(ID, eval_symblistXsymblist_list s,
                   eval_symblistXsymblist_list t)

  and eval_symblistXsymblist_list l = eval_list eval_symblistXsymblist
                                      asmb_symblistXsymblist_list  l

  and eval_symblistXsymblist (l,l') = 
      MK2(ID, eval_symblist l, eval_symblist l')

  and eval_symblist l = eval_list eval_symbol asmb_symbol_list l

  val bool3_store: dir -> (B'.bool3 B'.ref * B'.bool3)list
      = (fn(x1,x2,x3,x4,x5,x6,x7,x8,x9)=>x1) o listStore
  val tvkind_store: dir -> (B'.tvkind B'.ref * B'.tvkind)list
      = (fn(x1,x2,x3,x4,x5,x6,x7,x8,x9)=>x2) o listStore
  val tycon_store: dir -> (B'.tycon B'.ref * B'.tycon)list
      = (fn(x1,x2,x3,x4,x5,x6,x7,x8,x9)=>x3) o listStore
  val rowty_store: dir -> (B'.rowty B'.ref * B'.rowty)list
      = (fn(x1,x2,x3,x4,x5,x6,x7,x8,x9)=>x4) o listStore
  val ty_store: dir -> (B'.ty B'.ref * B'.ty)list
      = (fn(x1,x2,x3,x4,x5,x6,x7,x8,x9)=>x5) o listStore
  val ind_var_list_store: dir -> 
                          (B'.ind_var B'.list B'.ref * B'.ind_var B'.list)list
      = (fn(x1,x2,x3,x4,x5,x6,x7,x8,x9)=>x6) o listStore
  val binding_list_store: dir -> 
                          (B'.binding B'.list B'.ref * B'.binding B'.list)list
      = (fn(x1,x2,x3,x4,x5,x6,x7,x8,x9)=>x7) o listStore
  val tycona_store: dir -> (B'.tycon B'.arraykey * B'.tycon B'.arrayconts)list
      = (fn(x1,x2,x3,x4,x5,x6,x7,x8,x9)=>x8) o listStore
  val Structure_store: dir -> (B'.Structure B'.arraykey *
                               B'.Structure B'.arrayconts)list
      = (fn(x1,x2,x3,x4,x5,x6,x7,x8,x9)=>x9) o listStore
end;   (* HOM *)
                            
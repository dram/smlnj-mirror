structure Ens_types2 = 
struct
    type file = string

    type location = file * int * int
    fun locFile ((f,_,_) : location) = f


    (*simplified versions of the internal compiler types*)
    datatype stub_tycon = General of Stamps.stamp * InvPath.path
			| Record of Symbol.symbol list
			| Path of InvPath.path

    datatype ty' = Conty of stub_tycon * ty' list
		 | Ibound of {index: int, depth: int}
		 | Ubound of Symbol.symbol
		 | Poly of {arity: int, body: ty'}

    datatype tycon' = Datatype of bool * Symbol.symbol list (*eq * cons list*)
		    | Abstract of Symbol.symbol list
		    | Deftyc
		    | Primtyc of bool (* : eq *)


    (*the record containing def locations ...*)
    type var_elem = { access : Access.access,
		      name : Symbol.symbol,
		      parent : Access.access,
		      typ : ty',
		      def : location, 
		      usage : (location * ty' * Access.access) list ref
		    }

    type type_elem = { tycon : tycon',
		       stamp : Stamps.stamp,
		       name : Symbol.symbol,
		       def : location, 
		       usage : location list ref
		     }
		     
    type cons_elem = { name : Symbol.symbol,
		       dataty : Stamps.stamp,
		       def : location, 
		       ty : ty',
		       usage : (location * ty') list ref
		     }

    datatype key = Var of Access.access 		
		 | Str of Access.access 
		 | Type of Stamps.stamp 
		 | Cons of Stamps.stamp * Symbol.symbol
		 | Sig of Stamps.stamp

    datatype elements = 
	     Def of (int * Symbol.symbol * key) list
	   | Constraint of (int * Symbol.symbol * int) list * Access.access
	   | Alias of Access.access
		      
    type str_elem = { name : Symbol.symbol, 
		      access : Access.access,
		      parent : Access.access option,
		      sign : Stamps.stamp option, (*pas de sig pour les alias*)
		      def : location, 
		      elements : elements,
		      usage : location list ref
		    }
		    
    datatype spec_sig = Typ of tycon'
		      | Val of ty'
		      | Exception of ty'
		      | NamedStr of Symbol.symbol * Stamps.stamp
		      | InlineStr of (Symbol.symbol * spec_sig) list

    type sig_elem = { name : Symbol.symbol,
		      stamp : Stamps.stamp,
		      inferred : bool,
		      def : location, 
		      elements : (Symbol.symbol * spec_sig) list,
		      alias : (location * Symbol.symbol) list ref, 
		      usage : (location * Symbol.symbol) list ref
		    }
		    
    type all = var_elem list * 
	       type_elem list * 
	       cons_elem list * 
	       str_elem list * 
	       sig_elem list
end

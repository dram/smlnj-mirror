(* ML-Yacc parser generator version 1.0
Copyright (c) 1989 by Andrew W. Appel, David R. Tarditi

This software comes with ABSOLUTELY NO WARRANTY.
This software is subject only to the GNU GENERAL PUBLIC LICENSE
(in the file "LICENSE", distributed with this software, and available
from the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139).
You may copy and distribute this software; see the LICENSE
for details and restrictions.
*)
signature BASESET =
   sig
      type elem
      type base_set
      exception Select_arb
      val empty: base_set
          and insert: elem ->  base_set -> base_set
	  and exists: elem -> base_set -> bool
	  and find: elem -> base_set -> elem option
	  and setfold: ((elem * 'b) -> 'b) -> base_set -> 'b -> 'b
	  and revsetfold: ((elem * 'b) -> 'b) -> base_set -> 'b -> 'b
	  and elem_gt: (elem * elem -> bool)
	  and elem_eq: (elem * elem -> bool)
          and select_arb: base_set -> elem
	  and set_eq: (base_set * base_set) -> bool
	  and set_gt: (base_set * base_set) -> bool
	  and app : (elem -> 'a) -> base_set -> unit
   end;

signature FULLSET =
   sig
      type set
      type elem
      exception Select_arb
      val card: set -> int
	  and app: (elem -> 'b) -> set -> unit
	  and set_eq: (set * set) -> bool
	  and set_gt: (set * set) -> bool
	  and find : elem -> set ->  elem option
	  and exists: elem -> set -> bool
          and contained: elem -> (set -> bool)
          and difference: set * set -> set
          and elem_eq: (elem * elem -> bool)
	  and elem_gt : (elem * elem -> bool)
          and empty: set
          and insert: elem -> set -> set
          and is_empty: set -> bool
          and make_list: set -> elem list
          and make_set: (elem list -> set)
          and remove: (elem * set) -> set
          and same_set: set * set -> bool
          and partition: (elem -> bool) -> (set -> set * set)
	  and revsetfold: ((elem * 'b) -> 'b) -> set -> 'b -> 'b
	  and setfold: ((elem * 'b) -> 'b) -> set -> 'b -> 'b
          and select_arb: set -> elem
          and singleton: (elem -> set)
          and union: set * set -> set
          and closure: set * (elem -> set) -> set
   end;

signature GRAPH =
   sig

      type node
      type edge
      type graph

      val null_graph: graph
      val nodes_of: graph -> node list
      val num_nodes: graph -> int
      val join: graph * node * edge * node -> graph

      (* drt(9/12/88) - new functions added because join is inefficient,
         if one or both of the nodes already exist *)

      val add_edge : graph * node * edge * node -> graph
      val add_node : graph * node -> graph

      (* set of graph edges: records of {From:node, Edge:edge, To:node} *)


      structure EdgeSet : FULLSET
    	 
      val edges: graph * node -> EdgeSet.set  (* all edges from a node *)
      val all_edges : graph -> EdgeSet.set

      val find_node : graph * node -> node option

end;

signature BUSY =
   sig
      val dot: unit -> unit
      val star: unit -> unit
      val print: string -> unit
      val println: string -> unit

      val withSpace: ('a -> unit) -> ('a -> unit)
      val withNewline: ('a -> unit) -> ('a -> unit)

      val withDot: ('a -> 'b) -> 'a -> 'b

      val sendto_list : unit -> unit
      val sendto_file : outstream -> unit

      val get_list : unit -> string list
   end;

signature MEMO =
   sig
      type Arg

      val memo_fn: ((Arg * Arg) -> bool) -> ((Arg -> '2a) -> (Arg -> '2a))

      exception Enum_memo_fn

      val enum_memo_fn: ((Arg -> int) * int) -> ((Arg -> '2a) -> (Arg -> '2a))

      exception Catalog

      val catalog: {tag: Arg -> 'tag,
                    ordOfTag: 'tag -> int,
                    items: Arg list
                   } -> ('tag -> Arg list)
   end;
signature V2_LR_GRAMMAR =
   sig
      datatype Terminal =


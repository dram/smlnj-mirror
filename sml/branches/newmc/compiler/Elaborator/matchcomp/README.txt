The new match compiler
----------------------

Files

-- "special" or "administrative" variables introduced by match compilation (== VarCon.var)
matchcomp/svar.sig
matchcomp/svar.sml

-- finite, ordered sets of rule numbers
matchcomp/rules.sig      
matchcomp/rules.sml      

-- basic types used by the match compiler: keys, paths, and-or trees, decision trees
matchcomp/mctypes.sml
matchcomp/mcutil.sml
matchcomp/mcprint.sml

-- the three main processes:
matchcomp/andor.sml         -- building the and-or tree for a match
matchcomp/or-queues.sml     -- building and maintaining priority queues of OR nodes
matchcomp/decisiontree.sml  -- building the decision tree

-- virtual match "code constructions" producing absyn
matchcomp/vmcexp.sml    

-- environments used in "code generation" to build the match absyn
matchcomp/svarenv.sml
matchcomp/varenvmc.sml
matchcomp/varenvac.sml

-- the main function, building the and-or tree, the decision tree, and finally the absyn "code"
matchcomp/matchcomp.sml


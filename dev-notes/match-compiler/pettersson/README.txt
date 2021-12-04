(* match-compiler/pettersson/README.txt *)
(* DBM, 2021/12 *)

This is a prototype match compiler based on Pettersson's 1992 paper in CC and
Chapter 7 of his 1997.

It has been reworked to fit better with SML by introducing first-class expression
and pattern tuples.

Patterns from a match are preprocessed to replace pattern variables by a generic
variable place-holder (PPVAR) and to record a mapping (svenv) from pattern variables
to the location (path) of their (single) occurrence in the pattern.

Then the match function is used to generate a decision tree from the preprocessed
patterns arranged in a "pattern matrix" (pmatrix, or amatrix in augmented form with
path and rule number metadata).

The notes.txt file is a "stream-of-consciousness" style write-up of my understanding
of the match algorithm and ints underlying concepts and principles.

TODO:

1. add a "FDA optimizer" that would merge decision

2. add a "code generator" that translates a decision tree (plus pattern variable
environments) into an expression, using a shallow "SWITCH" construct that binds
intermediate "match variables" to constructor destructs.

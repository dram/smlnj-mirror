(* match-compiler/augustsson/README.txt *)
(* DBM, 2021/12 *)

This is a prototype match compiler based on:

[1] Lennart Augustsson, "Compiling Pattern Matching", FPCA, 1985.
[2] Philip Wadler, "Efficient Compilation of Pattern-Matching", Chapter 5 of
The Implementation of Functional Programming Languages, Simon Peyton-Jones, 1987.

It has been reworked to fit better with SML by introducing first-class expression
and pattern tuples.

absyn.sml: defines dcons, dcon families, variables, patterns, and expressions.

match.sml:
  The match function is used to generate a decision tree from the
  match patterns.

The notes.txt file is a "stream-of-consciousness" style write-up of my understanding
of the match algorithm and ints underlying concepts and principles.

TODO:

1. add a "code generator" that translates a decision tree (plus pattern variable
environments) into an expression, using a shallow "SWITCH" construct that binds
intermediate "match variables" to constructor destructs.

2. implement a backtracking version

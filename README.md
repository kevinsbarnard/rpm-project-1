OVERVIEW
========

This project will implement a task planner in the constraint-based
(SATPlan) style, using PDDL as the modeling language and Z3 as the
constraint solver.

Please work in groups of 3-4.


SETUP
=====

Install Z3
----------

* On Debian/Ubuntu: `apt-get install z3`

* Otherwise, you may install from source:
  https://github.com/Z3Prover/z3


Register Starter Code Lisp Package
----------------------------------

1. At the shell (substite appropriate path on your machine):

       mkdir -p ~/.local/share/common-lisp/source/
       ln -s PATH/TO/STARTERCODE/trivial-satplan.asd  ~/.local/share/common-lisp/source/

2. From the lisp REPL:

       (ql:quickload :trivial-satplan)


INSTRUCTIONS
============

1. Complete the starter code skeleton (locations marked TODO)

2. Answer the questions in report PDF


STARTER CODE OUTLINE
====================

* trivial-satplan.asd: Lisp "build script"
* package.lisp: package definition
* util.lisp: Utility and helper functions
* pddl.lisp: PDDL parsing and data structures
* smtlib.lisp: Interaction with the Z3 solver
* planner.lisp: Primary SATPlan implementation
* pddl/: Example PDDL files
* trivial-satplan: Wrapper shell script


TESTING / GRADING
=================

* Code will be called for grading the same way as in the
  trivial-satplan wrapper shell script.

* To debug your code, you may find it helpful to inspect the generated
  SMTLib statements and output.  The starter code places these files
  (by default) in `/tmp/trivial-satplan.smt2` and
  `/tmp/trivial-satplan-result`.

YOUR CODE NEEDS TO PRODUCE THE CORRECT RESULT TO RECEIVE CREDIT FOR
EACH TESTCASE. MAKE SURE YOUR CODE LOADS CLEANLY WITHOUT COMPILATION
ERRORS, EXTRA I/O, OR OTHER SIDE EFFECTS.

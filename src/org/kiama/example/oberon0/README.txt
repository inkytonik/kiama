This directory contains a Kiama rendition of the Oberon-0
implementation described in Niklaus Wirth's book Compiler Construction
(Addison-Wesley, 1996). The full text of the book and Wirth's original
code can be obtained from

   http://www.cs.inf.ethz.ch/~wirth/books/CompilerConstruction/

The main program here is a driver for the three main components of
the implementation.

compiler:

A compiler from Oberon-0 to a simple RISC machine. In Wirth's book the
compiler generates object code directly, whereas we generate assembly
language which is translated to object code by a separate assembler.

assembler:

The assembler for the simple RISC architecture. Given an assembly
language program, the assembler produces object code.

RISC machine

The RISC machine targeted here is implemented in the RISC Kiama
example.

TODO:

- when an invalid type is detected (eg due to a missing or duplicated
  declaration), then it should be reported at source but not trigger
  other errors where it is used.  E.g., x + 1 where x is not declared
  should report it at x, but not also complain that + needs two ints

- some nodes do not get proper Positional information, eg VarDecls
  since the value returned by the parser is a list of VarDecls, and
  the parser combinators only try to attach position information to
  the top-level return value of a parser, not its constituents
  revisit this when we are using the Scala parser combinators again

- some aspects of encoding could perhaps be done more cleanly using
  attributes, eg computation of memory offsets or the memory sizes

- a proper test suite



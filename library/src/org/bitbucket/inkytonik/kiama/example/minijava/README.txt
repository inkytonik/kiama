Simple MiniJava compiler to JVM byte code

MiniJava is a cut-down version of Java used in teaching compiler construction.
Information about MiniJava can be found at the side of the textbook that first
introduced this subset.

    http://www.cambridge.org/us/features/052182060X/#progs

The implementation here parses MiniJava programs to build abstract syntax
trees. The trees are checked for conformance to the semantic rules of the
language. They are translated to trees that represent Java Virtual machine
programs. Finally, the JVM trees are output using the syntax of the Jasmin JVM
assembler.

    http://jasmin.sourceforge.net/

Running the compiler on file 'program.java' produces the JVM code in .j
files named after the prorgam's classes.

You can also test the executable behaviour of the code that is generated
by the Kiama tests, by running the test/exectests script. It extracts the
code from the test output files, assembles each file with Jasmin, runs
the main class and compares the standard output with the corresponding
.exp file which contains the expected output.

The assignments directory contains the homework assignments from Macquarie
University unit COMP332 Programming Languages where this code first appeared.
Sample solution write-ups are included.


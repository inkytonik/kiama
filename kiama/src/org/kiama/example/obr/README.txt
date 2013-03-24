This example is a class exercise that illustrates the use of
high-level methods to specify a full compiler. Obr is a smallish but
realistic imperative language inspired by the Oberon family of
languages. The compiler uses parser combinators to construct a source
tree representation of Obr programs and attribute definitions to
specify semantic analysis, transformation into assembly language
constructs and encoding as SPARC or a simple RISC assembly code.

Detailed information on the Obr language, source tree representations
and transformation to assembly language can be found in the doc
directory.

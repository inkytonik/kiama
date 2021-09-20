# Kiama examples

Up: [Documentation](Documentation), Prev: [User Manual](UserManual), Next: [Research](Research)

Running examples are provided in the Kiama distribution to illustrate
use of the library. The examples usually solve smaller versions of
tasks that occur regularly in real world practical language processing
tasks.

The following examples are available. Some are documented in more
detail on separate pages.

**[Dataflow](Dataflow)**

Computation of live variable information for a simplified imperative
programming language. Makes use of reference attributes to define a
control flow graph and then circular attributes to solve the dataflow
equations over this graph.

**Grammar**

Calculation of context-free grammar properties (nullability, FIRST and
FOLLOW sets for grammar symbols) using Kiama's circular attributes.

**[Imperative](Imperative)**

An imperative programming language used particularly to define tests
for the rewriting components of Kiama.

**ISWIM**

An implementation of Landin's ISWIM language and extensions based on a
variant of the SECD machine. Illustrates the use of parsing,
attribution and abstract state machines in a complete compiler for a
functional language.

**JSON**

Some simple processing of JSON structures using Scala's parser
combinaors and Kiama's rewriting and pretty-printing modules.

**[Lambda](Lambda)**

A simple version of lambda calculus, implemented using rewrite rules
that encode explicit substitution and garbage collection.

**[Lambda2](Lambda2)**

A more extended lambda calculus that includes rewrite rules that
implement a variety of different evaluation strategies including
various forms of eager and lazy evaluation.

**Lambda3**

A version of the lambda calculus implemented using name binding abstraction.
Based on the example used in Scrap your Nameplate, James Cheney, ICFP 2005.

**MiniJava**

A compiler for a cut-down version of Java. Performs parsing, semantic analysis
(using attribute grammars) and code generation to JVM byte code for processing
by the Jasmine JVM assembler. Also includes a pretty-printer.

**[Oberon0](Oberon0)**

A compiler for the Oberon-0 programming language defined in Niklaus
Wirth's book
[Compiler Construction](http://www.cs.inf.ethz.ch/~wirth/books/CompilerConstruction/).
The example makes extensive use of attributes to define and check the
static properties of programs, as well as rewriting to desugar
constructs into simpler ones. It translates Oberon-0 programs into C.

**Obr**

This example is a class exercise that illustrates the use of
high-level methods to specify a full compiler. Obr is a smallish but
realistic imperative language inspired by the Oberon family of
languages. The compiler uses parser combinators to construct a source
tree representation of Obr programs and attribute definitions to
specify semantic analysis, transformation into assembly language
constructs and encoding as SPARC assembly code.

Detailed information on the Obr language, source tree representations
and transformation to SPARC assembly language can be found in the doc
directory within the example.

**OneOhOneCompanies**

A Kiama implementation of the main problems in the
[101 companies](http://101companies.org) project, plus some extra tasks.
Mainly uses Kiama rewriting and attribution.

**PicoJava**

A cut-down version of Java, first used as an example in the
[JastAdd project](http://jastadd.cs.lth.se/examples/PicoJava/index.shtml).
PicoJava focuses on the components that affect name and type analysis
and the example code uses attributes to check basic name usage and
type compatibility rules. The attribute equations follow the lead of
the JastAdd example to allow comparison between the two approaches.

**Prolog**

An interpreted implementation of a simple dialect of the Prolog logic
programming language.

**[Repmin](Attribution#markdown-header-repmin)**

An implementation of the classic Repmin problem that is often used to
illustrate the basic capabilities of attribute grammar systems and was
first introduced as an example of writing circular programs in lazy
functional programming languages.

**RISC**

An implementation of a simple RISC machine, as defined by Wirth in his book
[Compiler Construction](http://www.cs.inf.ethz.ch/~wirth/books/CompilerConstruction/).
An assembler for the assembly language and an abstract state machine-based
simulator for the underlying machine are also provided.

**TIL**

A partial implementation of the
[Tiny Imperative Language (TIL) Chairmarks](http://strategoxt.org/Sts/TILChairmarks)
which constitute typical challenges for program analysis and transformation systems.

**Transform**

A processor for a simple expression language where the operator
priority is configured by the input (based on the example in
[\_Higher order attribute grammars\_](http://doi.acm.org/10.1145/73141.74830)
by Vogt, Swierstra and Kuiper). This example illustrates the use of
higher-order attribute grammars to compute trees as attributes of
trees and the use of attribute forwarding to automatically direct
references from the original tree to a generated tree.

Up: [Documentation](Documentation), Prev: [User Manual](UserManual), Next: [Research](Research)

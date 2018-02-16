An example based on the multipass compiler example from the paper
"Higher order attribute grammars" by Vogt, Swierstra and Kuiper,
International Conference on Programming Language Design and
Implementation (PLDI), 1989.

The example processes an expression language with user-defined
operator priority to generate a simple target language. The output is
a tree with the correct priority reflected in the form of the tree and
name analysis is performed on the priority-correct tree. This example
illustrates the use of higher order attributes to transform
structures.

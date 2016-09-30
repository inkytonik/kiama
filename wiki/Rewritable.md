# Processing arbitrary data with Kiama

Up: [Context](Context.md), Prev: [Collections](Collections.md)

IMPORTANT NOTE: This page describes Kiama 1.x. Much of it also applies
to Kiama 2.x, but not all. Please consult the 2.x release notes for the
main differences. We are currently writing comprehensive documentation
for 2.x that will eventually replace these pages.

Any Scala reference value can have attributes defined for it using
Kiama.

Rewriting is more problematic because Kiama needs to have access to
the components of a value in order to be able to rewrite it in a
generic fashion. Kiama has built-in support for rewriting of `Product`
values (see [Case Classes](CaseClasses.md)), and `GenTraversable` or `Map` collections
(see [Collections](Collections.md)), since the Scala library provides mechanisms for
accessing their components.

You can make other types of value rewritable by having them implement
Kiama's `Rewritable` trait.

```
trait Rewritable {
    import Rewriter.Term

    def arity : Int
    def deconstruct : Seq[Term]

    def reconstruct (components : Array[Term]) : Term
    protected def illegalArgs (desc : String, argtypes : String, args : Array[Term])
}
```

(`Rewriter.Term` is `Any` in the current version of Kiama.)

A `Rewritable` value must provide a way of deconstructing itself using
the `arity` method, that returns the number of children that the value
has, and the `deconstruct` method, that returns a sequence of those
children.

The `reconstruct` method is used to build a new value that is the same
as the receiver, except that its children are replaced by those in the
`components` array.  If `reconstruct` cannot accept the `components`
for some reason (e.g., wrong number or type), the `illegalArgs` method
should be called.  By default, `illegalArgs` throws an `illegalArgumentException`.

See Kiama's `imperative` example (file
[ASTNonCase.scala](https://bitbucket.org/inkytonik/kiama/src/default/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/imperative/ASTNonCase.scala))
for an example of how to define `Rewritable` instances.

Up: [Context](Context.md), Prev: [Case Classes](CaseClasses.md)

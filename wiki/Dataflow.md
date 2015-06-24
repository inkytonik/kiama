# Example: Dataflow calculation for live variable analysis

Up: [Examples](Examples.md), Next: [Imperative](Imperative.md)

This example shows how Kiama's reference attributes can be used to
define a control flow graph for an imperative language. Circular
attributes are then used to implement a fixed-point dataflow
calculation over the control flow graph to compute variable liveness.

## Abstract syntax

File: [org.kiama.example.dataflow.DataflowTree.scala](https://bitbucket.org/inkytonik/kiama/src/default/library/src/org/kiama/example/dataflow/DataflowTree.scala)

Programs for this example consist of a single statement.

```
case class Program (body : Stm) extends Attributable
```

Statements come in assignment, while loop, if conditional, block,
return and empty statement varieties. To keep the example simple we do
not define complex expressions; they can only be single variables.

```
abstract class Stm extends Attributable
case class Assign (left : Var, right : Var) extends Stm
case class While (cond : Var, body : Stm) extends Stm
case class If (cond : Var, tru : Stm, fls : Stm) extends Stm
case class Block (stms : Stm*) extends Stm
case class Return (ret : Var) extends Stm
case class Empty () extends Stm
```

Finally, variables are just represented by strings.

```
type Var = String
```

## Control flow graph

File: [org.kiama.example.dataflow.Dataflow.scala](https://bitbucket.org/inkytonik/kiama/src/default/library/src/org/kiama/example/dataflow/Dataflow.scala)

The control flow graph is defined by the `succ` attribute. `s->succ`
is the set of statements to which control can flow from statement `s`.
There are five cases:

  * flow from a conditional statement (representing its condition) to  either of its branches
  * flow from a while loop (representing its condition) to either the  statement following the loop or to the body of the loop
  * flow from a return to nowhere (representing program termination)
  * flow from a block to the first statement of the block
  * flow from any other statement to the statement following it

Thus, `succ` will be a set of references to other nodes in the tree.
The cases given above can be specified in a straight-forward way if we
assume the existence of another attribute `s->following` that gives
the statement (if any) following statement `s`.

```
val succ : Stm ==> Set[Stm] =
    attr {
        case If (_, s1, s2)   => Set (s1, s2)
        case t @ While (_, s) => t->following + s
        case Return (_)       => Set ()
        case Block (s, _*)    => Set (s)
        case s                => s->following
    }
```

`following` depends on the parent context so we use a `childAttr` to
define it. There are three main cases:

  * the body of a while loop is followed by the while loop (representing  flow back to the loop condition)
  * the last statement in a block is followed by whatever follows the block
  * any other statement in a block is followed by the next statement in the sequence of statements that define the block

Again, these cases are easily specified, making use of the `isLast` and
`next` node properties to handle the sequence cases.

```
val following : Stm ==> Set[Stm] =
    childAttr {
        case s => {
             case t @ While (_, _)           => Set (t)
             case b @ Block (_*) if s isLast => b->following
             case Block (_*)                 => Set (s.next)
             case _                          => Set ()
        }
    }
```

(Note: these attributes are partial functions, indicated by the type constructor
`org.kiama.==>` because they are used in this example to define dynamically updated
attribution. Attributes that do not need to be updated at run-time are functions,
indicated by `=>`, not partial functions.)

## Variable definitions and uses

File: [org.kiama.example.dataflow.Dataflow.scala](https://bitbucket.org/inkytonik/kiama/src/default/library/src/org/kiama/example/dataflow/Dataflow.scala)

The next step is to specify the variables that are defined and used
directly by each kind of statement. We specify two attributes
`defines` and `uses` for this purpose.

Assignment statements define the variable on their left-hand side and
no other kind of statement defines a variable.

```
val defines : Stm ==> Set[Var] =
    attr {
        case Assign (v, _) => Set (v)
        case _             => Set ()
    }
```

A number of statement types use variables. If we had a more complex
expression language the `uses` attribute for statements would probably
be defined by delegating to the `uses` attribute of the expressions,
but in this simple case it is easy to define directly.

```
val uses : Stm ==> Set[Var] =
    attr {
        case If (v, _, _)  => Set (v)
        case While (v, _)  => Set (v)
        case Assign (_, v) => Set (v)
        case Return (v)    => Set (v)
        case _             => Set ()
    }
```

## Variable liveness

File: [org.kiama.example.dataflow.Dataflow.scala](https://bitbucket.org/inkytonik/kiama/src/default/library/src/org/kiama/example/dataflow/Dataflow.scala)

The standard approach to calculating variable liveness is to compute
`in` and `out` sets for each statement. `in (s)` contains all
variables that have values that are defined before `s` and are used
either in `s` or in later statements without first being redefined.
"Later" is interpreted in terms of the control flow from `s`.
Similarly, `out (s)` contains all variables whose values either pass
through `s` or are defined by `s` and are used later without first
being redefined.

This problem is usually phrased mathematically as a pair of mutually
dependent equations.
[Wikipedia](http://en.wikipedia.org/wiki/Live_variable_analysis)
has a version of the equations we use here. Their LIVE-in and LIVE-out
sets are our `in` and `out` attributes.  `GEN` is our `uses` attribute
and `KILL` is our `defines` attribute.

To compute the `in` and `out` sets for a statement we just implement
the dataflow equations directly as circular attributes. Note that the
equations don't say what the initial values of the attributes should
be. We will use empty sets so the final values are built up from
there.

```
val in : Stm => Set[Var] =
    circular (Set[Var]()) {
        case s => uses (s) ++ (out (s) -- defines (s))
    }

val out : Stm => Set[Var] =
    circular (Set[Var]()) {
        case s => (s->succ) flatMap (in)
    }
```

Notice that the attribute definitions very closely resemble the
dataflow equations. Standard Scala set operations suffice, along with
a `flatMap` to compute the union of the `in` sets for the successors
of a statement. We have used the `a (n)` notation for attributes
throughout since it closely matches the notation in the dataflow
equations.

## Tests

File: [org.kiama.example.dataflow.DataflowTests.scala](https://bitbucket.org/inkytonik/kiama/src/default/library/src/org/kiama/example/dataflow/DataflowTests.scala)

A test of live variable computations for a small program involving a
while loop.

```
sbt 'test-only org.kiama.example.dataflow.DataflowTests'
```

Up: [Examples](Examples.md), Next: [Imperative](Imperative.md)

# Overview of Kiama's support for pretty printing

Up: [User Manual](UserManual.md), Prev: [Machines](Machines.md), Next: [Messaging](Messaging.md)

IMPORTANT NOTE: This page describes Kiama 1.x. Much of it also applies
to Kiama 2.x, but not all. Please consult the 2.x release notes for the
main differences. We are currently writing comprehensive documentation
for 2.x that will eventually replace these pages.

This page provides an overview of Kiama's support for _pretty
printing_. Pretty printing is used in the following examples.

  * [Imperative](Imperative.md)
  * ISWIM
  * [Lambda2](Lambda2.md)
  * [Oberon0](Oberon0.md)

Fragments from these examples are used as illustrations below.

More information about Kiama pretty-printing can be found via the
[Research](Research.md) page.

Kiama's pretty printing library is based on the paper
["Linear, bounded, functional pretty-printing"](http://journals.cambridge.org/abstract_S0956796808006990)
by Swierstra and Chitil (Journal of Functional Programming, 19 (1),
2008) which details various pretty printing libraries for Haskell. We
use the continuation-based form of pretty printer discussed in this
paper.

Swierstra and Chitil's work is descended from earlier libraries by
Hughes and Wadler. We have also incorporated combinators from the
Haskell
[PPrint](http://research.microsoft.com/en-us/um/people/daan/pprint.html)
library by Leijen.

## Introduction

Pretty printing is the task of producing a pleasing output
representation from some data structure. In the Kiama context, we
usually mean the task of turning an abstract syntax tree representing
some software artefact into the textual syntax for that artefact. For
example, a code generator might create an abstract syntax tree that
represents the code to be generated. Pretty printing would turn that
tree into the generated source code.

The simplest form of this process is to simply traverse the tree in
some fashion, printing the textual equivalent of nodes as they are
encountered and inserting punctuation and other special characters
where needed. This process is usually called _unparsing_ since it
performs the opposite translation to that performed by a parser.

However, we usually want more control than an unparser can provide.
For example, we might want to ensure that statement blocks are
indented to a level deeper than the statement in which they occur. Or
we might want to ensure that no line of the output is longer than a
certain length. Obeying these kinds of constraints are what
distinguishes a pretty printer from an unparser.

## A document model for pretty printing

It is quite difficult to perform pretty printing during a single
simple traversal of a tree. Consequently, pretty printing libraries
often adopt a _document model_ view. The idea is that we construct a
value that represents the document to be created as the result of the
pretty printing. The document embodies the constant text that must be
present, optional parts, and the constraints if any on the final
output.

Kiama's pretty printing library is in the `org.bitbucket.inkytonik.kiama.output.PrettyPrinter`
trait. You must mix this trait into whichever class or object needs to perform
pretty printing. For example, if you have

```
object P extends org.bitbucket.inkytonik.kiama.output.PrettyPrinter
```

then within `P` or via `P`'s public methods, you can access the pretty
printing functionality.

Documents are represented by the type `PrettyPrinter.Doc`. Pretty printing
amounts to constructing a `Doc` value then converting it to a string using
the `pretty` method.

```
def pretty (d : Doc, w : Width = defaultWidth) : Layout
```

(`Layout` is a type alias for `String`.) `pretty` takes the given
document and a maximum output width measured in characters. It returns
the pretty printed form of the document as a string, which can then be
output or sent elsewhere as needed.

`defaultWidth` is 75, but can be overridden when the `PrettyPrinter`
trait is mixed in.

## Basic documents

The pretty printing library provides ways to make basic documents and
ways to combine documents into more complex documents.

The simplest document is an empty one, created by the `empty`
combinator:

```
def empty : Doc
```

`text` turns a string into a document. The string should not contain
newlines if they are intended to be significant to the pretty printer.

```
implicit def text (t : String) : Doc
```

`text` is implicit so you can use string literals as documents in most
cases, and we will do so in the following.

Documents can be combined in a simple way using the `<>` method of
`Doc` which creates a document whose content is the concatenation of
its receiver and the argument.

```
class Doc {
   ...
   def <> (e : Doc) : Doc
   ...
}
```

(Actually, methods including `<>` are defined in a `DocOps` trait
that is mixed in to a specific implementation of `Doc`, but the
difference does not matter to a client of the library.)

Thus, the document

```
"Hello" <> empty <> " world!"
```

creates a document that will be pretty printed as "`Hello world!`".

## Grouping

A fundamental operation for this kind of pretty printing library is
_grouping_. The idea is that we group a document when it has multiple
ways of being printed and we want the pretty printer to consider these
different ways to pick the best.

Use the `group` combinator to indicate a group.

```
def group (d : Doc) : Doc
```

The constraint that we want to satisfy in order to pick the best
output is to make sure the pretty printed version fits in the output
width. A pretty print result that fits in the output width is
preferred over one that doesn't.

As a result of this constraint, a group will be formatted either
horizontally (if it fits on the current line) or vertically (if it
doesn't fit). Groups occurring within a horizontal group will also be
formatted horizontally, but groups occurring within a vertical group
are not constrained in this way.

## Line breaks

The difference between horizontal formatting and vertical formatting
is expressed by _line breaks_, expressed in their simplest form by
the `line` combinator.

```
def line : Doc
```

The idea is that we indicate where the potential line breaks go by
putting `line` documents there. If a line break occurs within a group,
it will be replaced by a space if horizontal formatting is being used
for that group, or by a newline if vertical formatting is being used.
A line break occurring outside any group always results in a newline.

For example, the document

```
"Hello" <> line <> "world!"
```

will result in

```
Hello
world!
```

if it is not within a group.

In contrast, the document

```
group ("Hello" <> line <> "world!")
```

will be printed as "`Hello world!`" if that text would fit on the
current line, or as

```
Hello
world!
```

if there is insufficient room.

`linebreak` is the same as `line` except that it is replaced by
nothing if horizontal formatting is being used for the group in which
it appears.

```
def linebreak : Doc
```

`line` and `linebreak` are just special cases of`line (repl)` where
`repl` is an arbitrary string replacement.
For example, `line ("; ")` will be replaced by a semicolon followed by
a space, if the newline is omitted.

```
def linebreak : Doc
```

## Indentation

It is common to want to indent pretty printed output. By default, no
indentation is inserted, so documents must be created to handle the
indentation explicitly. The `nest` combinator makes it easy to deal
with indentation in a general way.

```
def nest (d : Doc, j : Indent = defaultIndent) : Doc
```

The result of `nest (d, j)` is a document that has the same content as
`d`, except that it is indented `j` spaces further than the current
indentation level. The indentation level is zero when pretty printing
of a document begins.

`defaultIndent` is four, but can be overridden when the
`PrettyPrinter` trait is mixed in.

Indentation interacts closely with the presence or absence of line
breaks, since indentation is only inserted after a line break. For
example, the document

```
"Hello" <> nest (line <> "world!")
```

will produce "`Hello world!`" if the line break is omitted, but

```
Hello
    world!
```

if it is not omitted.  Note that the `line` has to be inside the
`nest` for the indentation that follows it to be affected.

## Other combinators

The combinators discussed so far form the basic primitives of all
documents. The library provides an extensive collection of other
combinators as short-hand notations for common situations.

This page summarises the kinds of other combinators and lists some
instances of each kind.  See the
[PrettyPrinter trait](http://code.google.com/p/kiama/source/browse/src/main/scala/org/bitbucket/inkytonik/kiama/output/PrettyPrinter.scala)
for a complete list.

First up is `string` which is like `text` except that the string is
allowed to contain newlines. Each newline is turned into an invocation
of `line`.

```
def string (s : String) : Doc
```

`char` is the same as `string` but just for a single character.

```
implicit def char (c : Char) : Doc
```

`char` is implicit so you can use string literals as documents in most
cases.

Single characters have combinators so that they
can be made easier to read. These include `comma` and `dot`, plus bracketing
characters such as `lparen` and `rparen` (but also see the `surround` and
`enclose` combinators described in the next section).

```
def comma : Doc
def dot : Doc
def lparen : Doc
def rparen : Doc
```

Explicit spaces can be produced using the `space` and `spaces`
combinators. The latter produces exactly the number of spaces given by
its integer argument, or an empty document if the argument is not
positive.

```
def space : Doc
def spaces (n : Int) : Doc
```

The `softline` and `softbreak` combinators produce optional line
breaks that are individually grouped.

```
def softline : Doc
def softbreak : Doc
```

## More forms of concatenation

In addition to the basic concatenation method `<>`, there are
varieties called `<+>`, `<@>`, `<@@>, ``</>`, and `<\>` that include a
`space`, `line`, `linebreak`, `softline` and `softbreak`,
respectively, between the concatenated documents.

```
class Doc {
   ...
   def <+>  (e : Doc) : Doc
   def <@>  (e : Doc) : Doc
   def <@@> (e : Doc) : Doc
   def </>  (e : Doc) : Doc
   def <\>  (e : Doc) : Doc
   ...
}
```

The `surround` combinator surrounds a document `d` with two
occurrences of another document `b`, using `<>` for concatenation.

```
def surround (d : Doc, b : Doc) : Doc
```

Combinators such as `dquotes` (for double quotes) and `squotes`
(for single quotes) are special cases of `surround`.

`enclose` is a form of `surround` where the document on the left might
be different to the one on the right.

```
def enclose (l : Doc, d : Doc, r : Doc) : Doc
```

`braces` and `parens` are commonly used to enclose a document in
braces or parentheses, respectively.

```
def braces (d : Doc) : Doc
def parens (d : Doc) : Doc
```

## Pretty printing arbitrary values and sequences

`value` turns an arbitrary value into a document using its `toString`
method and `string`.

```
def value (v : Any) : Doc
```

Sequences (instances of `Seq`) can be pretty printed using a collection
of combinators of which `hsep` and `vsep` are typical.
`hsep` concatenates the items in the sequence using `<+>`, while
`vsep` uses `<@>`.

```
def hsep (ds : Seq[Doc]) : Doc
def vsep (ds : Seq[Doc]) : Doc
```

There are many other variants, such as ones that allow the separator
to be specified, and `lterm` that prints lists where the elements
are individually terminated rather than separated.

## The `PrettyPrintable` trait

The library comes with a variant of `pretty` that accepts an instance
of the `PrettyPrintable` trait instead of a document. This variant can
help to simplify some pretty printers.

```
def pretty (p : PrettyPrintable) : Layout

trait PrettyPrintable {
   def toDoc : Doc
}
```

This version of `pretty` calls the object's `toDoc` method to obtain
the document to be printed. By default, `toDoc` just calls the `value`
combinator on the object. There is also an implicit conversion from
`Any` to `PrettyPrintable` that uses `value` to implement `toDoc`.

The idea is that `PrettyPrintable` is mixed in to domain classes that
implement the `toDoc` method. Values of those domain classes can then
be pretty printed directly without having to explicitly call a method
to convert them to a document. The implicit conversion allows any
value to be pretty printed very easily.

## Lists

The `list` combinator pretty prints a `List` in Scala notation,
inserting grouping, nesting and line breaks as needed. For example,
the document

```
list (List (1, 2, 3))
```

results in "`List(1, 2, 3)`" if it fits on the current line, or

```
List(
    1,
    2,
    3)
```

if it doesn't.

`list` can be customised in many ways.  Here is the full signature:

```
def list[T] (l : List[T], prefix : String = "List",
             elemToDoc : T => Doc = (x : T) => value (x),
             sep : Doc = comma,
             sepfn : (Seq[Doc], Doc) => Doc = lsep) : Doc =
```

`prefix` is the string that is printed at the beginning of the list
(default: "List").  `elemToDoc` provides a way to convert the list
elements to documents (default: using the `value` method).  `sep`
is used to separate each element (default: a comma).  Finally,
`sepfn` is used to format the elements with the separator (default:
`lsep`).

For example, the alternative layout for our example list

```
List(1
    , 2
    , 3
    )
```

can be achieved using

```
list (List (1, 2, 3), sepfn = lsep2)
```

if the available line width is not sufficient.

See the [API documentation](http://code.google.com/p/kiama/wiki/Releases)
for your Kiama release for detailed descriptions of `lsep` and
`lsep2`.

## Pretty-printing any value

The `any` combinator provides a convenient way to pretty print any
value, most commonly instances of case classes, but also common
data structures such as tuples, vectors, and maps.

For example, given the classes

```
abstract class Exp
case class Num (i : Int) extends Exp
case class Add (l : Exp, r : Exp) extends Exp
```

the document

```
any (Add (Num (1), Num (2)))
```

might be pretty-printed as

```
Add (Num (1), Num (2))
```

or

```
Add (
    Num (
        1),
    Num (
        2))
```

depending on line width.

## Functional pretty printing: the Imperative language

As a more complex example, consider
[pretty printing](https://bitbucket.org/inkytonik/kiama/src/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/imperative/PrettyPrinter.scala)
for the [Imperative](Imperative.md) language. We implement a function `show` that
converts any imperative language abstract syntax tree node into a
document. `show` uses `showbin` that abstracts documents for
parenthesised applications of binary operators.  This is an
example of a functional-style pretty printer, where the pretty
printing logic is kept outside of the data that is to be printed.
(See the next example for a more object-oriented style.)

```
object PrettyPrinter extends org.bitbucket.inkytonik.kiama.output.PrettyPrinter {

    import AST._

    def pretty (t : ImperativeNode) : String =
        super.pretty (show (t))

    def show (t : ImperativeNode) : Doc =
        t match {
            case Num (d)      => value (d)
            case Var (s)      => s
            case Neg (e)      => parens ("-" <> show (e))
            case Add (l, r)   => showbin (l, "+", r)
            case Sub (l, r)   => showbin (l, "-", r)
            case Mul (l, r)   => showbin (l, "*", r)
            case Div (l, r)   => showbin (l, "/", r)
            case Null ()      => semi
            case Seqn (ss)    => braces (nest (line <> ssep (ss map show, line)) <> line)
            case Asgn (v, e)  => show (v) <+> "=" <+> show (e) <> semi
            case While (e, b) => "while" <+> parens (show (e)) <> nest (line <> show (b))
        }

    def showbin (l : ImperativeNode, op : String, r : ImperativeNode) : Doc =
        parens (show (l) <+> op <+> show (r))

}
```

The program

```
{ i = 10; count = 0; while (i) { count = count + 1; i = 1 + i; } }
```

is represented by the abstract syntax tree

```
Seqn (
    List (
        Asgn (Var ("i"), Num (10.0)),
        Asgn (Var ("count"), Num (0.0)),
        While (
            Var ("i"),
            Seqn (
                List (
                    Asgn (
                        Var ("count"),
                        Add (Var ("count"), Num (1.0))),
                    Asgn (Var ("i"), Add (Num (1.0), Var ("i"))))))))
```

This abstract syntax tree text was produced using `show` and
`product`. Notice how the second last `Asgn` node has been broken over
two lines (breaking at the comma) because it would not fit in the
default output width, whereas the last one has not been broken, since
it fits. The other nodes in the while loop above the assignments have
been broken in a similar fashion.

Omitting `product`, we get

```
{
    i = 10.0;
    count = 0.0;
    while (i)
        {
            count = (count + 1.0);
            i = (1.0 + i);
        }
}
```

## Object-oriented pretty printing: special pairs

A more object-oriented approach to pretty printing is to add the
conversion logic to the data classes. In this case we make the pretty
printable data implement the `PrettyPrintable` class of the pretty
printer and override the `toDoc` method to provide custom conversion.

First, we define a pretty printer and import its members into the
current scope.

```
object MyPrettyPrinter extends org.bitbucket.inkytonik.kiama.output.PrettyPrinter

import MyPrettyPrinter._
```

Now consider two classes that implement a special kind of pair whose
components are a special kind of integer.

```
class MyPair (val l : MyInt, val r : MyInt) extends PrettyPrintable {
    override def toDoc : Doc =
        brackets (l.toDoc <> ":" <> r.toDoc)
}

class MyInt (val i : Int) extends PrettyPrintable {
    override def toDoc : Doc =
        value (i) <> "!"
}
```

The `MyPair` and `MyInt` classes are `PrettyPrintable` and their
`toDoc` methods provide the desired custom conversion for those
types.

The program can create a `MyPair` and pretty print it as follows.

```
val l = new MyPair (new MyInt (1), new MyInt (2))
println (pretty (l))
```

The result is as follows.

```
[1!:2!]
```

## Parenthesisation

Expression-oriented languages use parentheses to control the application
of operators. We can print such programs with parentheses around every
expression and be sure that the printed program will respect the
operator precedence and associativity that was intended.

Alternatively, we can just put in parentheses where they are needed.
For example, if we are printing `1 + 2 + 3` then parentheses are not
needed at all, but in `(1 + 2) * 3` we need the parentheses around
`1 + 2` but not another set around the whole expression.

Kiama includes the `org.bitbucket.inkytonik.kiama.output.ParenPrettyPrinter` trait that
makes it easy to define pretty-printers that perform minimal
parenthesisation. The module uses algorithms published by Norman Ramsey
in his paper
["Unparsing expressions with prefix and postfix operators"](http://onlinelibrary.wiley.com/doi/10.1002/(SICI)1097-024X(1998100)28:12%3C1327::AID-SPE195%3E3.0.CO;2-C/abstract)
(Software: Practice and Experience, 28 (12), 1998).
See the [Oberon0](Oberon0.md) example for an illustration.

## Output filters

The module `org.bitbucket.inkytonik.kiama.output.Filters` provides functions that may be
usefully applied after pretty-printing.

Sometimes the pretty-printed output is designed to go in fixed space.
The filters `keepMaxChars` and `keepMaxLines` can be used to discard
excess characters or lines. E.g., `keepMaxLines (5) (s)` will return
the first five lines of `s` or the whole of `s` if `s` contains fewer
than five lines.

Similarly, `maxWords` filters to a maximum word limit.

Finally, `keepMaxIndent (n, s)` filters `s` by replacing all runs of
consecutive lines that have indentation at least `n` spaces with `"..."`
indented by `n` spaces. Thus, `keepMaxIndent` can be used to summarise `s` by
omitting information nested below a certain level. An optional extra argument
can be used to specify alternatives to `"..."`.

Up: [User Manual](UserManual.md), Prev: [Machines](Machines.md), Next: [Messaging](Messaging.md)

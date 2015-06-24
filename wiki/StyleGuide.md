# Style guide for Kiama developers

To keep the Kiama code fairly uniform, we try to use a similar style
throughout. See the code base for examples.

The following points summarise the main guidelines.

  * Each file should begin with a Kiama copyright comment. See an existing file for an example. Update the copyright date as appropriate. Also, if the code in the file is based on code from somewhere else, include an extra comment after the Kiama comment, explaining this and give a reference. E.g., see the PicoJava example.

  * Use spaces instead of tab characters for indentation. Most editors can be configured to do this automatically.

  * Use four spaces per indentation level.

  * Limit very long lines but don't necessarily restrict to 80 characters if readability is enhanced by a longer line.

  * Put opening braces for non-trivial blocks on the end of the line before the block really starts, not on a separate line.

  * Parenthesise calls to methods and functions like this: `func (1, 2, 3)` instead of this: `func(1,2,3)` or this: `func( 1, 2, 3 )`. I.e. use a single space after the function name and before each argument except the first. Similarly for patterns.

  * `def` constructs with no arguments should be declared with a single empty argument list, as in `def foo () = ...` if they have side effects. This usage ensures that a caller can provide the argument list (`foo ()`) or not (just `foo`), as they prefer. If the argument list is omitted, only the second is legal.

  * Separate the headings of named entities such as vals or defs from their definitions on a separate line unless they are very short.

  * Write type annotations like this: `foo : Int`, rather than this: `foo: Int` or `foo:Int`. I.e., use a space before and after the colon.

  * Write short explanatory ScalaDoc-ish comments to describe methods, functions etc so that a user can consult the API documentation and not have to read the code. To keep things simple, we usually don't use full ScalaDoc  defining parameters etc. Instead we just incorporate a description of the parameter meanings and return values into the method or function documentation. We use the back-quote notation to format things in `code style` within ScalaDoc comments.

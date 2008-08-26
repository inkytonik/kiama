package kiama.parsing

/**
 * Implementation of packrat parsers for parsing expression grammars with
 * extensions.  The general structure of this library is modelled after
 * the Scala Library parser combinator library as much as possible to
 * reduce the learning curve and conversion costs.
 */
trait Packrat {
    
    import scala.util.parsing.input.Reader
    
    /**
     * The type of input element that these parsers process.  Just Char
     * for now, but ultimately will extend to any type, probably.
     */
    type Elem = Char

    /**
     * The input for these parsers comes from a reader of the element 
     * type.  We use the same readers as the standard parser combinator
     * library.
     */
    type Input = Reader[Elem]

    /**
     * Parse results.
     */
    sealed abstract class ParseResult[+T] {
        def map[U] (f : T => U) : ParseResult[U]
        def flatMapWithNext[U] (f : T => Input => ParseResult[U]) : ParseResult[U]  
        def append[U >: T] (a : => ParseResult[U]) : ParseResult[U]
    }
    
    /**
     * A successful parse result.
     *
     * @param result a value representing the result of the parse
     * @param in the remainder of the input
     */
    case class Success[T] (result : T, in : Input) extends ParseResult[T] {
        
        def map[U] (f : T => U) =
            Success (f (result), in)

        def flatMapWithNext[U] (f : T => Input => ParseResult[U]) : ParseResult[U] =
            f (result) (in) 
            
        def append[U >: T] (a : => ParseResult[U]) : ParseResult[U] =
            this
            
        override def toString = 
            "[" + in.pos + "] parsed: " + result

    }
     
    /**
     * A parse result representing failure of a parse.
     *
     * @param msg a message describing the failure
     * @param in the remainder of the input
     */
    case class Failure (msg : String, in : Input) extends ParseResult[Nothing] {
        
        def map[U] (f : Nothing => U) =
            this

        def flatMapWithNext[U] (f : Nothing => Input => ParseResult[U]) : ParseResult[U] =
            this
            
        def append[U >: Nothing] (a : => ParseResult[U]) : ParseResult[U] =
            // FIXME: is this right?  The Scala lib version does some stuff with
            // the position to return the "furthest" position, but we just want
            // to do whatever a does, right?
            a
            
        override def toString =
            "[" + in.pos + "] failure: " + msg + "\n\n" + in.pos.longString
            
    }

    /**
     * A special kind of tuple for compound parser result values.
     */
    case class ~[+U,+V] (l : U, r : V)

    /**
     * A parser implemented as a function from input to a parse result.
     */
    abstract class Parser[+T] extends (Input => ParseResult[T]) {
        
        /**
         * Alias this as p to make it easier to refer to in the combinator
         * definitions below.
         */
        p => 
        
        /** 
         * Run this parser.
         *
         * @param in the input on which the parser should run
         * @return the result of the parse
         */
        def apply (in : Input) : ParseResult[T]
        
        def map[U] (f : T => U) : Parser[U] =
            Parser { in =>
                p (in) map (f)
            }
        
        def flatMap[U] (f : T => Parser[U]) : Parser[U] =
            Parser { in =>
                p (in) flatMapWithNext (f)
            }
        
        def append[U >: T] (q : => Parser[U]) : Parser[U] =
            Parser { in =>
                p (in) append q (in)
            }
        
        /**
         * Construct a parser that parses zero or more occurrences of
         * what this parser parses.
         */
            
        def ~[U] (q : => Parser[U]) : Parser[~[T, U]] =
            for (v <- p; w <- q) yield new ~ (v,w)
        
        def ~>[U] (q : => Parser[U]) : Parser[U] =
            for (v <- p; w <- q) yield w
            
        def <~[U] (q : => Parser[U]) : Parser[T] =
            for (v <- p; w <- q) yield v
            
        def * : Parser[List[T]] =
            (p+) | success (List ())

        def + : Parser[List[T]] = {
            def q : Parser[List[T]] =
                (p ~ q) ^^ { case t ~ ts => t :: ts } |
                p ^^ (t => List (t))
            q
        }
            
        def |[U >: T] (q : => Parser[U]) : Parser[U] =
            append (q)

        def ^^[U] (f : T => U) : Parser[U] =
            map (f)
 
    }
    
    /**
     * A parser that memoises the results returned by a given parser.  Assumes
     * that the underlying readers share a common source.
     */
    class MemoParser[T] (p : Parser[T]) extends Parser[T] {
        
        import scala.util.parsing.input.Position
        import scala.collection.mutable.HashMap
    
        /**
         * The memo table.
         */
        var memo = new HashMap[Position,ParseResult[T]]

        /** 
         * Run this parser, memoising its results.
         *
         * @param in the input on which the parser should run
         * @return the result of the parse
         */
        def apply (in : Input) : ParseResult[T] = {
            val pos = in.pos
            if (memo.contains (pos)) {
                return memo (pos)
            } else {
                val res = p.apply (in)
                memo += (pos -> res)
                return res
            }
        }

    }

    /**
     * Construct a parser from a function on input.
     */
    def Parser[T] (f : Input => ParseResult[T]) : Parser[T] =
       new Parser[T] { def apply (in : Input) = f (in) }
    
    /**
     * Construct a parser that always succeeds without consuming any input.
     * 
     * @param result the value to succeed with
     * @return the constructed parser
     */
    def success[T] (result : T) : Parser[T] =
        Parser { in => Success (result, in) }
    
    /**
     * Construct a parser that always fails without consuming any input.
     * 
     * @param msg the message to fail with
     * @return the constructed parser
     */
    def failure (msg : String) : Parser[Nothing] =
        Parser { in => Failure (msg, in) }
        
    /**
     * Construct a parser that recognises a given input element.
     *
     * @param e the input element to recognise
     * @return the constructed parser
     */
    implicit def accept (e : Elem) : Parser[Elem] =
        Parser { in =>
            if (e == in.first)
                Success (in.first, in.rest)
            else
                Failure (e.toString, in)
        }
        
    /**
     * Construct a parser that accepts an element for which a
     * predicate evaluates to true.
     */
    implicit def acceptIf (pred : Elem => Boolean) : Parser[Elem] =
        Parser { in =>
            if (pred (in.first))
                Success (in.first, in.rest)
            else
                Failure ("acceptIf", in)
        }
        
    /**
     * Construct a memoising parser based on a given parser.
     */
    def memo[T] (p : Parser[T]) : Parser[T] =
        new MemoParser (p)
        
    // Character reader-specific ones are below here
    
    /**
     * Construct a parser that accepts a given string.
     */
    implicit def accceptString (s : String) : Parser[String] =
        token (Parser { in =>
            val source = in.source
            val offset = in.offset
            var i = 0
            var j = offset
            while (i < s.length && j < source.length && s.charAt (i) == source.charAt (j)) {
                i += 1
                j += 1
            }
            if (i == s.length)
                Success (source.subSequence (offset, j).toString, in.drop (j - offset))
            else 
                Failure ("'" + s + "' expected", in)
        })

    def token[T] (p : Parser[T]) : Parser[T] =
        (whitespace*) ~> p
        
    def whitespace : Parser[Char] =
        (ch : Char) => ch.isWhitespace

    def digit : Parser[Char] =
        (ch : Char) => ch.isDigit

    def letter : Parser[Char] =
        (ch : Char) => ch.isLetter
        
    def letterOrDigit : Parser[Char] =
        (ch : Char) => ch.isLetterOrDigit

}


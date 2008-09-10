package kiama.parsing

/**
 * Implementation of packrat parsers for parsing expression grammars with
 * extensions.  The general structure of this library is modelled after
 * the Scala Library parser combinator library as much as possible to
 * reduce the learning curve and conversion costs.
 */
trait Parsers {
    
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
     * Parse results represented as an answer and the point to which the
     * input was processed.
     */
    case class ParseResult[+T] (ans : ParseAnswer[T], in : Input) {
        def map[U] (f : T => U) : ParseResult[U] =
            ParseResult (ans map f, in)
        def flatMapWithNext[U] (f : T => Input => ParseResult[U]) : ParseResult[U] =
            (ans flatMapWithNext (f, (b : ParseAnswer[U]) => in => ParseResult (b, in))) (in)
        def append[U >: T] (a : => ParseResult[U]) : ParseResult[U] =
            ans append (b => ParseResult (b, in), a)      
        override def toString = "[" + in.pos + "] " + ans
    }
    
    /**
     * Parsing answers.
     */
    abstract class ParseAnswer[+T] {
        def map[U] (f : T => U) : ParseAnswer[U]
        def flatMapWithNext[U] (f : T => Input => ParseResult[U], g : ParseAnswer[U] => Input => ParseResult[U]) : Input => ParseResult[U]
        def append[U >: T] (f : ParseAnswer[T] => ParseResult[U], a : => ParseResult[U]) : ParseResult[U]
    }
    
    /**
     * A successful parse answer.
     *
     * @param result a value representing the result of the parse
     */
    case class Success[T] (result : T) extends ParseAnswer[T] {
        def map[U] (f : T => U) =
            Success (f (result))
        def flatMapWithNext[U] (f : T => Input => ParseResult[U], g : ParseAnswer[U] => Input => ParseResult[U]) : Input => ParseResult[U] =
            f (result)
        def append[U >: T] (f : ParseAnswer[T] => ParseResult[U], a : => ParseResult[U]) : ParseResult[U] =
            f (this)
        override def toString = "parsed: " + result
    }
    
    /**
     * Convenience factory method for success results.
     */
    def success[T] (result : T, in : Input) = ParseResult (Success (result), in)
     
    /**
     * A parse answer representing failure of a parse.
     *
     * @param msg a message describing the failure
     */
    case class Failure (msg : String) extends ParseAnswer[Nothing] {
        def map[U] (f : Nothing => U) : ParseAnswer[U] =
            this
        def flatMapWithNext[U] (f : Nothing => Input => ParseResult[U], g : ParseAnswer[U] => Input => ParseResult[U]) : Input => ParseResult[U] =
            g (this)
        def append[U >: Nothing] (f : ParseAnswer[Nothing] => ParseResult[U], a : => ParseResult[U]) : ParseResult[U] =
            a
        override def toString = "failure: " + msg
    }    
        
    /**
     * Convenience factory method for failure results.
     */
    def failure[T] (msg : String, in : Input) = ParseResult (Failure (msg), in)

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
        Parser { in => success (result, in) }
    
    /**
     * Construct a parser that always fails without consuming any input.
     * 
     * @param msg the message to fail with
     * @return the constructed parser
     */
    def failure (msg : String) : Parser[Nothing] =
        Parser { in => failure (msg, in) }
        
    /**
     * Construct a parser that recognises a given input element.
     *
     * @param e the input element to recognise
     * @return the constructed parser
     */
    implicit def accept (e : Elem) : Parser[Elem] =
        Parser { in =>
            if (e == in.first)
                success (in.first, in.rest)
            else
                failure (e.toString, in)
        }
        
    /**
     * Construct a parser that accepts an element for which a
     * predicate evaluates to true.
     */
    implicit def acceptIf (pred : Elem => Boolean) : Parser[Elem] =
        Parser { in =>
            if (pred (in.first))
                success (in.first, in.rest)
            else
                failure ("acceptIf", in)
        }
                
    // Character reader-specific ones are below here
    
    /**
     * Construct a parser that accepts a given string.  Implicitly invoked
     * on strings where parsers are expected.
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
                success (source.subSequence (offset, j).toString, in.drop (j - offset))
            else 
                failure ("'" + s + "' expected", in)
        })

    def token[T] (p : Parser[T]) : Parser[T] =
        (whitespace*) ~> p
        
    val whitespace : Parser[Char] =
        (ch : Char) => ch.isWhitespace

    val digit : Parser[Char] =
        (ch : Char) => ch.isDigit

    val letter : Parser[Char] =
        (ch : Char) => ch.isLetter
        
    val letterOrDigit : Parser[Char] =
        (ch : Char) => ch.isLetterOrDigit
    
}

trait Packrat extends Parsers {
          
    /**
     * A rule is a memoising, left recursion-detecting encapsulation of a parser.
     */
    class Rule[T] (body : => Parser[T]) extends Parser[T] {

        import scala.collection.mutable.HashMap
      
        /*
         * The section of the memo table relating to this rule.
         */
        private val table = new HashMap[Input,ParseResult[T]]

        /**
         * Apply this rule, memoising the result.
         */
        def apply (in : Input) : ParseResult[T] = {
            if (table contains in)
                table (in)
            else {
                table += (in -> ParseResult (Failure ("left recursion"), in))
                val m = body (in)
                table += (in -> m)
                m
            }
        }
                      
    }
    
    /**
     * Convenience function for turning a parser into a memoising one.
     */
    def memo[T] (parser : => Parser[T]) : Rule[T] = new Rule (parser)

}


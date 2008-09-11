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
     * Parse results.
     */
    sealed abstract class ParseResult[+T] {
        val in : Input
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
        def map[U] (f : T => U) = Success (f (result), in)
        def flatMapWithNext[U] (f : T => Input => ParseResult[U]) : ParseResult[U] =
            f (result) (in) 
        def append[U >: T] (a : => ParseResult[U]) : ParseResult[U] =
            this
        override def toString = "[" + in.pos + "] parsed: " + result
    }
     
    /**
     * A parse result representing failure of a parse.
     *
     * @param msg a message describing the failure
     * @param in the remainder of the input
     */
    case class Failure (msg : String, in : Input) extends ParseResult[Nothing] {
        def map[U] (f : Nothing => U) = this
        def flatMapWithNext[U] (f : Nothing => Input => ParseResult[U]) : ParseResult[U] =
            this
        def append[U >: Nothing] (a : => ParseResult[U]) : ParseResult[U] =
            a
        override def toString = "[" + in.pos + "] failure: " + msg + "\n\n" + in.pos.longString
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
                Success (source.subSequence (offset, j).toString, in.drop (j - offset))
            else 
                Failure ("'" + s + "' expected", in)
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

trait PackratParsers extends Parsers {
  
    import scala.collection.mutable.HashMap
    import scala.collection.mutable.Set
    import scala.util.parsing.input.Position
  
    /**
     * Information about a left recursion. 
     */
    case class Head (rule : Rule, var involvedSet : Set[Rule], var evalSet : Set[Rule])
    
    /**
     * Heads map
     */
    var heads = new HashMap[Input,Head]  
    
    /**
     * Parsing answers.
     */
    abstract class Answer[T]
    
    /**
     * An answer that is a parser result.
     */
    case class Result[T] (result : ParseResult[T]) extends Answer[T]
    
    /**
     * An answer that is a left recursion record.
     */
    case class LR[T] (var seed : ParseResult[T], rule : Rule, var head : Head, next : LR[_]) extends Answer[T]
  
    /**
     * Left recursion stack.
     */
    var LRStack : LR[_] = null
    
    /**
     * Common supertype for all rules.
     */
    trait Rule
    
    /**
     * A rule is a memoising, left recursion-detecting encapsulation of a parser.
     */
    class TypedRule[T] (body : => Parser[T]) extends Parser[T] with Rule {
      
	    /**
	     * Memo table entries.
	     */
        case class MemoEntry (var ans : Answer[T], var in : Input)
        
        /**
         * The section of the memo table relating to this rule.
         */
        val memo = new HashMap[Input,MemoEntry]
                        
        /**
         * Apply this rule, memoising the result.
         */
        def apply (in : Input) : ParseResult[T] = {
            recall (in) match {
                case None =>
                    val lr = LR[T] (Failure ("left recursion", in), this, null, LRStack)
                    LRStack = lr
                    val m = MemoEntry (lr, in)
                    memo += (in -> m)
                    val ans = body (in)
                    LRStack = LRStack.next
                    m.in = ans.in
                    if (lr.head == null) {
                        m.ans = Result (ans)
                        ans
                    } else {
                        lr.seed = ans
                        lranswer (in, m)
                    }
                case Some (MemoEntry (Result (r), _)) =>
                    r
                case Some (MemoEntry (lr @ LR (_, _, _, _), _)) =>                    
                    setuplr (lr)
                    lr.seed
            }                                
        }
        
        /**
         *
         */
        def setuplr (l : LR[T]) {
            if (l.head == null)
                l.head = Head (this, Set (), Set ())
            var s = LRStack
            while (s.head != l.head) {
                s.head= l.head
                l.head.involvedSet = l.head.involvedSet + s.rule
                s = s.next
            }
        }
        
        /**
         *
         */
        def lranswer (in : Input, m : MemoEntry) : ParseResult[T] = {
            m.ans match {
                case lr @ LR (_, _, _, _) =>
                    val h = lr.head
                    if (h.rule == this) {
                        m.ans = Result (lr.seed)
                        m.ans match {
                            case Result (f @ Failure (_, _)) =>
                                f
                            case _ =>
                                 growlr (in, m, h)
                        }
                    } else
                        lr.seed                      
                case _ =>
                    error ("lranswer: unexpected non-LR answer")
            }
        }
        
        /**
         *
         */
        def recall (in : Input) : Option[MemoEntry] = {
            val om = memo.get (in) 
            heads.get (in) match {
                case None     => om
                case Some (h) =>
                    if ((om == None) && !((h.involvedSet + h.rule) contains this))
                        return Some (MemoEntry (Result (Failure ("left recursion skip", in)), in))
                    if (h.evalSet contains this) {
                        h.evalSet = h.evalSet - this
                        val ans = body (in)
                        memo += (in -> MemoEntry (Result (ans), ans.in))
                        memo.get (in)
                    } else
                        om
            }
        }
        
        /**
         *
         */
        def growlr (in : Input, m : MemoEntry, h : Head) : ParseResult[T] = {
          
            def nolater (p1 : Position, p2 : Position) : Boolean =
                (p1 == p2) || (p1 < p2)
          
            heads += (in -> h)
            while (true) {
                h.evalSet = h.involvedSet.clone
                val ans = body (in)
                if (ans.isInstanceOf[Failure] || nolater (ans.in.pos, m.in.pos)) {
                    heads -= in
                    m.ans match {
                        case Result (r) =>
                            return r
                        case _ =>
                            error ("growlr: unexpected non-result answer")
                    }
                }
                m.ans = Result (ans)
                m.in = ans.in
            }
            error ("growlr: went where I shouldn't go")
        }

    }
    
    /**
     * Convenience function for turning a parser into a memoising one.
     */
    def memo[T] (parser : => Parser[T]) : TypedRule[T] = new TypedRule[T] (parser)

}

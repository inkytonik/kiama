/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008 Anthony M Sloane, Macquarie University.
 *
 * Kiama is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * Kiama is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with Kiama.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */                         
                                
package kiama.parsing

/**
 * Parser combinator library modelled on the Scala parser combinator
 * library.
 */
trait Parsers {
    
    import scala.util.parsing.input.Reader
    
    /**
     * The abstract type of input element that these parsers process.
     */
    type Elem

    /**
     * The input for these parsers comes from a reader of the element 
     * type.  We use the same readers as the standard parser combinator
     * library.
     */
    type Input = Reader[Elem]

    /**
     * Representations of the results of parsing.
     */
    sealed abstract class ParseResult[+T] {
      
        /**
         * The input that remains to be consumed.
         */
        val in : Input
        
        /**
         * If this result reflects a successful parse, apply f to the value
         * produced and return the result of that application.
         */
        def map[U] (f : T => U) : ParseResult[U]
        
        /**
         * If this result reflects a successful parse, feed the resulting
         * value and the remainder of the input to f to obtain a final result.
         */
        def flatMapWithNext[U] (f : T => Input => ParseResult[U]) : ParseResult[U]
        
        /**
         * If this result reflects a successful parse, return it, otherwise 
         * return a.
         */
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
     * A tuple for compound parser result values.  Designed to match the ~
     * combinator used for sequencing.
     */
    case class ~[+U,+V] (l : U, r : V)

    /**
     * A parser from inputs to parse results.
     */
    abstract class Parser[+T] extends (Input => ParseResult[T]) {
        
        /**
         * Alias this parser as p to make it easier to refer to in the
         * combinator definitions.
         */
        p => 
        
        /** 
         * Run this parser.
         *
         * @param in the input on which the parser should run
         * @return the result of the parse
         */
        def apply (in : Input) : ParseResult[T]
        
        /**
         * Run this parser and, if the parse was successful, apply f to the
         * result.
         */
        def map[U] (f : T => U) : Parser[U] =
            Parser { in =>
                p (in) map (f)
            }
        
        /**
         * Run this parser and, if the parse was successful, feed the resulting
         * value to f to continue parsing.
         */
        def flatMap[U] (f : T => Parser[U]) : Parser[U] =
            Parser { in =>
                p (in) flatMapWithNext (f)
            }
        
        /**
         * Run this parser and, if the parse was successful, return its result.
         * Otherwise try parsing with q.
         */
        def append[U >: T] (q : => Parser[U]) : Parser[U] =
            Parser { in =>
                p (in) append q (in)
            }
        
        /**
         * Construct a parser that applies this parser and then q, returning
         * a tuple of the results if the parses succeed.
         */
        def ~[U] (q : => Parser[U]) : Parser[~[T, U]] =
            for (v <- p; w <- q) yield new ~ (v,w)
        
        /** 
         * Construct a parser that applies this parser and then q, returning
         * the result of q if the parses succeed.
         */
        def ~>[U] (q : => Parser[U]) : Parser[U] =
            for (v <- p; w <- q) yield w

        /** 
         * Construct a parser that applies this parser and then q, returning
         * the result of this parser if the parses succeed.
         */            
        def <~[U] (q : => Parser[U]) : Parser[T] =
            for (v <- p; w <- q) yield v

        /**
         * Construct a parser that parses zero or more occurrences of
         * what this parser parses.  Collect the result values in a
         * list.
         */
        def * : Parser[List[T]] =
            (p+) | success (List ())

        /**
         * Construct a parser that parses one or more occurrences of
         * what this parser parses.  Collect the result values in a
         * list.  This parser is right recursive.
         */
        def + : Parser[List[T]] = {
            def q : Parser[List[T]] =
                (p ~ q) ^^ { case t ~ ts => t :: ts } |
                p ^^ (t => List (t))
            q
        }
            
        /**
         * Construct a parser that tries to parse using this parser,
         * and if successful, returns the result of that parse.  If
         * the parse fails, try parsing with q.
         */
        def |[U >: T] (q : => Parser[U]) : Parser[U] =
            append (q)

        /**
         * Construct a parser that parse what this parser parses and,
         * if successful, applies f to the result. 
         */
        def ^^[U] (f : T => U) : Parser[U] =
            map (f)
 
    }

    /**
     * Construct a parser that produces whatever result f produces when
     * applied to the input.
     */
    def Parser[T] (f : Input => ParseResult[T]) : Parser[T] =
       new Parser[T] { def apply (in : Input) = f (in) }
    
    /**
     * Construct a parser that always succeeds with the given result 
     * without consuming any input.
     */
    def success[T] (result : T) : Parser[T] =
        Parser { in => Success (result, in) }
    
    /**
     * Construct a parser that always fails with the given message without
     * consuming any input.
     */
    def failure (message : String) : Parser[Nothing] =
        Parser { in => Failure (message, in) }
        
    /**
     * (Implicitly) construct a parser that succeeds with e if the next input
     * element is e, and otherwise fails.
     */
    implicit def accept (e : Elem) : Parser[Elem] =
        Parser { in =>
            if (e == in.first)
                Success (in.first, in.rest)
            else
                Failure (e.toString, in)
        }
        
    /**
     * (Implicitly) construct a parser that succeeds if the given predicate 
     * answers true when applied to the next input element, and otherwise
     * fails.
     */
    implicit def acceptIf (pred : Elem => Boolean) : Parser[Elem] =
        Parser { in =>
            if (pred (in.first))
                Success (in.first, in.rest)
            else
                Failure ("acceptIf", in)
        }

}

trait CharParsers extends Parsers {
  
    /**
     * CharParsers parse character elements.
     */
    type Elem = Char
                
    /**
     * (Implicitly) construct a parser that succeeds if the next part
     * of the input is the given string, and otherwise fails.
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

    /**
     * Parse wahtever p parses preceded by optional white space.
     */
    def token[T] (p : Parser[T]) : Parser[T] =
        (whitespace*) ~> p
        
    /**
     * Parse a whitespace character.
     */
    val whitespace : Parser[Char] =
        (ch : Char) => ch.isWhitespace

    /**
     * Parse a digit character.
     */
    val digit : Parser[Char] =
        (ch : Char) => ch.isDigit

    /**
     * Parse a letter character.
     */
    val letter : Parser[Char] =
        (ch : Char) => ch.isLetter

    /**
     * Parse a letter or digit character.
     */
    val letterOrDigit : Parser[Char] =
        (ch : Char) => ch.isLetterOrDigit
    
}

/**
 * Parsers that use the packrat parsing approach to memoise parsing results,
 * including support for left recursive grammar rules.
 * 
 * The algorithsm used here are from "Packrat parsers can support left
 * recursion" by Warth, Douglass and Millstein, ACM SIGPLAN Symposium on
 * Partial Evaluation and Semantics-based Program Manipulation, 2008.
 */
trait PackratParsers extends CharParsers {
  
    import scala.collection.mutable.HashMap
    import scala.collection.mutable.Set
    import scala.util.parsing.input.Position
  
    /**
     * Information about an active instance of left recursion. 
     */
    case class Head (rule : Rule, var involvedSet : Set[Rule], var evalSet : Set[Rule])
    
    /**
     * Map between left input positions and active left recursion instances.
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
     * Common supertype for all rules (ie regardless of result type).
     */
    trait Rule
    
    /**
     * A typed rule is a memoising, left recursion-detecting encapsulation
     * of a parser that returns a value of a particular type.
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
         * Initialise the left recursion data for a new application of this
         * rule.
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
         * Process a given left recursion instance.
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
         * Look up the memoised result for this rule, taking into account that 
         * it might be participating in an active left recursion.
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
         * Grow the current parse result according to a left recursion.
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

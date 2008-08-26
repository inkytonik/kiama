package kiama.parsing

import org.scalacheck._
import kiama.example.imperative.AST._

/**
 * Run this to perform the tests.
 */
object PackratTests extends Packrat with Application {
    
    import scala.util.parsing.input.CharArrayReader

    /**
     * Convenience method for creating a reader that reads from a string.
     *
     * @param str the string to read from
     * @return the constructed reader
     */
    def input (str : String) = new CharArrayReader (str.toArray)
    
    /**
     * An input with nothing in it.
     */
    val empty = input ("")
    
    /**
     * Convert a predicate on input to a predicate on strings.
     */
    def pred (pred : Input => Boolean) : String => Boolean =
        s => { val in = input (s); pred (in) }
        
    /**
     * Equality of inputs by content.
     */
    def same (l : Input, r : Input) : Boolean = {
        var lv = l
        var rv = r
        while (!lv.atEnd) {
            if (rv.atEnd || (lv.first != rv.first)) return false
            lv = lv.rest
            rv = rv.rest
        }
        rv.atEnd
    }
    
    /**
     * Equality of parser results.
     */
    def same[T] (l : ParseResult[T], r : ParseResult[T]) : Boolean = {
        l match {
            case Success (lr, li) =>
                r match {
                    case Success (rr, ri) => (lr == rr) && same (li, ri)
                    case _                => false
                }
            case Failure (lm, li) =>
                r match {
                    case Failure (rm, ri) => (lm == rm) && same (li, ri)
                    case _                => false
                }
        }
    }
        
    /**
     * Tests that don't involve reading any input.
     */
    object NoReadTests extends Properties ("noread") {
        specify ("success.empty", same (success ("hi") (empty), Success ("hi", empty)))
        specify ("failure.empty", same (failure ("fail") (empty), Failure ("fail", empty)))
        specify ("success.gen", pred (in => same (success (42) (in), Success (42, in))))
        specify ("failure.gen", pred (in => same (failure ("fail") (in), Failure ("fail", in))))
    }
    
    /**
     * Tests that examine a single element of the input.
     */
    object SingleElemTests extends Properties ("singleelem") {
        // Looking for the first element of the input (if there is one) should
        // succeed
        specify ("success.gen", pred (in => {
            if (in.atEnd)
                true
            else
                same ((in.first) (in), Success (in.first, in.rest))
        }))
        
        // Looking for something that cannot be the first element of the input
        // (if there is one) should fail
        specify ("failure.gen", pred (in => {
            import scala.Math.{MAX_CHAR,MIN_CHAR}
            if (in.atEnd) {
                true
            } else {
                val ch = if (in.first == MAX_CHAR) MIN_CHAR else MAX_CHAR
                same (ch (in), Failure (ch.toString, in))
            }
        }))
    }
    
    /**
     * Tests of recognising the imperative language.
     */
    object ImperativeTests extends Properties ("imperative")
                           with kiama.example.imperative.TestBase {
        // Randomly generate imperative programs, pretty-print them and
        // parse the resulting string.  The resulting AST should be what
        // we started with.
        
        specify ("roundtrip", (s : Stmt) => {
            parse (input (pretty (s))) match {
                case Success (v, in) => v == s
                case failure         => println (failure); false
            }
        })
    }
    
    object AllTests extends Properties ("packrat") {
        include (NoReadTests)
        include (SingleElemTests)
        include (ImperativeTests)
    }
    
    Test.checkProperties (AllTests)

}

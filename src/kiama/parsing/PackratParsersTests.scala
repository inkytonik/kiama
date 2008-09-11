package kiama.parsing

import junit.framework.Assert._
import junit.framework.TestCase
import org.scalacheck._
import org.scalacheck.Prop._ 
import org.scalatest.junit.JUnit3Suite 
import org.scalatest.prop.Checkers 
import kiama.example.imperative.TestBase

/**
 * Run this to perform the tests.
 */
class PackratParsersTests extends TestCase with PackratParsers with TestBase
                          with JUnit3Suite with Checkers {
    
    import kiama.example.imperative.AST._
    import scala.util.parsing.input.CharArrayReader

    /**
     * Convenience method for creating a reader that reads from a string.
     *
     * @param str the string to read from
     * @return the constructed reader
     */
    def input (str : String) = new CharArrayReader (str.toArray)
        
    /**
     * Empty input.
     */
    val empty = input ("")

    /**
     * Convert a predicate on input to a predicate on strings.
     */
    def pred (p : Input => Boolean) : String => Boolean =
        s => p (input (s))
        
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
     * A successful parse on empty input succeeds with the specified
     * result.
     */
    def testNoReadSuccess () {
        assertTrue (same (success ("hi") (empty), Success ("hi", empty)))
    }
    
    /**
     * A failing parse on empty input fails with the specified message.
     */
    def testNoReadFailure () {
        assertTrue (same (failure ("fail") (empty), Failure ("fail", empty)))
    }
    
    /**
     * A successful parse succeeds with the specified result no matter
     * what the input is.
     */
    def testAnyInputSuccess () {
        check (pred (in => same (success (42) (in), Success (42, in))))
    }
    
    /**
     * A failing parse fails with the specified message no matter
     * what the input is.
     */
    def testAnyInputFail () {
        check (pred (in => same (failure ("fail") (in), Failure ("fail", in))))
    }
    
    /**
     * Looking for the first element of the input (if there is one) should
     * succeed.
     */
    def testFirstElementSuccess () {
        check (pred (in => {
            if (in.atEnd)
                true
            else
                same ((in.first) (in), Success (in.first, in.rest))
        }))
    }
        
    /**
     * Looking for something that cannot be the first element of the input
     * (if there is one) should fail.
     */
    def testFirstElementFailure () {
        check (pred (in => {
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
     * Try to parse a string and expect the given result.  Also check that
     * there is no more input left.  This is the JUnit version.
     * 
     * @param str the string to try to parse
     * @param result the result value that the parser should return 
     */
    def expect[T] (parser : Parser[T], str : String, result : T) {
        parser (input (str)) match {
            case Success (r, in) =>
                if (r != result) fail ("found " + r + " not " + result)
                if (!in.atEnd) fail ("input remaining at " + in.pos)
            case Failure (m, in) =>
                fail (m + " at " + in.pos)
        }
    }
    
    /**
     * A Boolean version of expect for use in ScalaCheck checks.
     */
    def expectBool[T] (parser : Parser[T], str : String, result : T) : Boolean = {
        val p = parser <~ (whitespace*)
        p (input (str)) match {
            case Success (r, in) =>
                (r == result) && in.atEnd
            case Failure (_, _) =>
                false            
        }
    }
    
    /**
     * Parse numbers.
     */
    def testParseNumbers () {
        check ((i : Int) => (i >= 0) ==> expectBool (number, i.toString, Num (i)))
    }
    
    /**
     * Parse variables (subsumes tests for identifier parsing).
     */
    def testParseVariables () {
        expect (variable, "a", Var ("a"))
        expect (variable, "total", Var ("total"))
        expect (variable, "sum123", Var ("sum123"))
        implicit def arbIdn : Arbitrary[String] = Arbitrary (genIdn)
        check ((s : String) => expectBool (variable, s, Var (s)))
    }
    
    /**
     * Roundtrip test parse of pretty-printed value.
     */
    def roundtrip[T <: PrettyPrintable] (parser : Parser[T])(implicit arbT : Arbitrary[T]) {
        check ((t : T) => {
            val buffer = new StringBuilder
            t.pretty (buffer)
            expectBool (parser, buffer.toString, t)
        })
    }
    
    /**
     * Parse expressions
     */
    def testParseExpressions () {
        expect (exp, "1", Num (1))
        expect (exp, "1+2", Add (Num (1), Num (2)))
        expect (exp, "1+2*3", Add (Num (1), Mul (Num (2), Num (3))))
        expect (exp, "(1+2)*3", Mul (Add (Num (1), Num (2)), Num (3)))
        roundtrip (exp)
    }
    
    /**
     * Parse a null statement.
     */
    def testParseNullStmt () {
        expect (stmt, ";", Null ())
        expect (stmt, "     ;", Null ())
    }
    
    /**
     * Parse assignment statements.
     */
    def testParseAssignStmts () {
        expect (asgnStmt, "a = 5;", Asgn ("a", Num (5)))
        expect (asgnStmt, "a = b;", Asgn ("a", Var ("b")))
        roundtrip (asgnStmt)
    }

    /**
     * Parse statement sequences.
     */
    def testParseSequences () {
        expect (sequence, "{}", Seqn (List ()))
        expect (sequence, "{ ; }", Seqn (List (Null ())))
        expect (sequence, "{ v = 1; v = 2; }",
                Seqn (List (Asgn ("v", Num (1)), Asgn ("v", Num (2)))))
        roundtrip (sequence)
    }
    
    /**
     * Parse while statements.
     */
    def testParseWhiles () {
        expect (whileStmt, "while (1) ;", While (Num (1), Null ()))
        expect (whileStmt, "while (a + 3) { a = a - 1; }",
                While (Add (Var ("a"), Num (3)),
                       Seqn (List (Asgn ("a", Sub (Var ("a"), Num (1)))))))
        roundtrip (whileStmt)
    }
    
    /**
     * Parse statements.
     */
    def testParseStatements () {
        roundtrip (stmt)
    }
    
}

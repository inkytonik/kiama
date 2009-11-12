/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2009 Anthony M Sloane, Macquarie University.
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

import junit.framework.Assert._
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalatest.junit.JUnit3Suite
import org.scalatest.prop.Checkers
import kiama.example.imperative.TestBase

/**
 * Packrat parsing test cases.
 */
class PackratParsersTests extends JUnit3Suite with Checkers
                          with CharPackratParsers with TestBase {

    import kiama.example.imperative.AST._
    import scala.util.parsing.input.CharSequenceReader

    /**
     * Convenience method for creating a parser input that reads from
     * a given string.
     */
    def input (str : String) = new CharSequenceReader (str)

    /**
     * Input containing no characters at all.
     */
    val empty = input ("")

    /**
     * Implicitly generate an input by generating a non-empty string to form its
     * contents.
     */
    implicit def arbInput : Arbitrary[Input] =
        Arbitrary (for (s <- Arbitrary.arbString.arbitrary if !s.isEmpty) yield (input (s)))

    /**
     * Implicitly generate an input based on a pretty-printable language construct.
     */
    implicit def arbASTToInput[T <: PrettyPrintable] (t : T) : Input =
        input (pretty (t))

    /**
     * Implicitly generate a string based on a pretty-printable language construct.
     */
    implicit def arbASTToString[T <: PrettyPrintable] (t : T) : String =
        pretty (t)

    /**
     * Random value generator used by some tests.
     */
    val random = new scala.util.Random

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
     * Equality of parser results, including input states.
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
     * Return true if the given parser result is a failure regardless of the
     * message or position.  Otherwise return false.
     */
    def isFail[T] (r : ParseResult[T]) : Boolean =
        r match {
            case Failure (_, _) => true
            case _              => false
        }

    /**
     * Return true if the given parser result is a failure with the given
     * message but regardless of position.  Otherwise return false.
     */
    def isFailWith[T] (r : ParseResult[T], m : String) : Boolean =
        r match {
            case Failure (`m`, _) => true
            case _                => false
        }

    /**
     * Return true if the given parser result is a success with the given
     * value but regardless of position.  Otherwise return false.
     */
    def isSuccessWith[T] (r : ParseResult[T], t : T) : Boolean =
        r match {
            case Success (`t`, _) => true
            case _                => false
        }

    /**
     * Return true if the given parser result is a failure at the given input
     * position, regardless of the message.  Otherwise return false.
     */
    def isFailAt[T] (r : ParseResult[T], in : Input) : Boolean =
        r match {
            case Failure (_, in2) => same (in, in2)
            case _                => false
        }

    /**
     * Try to parse a string and expect a given result.  Also check that
     * there is no more input left.  Return a JUnit test case result.
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
     * Try to parse a string and expect a given result.  Also check that
     * there is no more input left.  Return a Boolean result.
     */
    def expectBool[T] (parser : Parser[T], str : String, result : T) : Boolean = {
        val p = parser <~ layout
        p (input (str)) match {
            case Success (`result`, in) => in.atEnd
            case _                      => false
        }
    }

    /**
     * Roundtrip test.  Pretty print a value, parse the resulting string and
     * check that the parse result is the same as the original value.
     */
    def roundtrip[T <: PrettyPrintable] (parser : Parser[T])(implicit arbT : Arbitrary[T]) {
        check ((t : T) => {
            val buffer = new StringBuilder
            t.pretty (buffer)
            expectBool (parser, buffer.toString, t)
        })
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
        check ((in : Input) => same (success (42) (in), Success (42, in)))
    }

    /**
     * A failing parse fails with the specified message no matter
     * what the input is.
     */
    def testAnyInputFailure () {
        check ((in : Input) => same (failure ("fail") (in), Failure ("fail", in)))
    }

    /**
     * Looking for the first element of the input (if there is one) should
     * succeed.
     */
    def testFirstElementSuccess () {
        check ((in : Input) => same ((in.first) (in), Success (in.first, in.rest)))
    }

    /**
     * Looking for something that cannot be the first element of the input
     * (if there is one) should fail.
     */
    def testFirstElementFailure () {
        check ((in : Input) => {
            import scala.Math.{MAX_CHAR,MIN_CHAR}
            val ch = if (in.first == MAX_CHAR) MIN_CHAR else MAX_CHAR
            same (ch (in), Failure (ch.toString, in))
        })
    }

    /**
     * The any parser should indeed accept anything.
     */
    def testAny () {
        check ((c : Char) => {
            val in = input (c.toString)
            same (any (in), Success (c, in.rest))
        })
    }

    /**
     * Test parsing of arbitrary numbers.
     */
    def testParseNumbers () {
        check ((i : Int) => (i >= 0) ==> expectBool (integer, i.toString, Num (i)))
        check ((d : Double) => (d >= 0) ==> expectBool (double, d.toString, Num (d)))
    }

    /**
     * Test parsing of variables (subsumes tests for identifier parsing).
     */
    def testParseVariables () {
        expect (variable, "a", Var ("a"))
        expect (variable, "total", Var ("total"))
        expect (variable, "sum123", Var ("sum123"))
        implicit def arbIdn : Arbitrary[String] = Arbitrary (genIdn)
        check ((s : String) => expectBool (variable, s, Var (s)))
    }

    /**
     * Test parsing of expressions.
     */
    def testParseExpressions () {
        expect (exp, "1", Num (1))
        expect (exp, "1+2", Add (Num (1), Num (2)))
        expect (exp, "1+2*3", Add (Num (1), Mul (Num (2), Num (3))))
        expect (exp, "(1+2)*3", Mul (Add (Num (1), Num (2)), Num (3)))
        roundtrip (exp)
    }

    /**
     * Test whether keywords are appropriately rejected as identifiers.
     */
    def testKeywords () {
        assertTrue (isFail (asgnStmt (input ("while = 3;"))))
    }

    /**
     * Test parsing of null statements.
     */
    def testParseNullStmt () {
        expect (stmt, ";", Null ())
        expect (stmt, ";     ", Null ())
    }

    /**
     * Test parsing of assignment statements.
     */
    def testParseAssignStmts () {
        expect (asgnStmt, "a = 5;", Asgn ("a", Num (5)))
        expect (asgnStmt, "a = b;", Asgn ("a", Var ("b")))
        roundtrip (asgnStmt)
    }

    /**
     * Test parsing of statement sequences.
     */
    def testParseSequences () {
        expect (sequence, "{}", Seqn (Nil))
        expect (sequence, "{ ; }", Seqn (List (Null ())))
        expect (sequence, "{ v = 1; v = 2; }",
                Seqn (List (Asgn ("v", Num (1)), Asgn ("v", Num (2)))))
        roundtrip (sequence)
    }

    /**
     * Test parsing of while statements.
     */
    def testParseWhilestmts () {
        expect (whileStmt, "while (1) ;", While (Num (1), Null ()))
        expect (whileStmt, "while (a + 3) { a = a - 1; }",
                While (Add (Var ("a"), Num (3)),
                       Seqn (List (Asgn ("a", Sub (Var ("a"), Num (1)))))))
        roundtrip (whileStmt)
    }

    /**
     * Test parse of arbitrary statements.
     */
    def testParseStatements () {
        roundtrip (stmt)
    }

    /**
     * Test parsing of whole programs.
     */
    def testParsePrograms () {
        roundtrip (parse)
    }

    /**
     * Test a not predicate parser on a given string.  Return true if it succeeds
     * with ()true) and the input unchanged, otherwise return false.
     */
    def notPredSucceeds[T] (parser : Parser[T], str : String) : Boolean = {
        val in = input (str)
        parser (in) match {
            case Success ((), in2) =>
                same (in, in2)
            case Failure (_, _) =>
                false
        }
    }

    /**
     * Test a not predicate parser on a given string.  Return true if it fails
     * with the input unchanged, otherwise return false.
     */
    def notPredFails[T] (parser : Parser[T], str : String) : Boolean = {
        val in = input (str)
        isFailAt (parser (in), in)
    }

    /**
     * Test that the not predicate appropriately negates the behaviour of
     * other parsers.
     */
    def testNotPredicate () {
        assertTrue (notPredSucceeds (!integer, "hello"))
        assertTrue (notPredFails (!integer, "42"))
        assertTrue (notPredSucceeds (!asgnStmt, "a = ;"))
        assertTrue (notPredFails (!asgnStmt, "a = 5;"))
        assertTrue (notPredSucceeds (!stmt, "while (c + 3) { a = 5; b = ; }"))
        assertTrue (notPredFails (!stmt, "while (c + 3) { a = 5; b = c; }"))
        check ((s : Stmt) => notPredFails (!stmt, pretty (s)))
        check ((e : Exp) => notPredSucceeds (!stmt, pretty (e)))
    }

    /**
     * Check that +p returns the same result as p on random input, but
     * doesn't change the input.  Starts with valid phrases for the parser
     * but randomly injects whitespace to provoke errors.
     */
    def sameResultNoInput[T <: PrettyPrintable] (p : Parser[T])(implicit arbT : Arbitrary[T]) {
        check ((t : T) => {
            var s = pretty (t)
            if (random.nextBoolean) {
                val pos = random.nextInt (s.length)
                s = s.take (pos) + " " + s.drop (pos)
            }
            val in = input (s)
            val pv = p (in)
            val ppv = (+p) (in)
            pv match {
                case Success (lr, _) =>
                    ppv match {
                        case Success (rr, ri) => (lr == rr) && (ri == in)
                        case _                => false
                    }
                case Failure (lm, _) =>
                    ppv match {
                        case Failure (rm, ri) => (lm == rm) && (ri == in)
                        case _                => false
                    }
            }
        })
    }

    /**
     * Test that the and predicate preserves the results of other parsers
     * but has no effect on the input.
     */
    def testAndPredicate () {
        sameResultNoInput (integer)
        sameResultNoInput (double)
        sameResultNoInput (asgnStmt)
        sameResultNoInput (whileStmt)
        sameResultNoInput (stmt)
    }

    /**
     * Make sure that optional parsers work both ways (so to speak).
     */
    def testOptionality () {
        check ((s : Stmt) => same (stmt (s) map (t => Some (t)), (stmt?) (s)))
        check ((e : Exp) => { val in = input (e); same (stmt? (in), Success (None, in)) })
    }

    /**
     * Test that basic regular expression matching works.
     */
    def testRegexes () {
        val decimal : Parser[String] = """-?\d+\.(\d*)?""".r
        for (s <- List ("1.", "123.456", "-99.0"))
            expect (decimal, s, s)
        for (s <- List ("123", "  123", ".99", "-.001", "harold"))
            assertTrue ({val in = input (s); isFailAt (decimal (in), in)})
    }

    /**
     * Test passing values from one parser to another.
     */
    def testValueSequence () {
        // Recognise sequences of the form: n n*2
        val number = token (digit+) ^^ (s => s.mkString.toInt)
        val nntimes2 = number >> (i => token ((i*2).toString))
        expect (nntimes2, "0 0", "0")
        expect (nntimes2, "21 42", "42")
        assertTrue (isFail (nntimes2 (input ("4"))))
        assertTrue (isFail (nntimes2 (input ("12 99"))))
    }

    /**
     * Test use of partial functions to process parse results.
     */
    def testPartialFunction () {
        val p = integer ^? { case Num (i) => (i-1).toString }
        expect (p, "123", "122.0")
        assertTrue (isFail (p (input ("hello"))))

        val q = exp ^? { case Var (v) => v }
        assertTrue (isFail (q (input ("123"))))
        assertTrue (isFail (q (input ("9.0"))))

        val r = idn ^? ({case s if s.length == 3 => s + " has length 3"},
                        s => s + " unexpected")
        assertTrue (isSuccessWith (r (input ("abc")), "abc has length 3"))
        assertTrue (isFailWith (r (input ("hello")), "hello unexpected"))
    }

    /**
     * Test separator parsing.
     */
    def testSeparators () {
        val p = repsep (exp, ",")
        expect (p, "", Nil)
        assertTrue (isSuccessWith (p (empty), Nil))
        assertTrue (isSuccessWith (p (input (":=")), Nil))
        expect (p, "1, 2", List (Num (1), Num (2)))
        expect (p, "v, 67, 12.3", List (Var ("v"), Num (67), Num (12.3)))

        val q = rep1sep (exp, ",")
        assertTrue (isFail (q (empty)))
        assertTrue (isFail (q (input (":="))))
        expect (q, "1,2", List (Num (1), Num (2)))
        expect (q, "v,67,12.3", List (Var ("v"), Num (67), Num (12.3)))
    }

    /**
     * Test repetition a fixed number of times.
     */
    def testFixedRepetition () {
        expect (repN (0, exp), "", Nil)
        assertTrue (isSuccessWith (repN (0, exp) (input ("[]")), Nil))
        expect (repN (3, exp), "56 val 2.3", List (Num (56), Var ("val"), Num (2.3)))
        assertTrue (isSuccessWith (repN (2, exp) (input ("56 val 2.3")), List (Num (56), Var ("val"))))
    }

}


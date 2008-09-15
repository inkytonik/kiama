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

import junit.framework.Assert._
import junit.framework.TestCase
import org.scalacheck._
import org.scalacheck.Prop._ 
import org.scalatest.junit.JUnit3Suite 
import org.scalatest.prop.Checkers 
import kiama.example.imperative.TestBase

/**
 * Packrat parsing test cases.
 */
class PackratParsersTests extends TestCase with JUnit3Suite with Checkers
                          with PackratParsers with TestBase {
    
    import kiama.example.imperative.AST._
    import scala.util.parsing.input.CharArrayReader

    /**
     * Convenience method for creating a reader that reads from a given string.
     */
    def input (str : String) = new CharArrayReader (str.toArray)
        
    /**
     * Input containing no characters at all.
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
    def testAnyInputFailure () {
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
        val p = parser <~ (whitespace*)
        p (input (str)) match {
            case Success (r, in) =>
                (r == result) && in.atEnd
            case Failure (_, _) =>
                false            
        }
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
     * Test parsing of null statements.
     */
    def testParseNullStmt () {
        expect (stmt, ";", Null ())
        expect (stmt, "     ;", Null ())
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
        expect (sequence, "{}", Seqn (List ()))
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
    
}

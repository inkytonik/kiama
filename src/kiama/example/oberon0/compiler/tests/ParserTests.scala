/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009 Anthony M Sloane, Macquarie University.
 * 
 * Contributed by Ben Mockler.
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
 
package kiama.example.oberon0.compiler.tests

import junit.framework.Assert._
import junit.framework.TestCase
import org.scalacheck._
import org.scalacheck.Prop._ 
import org.scalatest.junit.JUnit3Suite 
import org.scalatest.prop.Checkers
import kiama.parsing.CharPackratParsers
import kiama.example.oberon0.compiler.Parser

/**
 * Oberon0 parsing test cases.
 */
class ParserTests extends TestCase with JUnit3Suite with Checkers
                          with CharPackratParsers with Parser {

    import kiama.example.oberon0.compiler.AST._    
    import scala.util.parsing.input.CharArrayReader

    /**
     * Convenience method for creating a parser input that reads from
     * a given string.
     */
    def input (str : String) = new CharArrayReader (str.toArray)

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
     * Test parsing of identifiers.
     */
    def testIdentifiers () {
        expect (ident, "a", Ident ("a"))
        expect (ident, "total", Ident ("total"))
        expect (ident, "var786", Ident ("var786"))
    }

    def testNumber () {
        expect (number, "5", Number (5))
        assertTrue (isFail (number (input ("x"))))
    }

    /**
     * Test parsing of expressions.
     */
    def testParseExpressions () {
        expect (expression, "1", Number (1))
        expect (expression, "1+2", Plus (Number (1), Number (2)))
        expect (expression, "1+2+3", Plus (Plus (Number (1), Number (2)), Number (3)))
        expect (expression, "1+2*3", Plus (Number(1), Mult (Number (2), Number (3))))
        expect (expression, "(1+2)*3", Mult (Plus (Number (1), Number (2)), Number (3)))
    }

    /**
     * Test whether keywords are appropriately rejected as identifiers.
     */
    def testKeywords () {
        assertTrue (isFail (assignment (input ("WHILE := 3"))))
    }

    /**
     * Test parsing of assignment statements.
     */
    def testParseAssignStmts () {
        expect (assignment, "a := 5", Assignment (Ident ("a"), Number (5)))
        expect (assignment, "a := b", Assignment (Ident ("a"), Ident ("b")))
    }

    /**
     * Test parsing of statement sequences.
     */
    def testParseSequences() {
        expect(statementSequence, "", Nil)
        expect(statementSequence, "v := 1; v := 2",
            List (Assignment (Ident ("v"), Number (1)),
                  Assignment (Ident ("v"), Number (2))))
    }

    /**
     * Test parsing of while statements.
     */
    def testParseWhilestmts() {
        expect (whileStatement, "WHILE x DO x:= 1 END",
                WhileStatement (Ident ("x"),
                    List (Assignment (Ident ("x"), Number (1)))))
    }

}

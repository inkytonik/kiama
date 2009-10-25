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
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalatest.junit.JUnit3Suite
import org.scalatest.prop.Checkers
import kiama.parsing.CharPackratParsers
import kiama.example.oberon0.compiler.Parser

/**
 * Oberon0 parsing test cases.
 */
class ParserTests extends JUnit3Suite with Checkers with CharPackratParsers
                  with Parser {

    import kiama.example.oberon0.compiler.AST._
    import scala.util.parsing.input.CharSequenceReader

    /**
     * Convenience method for creating a parser input that reads from
     * a given string.
     */
    def input (str : String) = new CharSequenceReader (str)

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

    def testIntegerLiteral () {
        expect (number, "5", IntegerLiteral (5))
        assertTrue (isFail (number (input ("x"))))
    }

    /**
     * Test parsing of expressions.
     */
    def testParseExpressions () {
        expect (expression, "1", IntegerLiteral (1))
        expect (expression, "1+2", Plus (IntegerLiteral (1), IntegerLiteral (2)))
        expect (expression, "1+2+3", Plus (Plus (IntegerLiteral (1), IntegerLiteral (2)), IntegerLiteral (3)))
        expect (expression, "1+2*3", Plus (IntegerLiteral(1), Mult (IntegerLiteral (2), IntegerLiteral (3))))
        expect (expression, "(1+2)*3", Mult (Plus (IntegerLiteral (1), IntegerLiteral (2)), IntegerLiteral (3)))
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
        expect (assignment, "a := 5", Assignment (Ident ("a"), IntegerLiteral (5)))
        expect (assignment, "a := b", Assignment (Ident ("a"), Ident ("b")))
    }

    /**
     * Test parsing of statement sequences.
     */
    def testParseSequences() {
        expect(statementSequence, "", Nil)
        expect(statementSequence, "v := 1; v := 2",
            List (Assignment (Ident ("v"), IntegerLiteral (1)),
                  Assignment (Ident ("v"), IntegerLiteral (2))))
    }

    /**
     * Test parsing of while statements.
     */
    def testParseWhilestmts() {
        expect (whileStatement, "WHILE x DO x:= 1 END",
                WhileStatement (Ident ("x"),
                    List (Assignment (Ident ("x"), IntegerLiteral (1)))))
    }

}

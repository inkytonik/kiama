/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2010 Anthony M Sloane, Macquarie University.
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

package org.kiama
package example.oberon0.tests

import org.scalacheck.Prop._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import org.kiama.example.oberon0.Driver
import org.kiama.example.oberon0.compiler.Parser

/**
 * Oberon0 parsing test cases.
 */
@RunWith(classOf[JUnitRunner])
class ParserTests extends Driver with FunSuite with Checkers {

    import org.kiama.example.oberon0.compiler.AST._

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
     * there is no more input left.
     */
    def expect[T] (parser : Parser[T], str : String, result : T) {
        parseAll (parser, str) match {
            case Success (r, in) =>
                if (r != result) fail ("found " + r + " not " + result)
                if (!in.atEnd) fail ("input remaining at " + in.pos)
            case f =>
                fail ("parse failure: " + f)
        }
    }

    test ("parse single letter identifiers") {
        expect (ident, "a", Ident ("a"))
    }

    test ("parse mutliple letter identifiers") {
        expect (ident, "total", Ident ("total"))
    }

    test ("parse identifier with digits in it") {
        expect (ident, "var786", Ident ("var786"))
    }

    test ("parse integer literal: single digit") {
        expect (number, "5", IntegerLiteral (5))
    }

    test ("parse integer literal: multiple digits") {
        expect (number, "123", IntegerLiteral (123))
    }

    test ("fail to parse an identifier as a number") {
        assert (isFail (parseAll (number, "x")))
    }

    test ("parse expressions: number") {
        expect (expression, "1", IntegerLiteral (1))
    }

    test ("parse expressions: one operator") {
        expect (expression, "1+2", Plus (IntegerLiteral (1), IntegerLiteral (2)))
    }

    test ("parse expressions: two operators, same precedence") {
        expect (expression, "1+2+3", Plus (Plus (IntegerLiteral (1), IntegerLiteral (2)), IntegerLiteral (3)))
    }

    test ("parse expressions: two operators, different  precedence") {
        expect (expression, "1+2*3", Plus (IntegerLiteral(1), Mult (IntegerLiteral (2), IntegerLiteral (3))))
    }

    test ("parse expressions: two operators, override  precedence") {
        expect (expression, "(1+2)*3", Mult (Plus (IntegerLiteral (1), IntegerLiteral (2)), IntegerLiteral (3)))
    }

    test ("keywords are rejected as identifiers") {
        assert (isFail (parseAll (assignment, "WHILE := 3")))
    }

    test ("parse assignment statements: number right-hand side") {
        expect (assignment, "a := 5", Assignment (Ident ("a"), IntegerLiteral (5)))
    }

    test ("parse assignment statements: identifier right-hand side") {
        expect (assignment, "a := b", Assignment (Ident ("a"), Ident ("b")))
    }

    test ("parse statement sequences: empty") {
        expect(statementSequence, "", Nil)
    }

    test ("parse statement sequences: non-empty") {
        expect(statementSequence, "v := 1; v := 2",
            List (Assignment (Ident ("v"), IntegerLiteral (1)),
                  Assignment (Ident ("v"), IntegerLiteral (2))))
    }

    test ("parse while statement") {
        expect (whileStatement, "WHILE x DO x:= 1 END",
                WhileStatement (Ident ("x"),
                    List (Assignment (Ident ("x"), IntegerLiteral (1)))))
    }

    test ("parse factorial program") {
        val program =
"""
(* A multiline comment
  another line
  do do do *)
MODULE Factorial;

CONST
    limit = 10;  (* Hello *)

VAR
    v : INTEGER;
    c : INTEGER;
    fact : INTEGER;

BEGIN
    (* Read (w); *)
    Read (v);
    IF (v < 0) OR (v > limit) THEN
        WriteLn (-1)
    ELSE
        c := 0;
        fact := 1;
        WHILE (**)c < v DO
            c := c + 1;
            fact := fact * c
(* END *)  END;
        WriteLn (fact)
    END
END Factorial.
"""
        expect (parser, program,
            ModuleDecl ("Factorial",
                List (ConstDecl ("limit", IntegerLiteral (10)),
                      VarDecl ("v", IntegerType),
                      VarDecl ("c", IntegerType),
                      VarDecl ("fact", IntegerType)),
                List (ProcedureCall (Ident ("Read"), List (Ident ("v"))),
                      IfStatement (Or (LessThan (Ident ("v"), IntegerLiteral (0)),
                                       GreaterThan (Ident ("v"), Ident ("limit"))),
                          List (ProcedureCall (Ident ("WriteLn"), List (Neg (IntegerLiteral (1))))),
                          List (Assignment (Ident ("c"), IntegerLiteral (0)),
                                Assignment (Ident ("fact"), IntegerLiteral (1)),
                                WhileStatement (LessThan (Ident ("c"), Ident ("v")),
                                     List (Assignment (Ident ("c"), Plus (Ident ("c"), IntegerLiteral (1))),
                                           Assignment (Ident ("fact"), Mult (Ident ("fact"), Ident ("c"))))),
                                ProcedureCall (Ident ("WriteLn"), List (Ident ("fact")))))),
                "Factorial",
                ModuleType ()))
    }

    filetests ("Oberon0", "src/org/kiama/example/oberon0/tests", ".ob0parseerr", ".err")

}

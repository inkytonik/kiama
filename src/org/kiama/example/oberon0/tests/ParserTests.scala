/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2011 Anthony M Sloane, Macquarie University.
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

import org.junit.runner.RunWith
import org.kiama.util.RegexParserTests 
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import org.kiama.example.oberon0.Driver
import org.kiama.example.oberon0.compiler.Parser

/**
 * Oberon0 parsing test cases.
 */
@RunWith(classOf[JUnitRunner])
class ParserTests extends Driver with RegexParserTests with Checkers {

    import org.kiama.example.oberon0.compiler.AST._

    test ("parse single letter identifiers") {
        assertParseOk ("a", ident, Ident ("a"))
    }

    test ("parse mutliple letter identifiers") {
        assertParseOk ("total", ident, Ident ("total"))
    }

    test ("parse identifier with digits in it") {
        assertParseOk ("var786", ident, Ident ("var786"))
    }

    test ("parse integer literal: single digit") {
        assertParseOk ("5", number, IntegerLiteral (5))
    }

    test ("parse integer literal: multiple digits") {
        assertParseOk ("123", number, IntegerLiteral (123))
    }

    test ("fail to parse an identifier as a number") {
        assertParseError ("x", number, 1, 1, "string matching regex `[0-9]+' expected but `x' found")
    }

    test ("parse expressions: number") {
        assertParseOk ("1", expression, IntegerLiteral (1))
    }

    test ("parse expressions: one operator") {
        assertParseOk ("1+2", expression, Plus (IntegerLiteral (1), IntegerLiteral (2)))
    }

    test ("parse expressions: two operators, same precedence") {
        assertParseOk ("1+2+3", expression, Plus (Plus (IntegerLiteral (1), IntegerLiteral (2)), IntegerLiteral (3)))
    }

    test ("parse expressions: two operators, different  precedence") {
        assertParseOk ("1+2*3", expression, Plus (IntegerLiteral(1), Mult (IntegerLiteral (2), IntegerLiteral (3))))
    }

    test ("parse expressions: two operators, override  precedence") {
        assertParseOk ("(1+2)*3", expression, Mult (Plus (IntegerLiteral (1), IntegerLiteral (2)), IntegerLiteral (3)))
    }

    test ("keywords are rejected as identifiers") {
        assertParseError ("WHILE := 3", assignment, 1, 1, "Expected failure")
    }

    test ("parse assignment statements: number right-hand side") {
        assertParseOk ("a := 5", assignment, Assignment (Ident ("a"), IntegerLiteral (5)))
    }

    test ("parse assignment statements: identifier right-hand side") {
        assertParseOk ("a := b", assignment, Assignment (Ident ("a"), Ident ("b")))
    }

    test ("parse statement sequences: empty") {
        assertParseOk ("", statementSequence, Nil)
    }

    test ("parse statement sequences: non-empty") {
        assertParseOk ("v := 1; v := 2", statementSequence,
            List (Assignment (Ident ("v"), IntegerLiteral (1)),
                  Assignment (Ident ("v"), IntegerLiteral (2))))
    }

    test ("parse while statement") {
        assertParseOk ("WHILE x DO x:= 1 END", whileStatement,
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
        assertParseOk (program, parser,
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

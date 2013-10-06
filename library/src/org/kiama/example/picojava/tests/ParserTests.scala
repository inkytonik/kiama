/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2013 Anthony M Sloane, Macquarie University.
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

/*
 * This file is derived from a JastAdd implementation of PicoJava, created
 * in the Department of Computer Science at Lund University.  See the
 * following web site for details:
 *
 * http://jastadd.cs.lth.se/examples/PicoJava/index.shtml
 */

package org.kiama
package example.picojava.tests

import org.kiama.example.picojava.Parser
import org.kiama.util.RegexParserTests

class ParserTests extends Parser with RegexParserTests {

    import org.kiama.example.picojava.AbstractSyntax._

    test ("parse identifier: single letter") {
        assertParseOk ("a", IDENTIFIER, "a")
    }

    test ("parse identifier: multiple letter") {
        assertParseOk ("ab", IDENTIFIER, "ab")
    }

    test ("parse identifier: letter and digit") {
        assertParseOk ("a1", IDENTIFIER, "a1")
    }

    test ("parse identifier: mixed letter and digit") {
        assertParseOk ("a1b", IDENTIFIER, "a1b")
    }

    test ("parse identifier: multiple mixed letter and digit") {
        assertParseOk ("a1b1", IDENTIFIER, "a1b1")
    }

    test ("parse comments") {
        assertResult (whiteSpace.replaceFirstIn ("// !@#$%^&*abc\n", "")) ("")
    }

    test ("generate errors for invalid tokens: leading underscore") {
        assertParseError ("_a", IDENTIFIER, 1, 1,
            "string matching regex `[a-zA-Z][a-zA-Z0-9]*' expected but `_' found");
    }

    test ("generate errors for invalid tokens: digit") {
        assertParseError ("1", IDENTIFIER, 1, 1,
            "string matching regex `[a-zA-Z][a-zA-Z0-9]*' expected but `1' found");
    }

    test ("generate errors for invalid tokens: leading digit") {
        assertParseError ("1a", IDENTIFIER, 1, 1,
            "string matching regex `[a-zA-Z][a-zA-Z0-9]*' expected but `1' found");
    }

    test ("generate errors for invalid tokens: C-style comment") {
        assertParseError ("/* abc */", whiteSpace, 1, 1,
            """string matching regex `(\s|(//.*\n))+' expected but `/' found""");
    }

    test ("parse an empty block") {
        assertParseOk ("{}", program, Program (Block (List ())))
    }

    test ("generate a parse error for an empty program") {
        assertParseError ("", program, 1, 1, "`{' expected but end of source found")
    }

    test ("generate a parse error for a semi-colon only program") {
        assertParseError (";", program, 1, 1, "`{' expected but `;' found")
    }

    test ("parse an empty class declaration") {
        assertParseOk ("{ class A { } }", program,
            Program (Block (List (ClassDecl ("A", None, Block (List ()))))))
    }

    test ("parse an empty class declaration with an extends clause") {
        assertParseOk ("{ class A extends B { } }", program,
            Program (Block (List (ClassDecl ("A", Some (Use ("B")), Block (List ()))))))
    }

    test ("generate a parse error for a class declaration with a qualified extends clause") {
        assertParseError ("{ class A extends A.B { } }", program, 1, 20,
            "`{' expected but `.' found")
    }

    test ("parse a nested class") {
        assertParseOk ("{ class A { class B { } } }", program,
            Program (Block (List (ClassDecl ("A", None, Block (List (ClassDecl ("B", None, Block (List ())))))))))
    }

    test ("parse a variable declaration with a simple type") {
        assertParseOk ("{ A a; }", program,
            Program (Block (List (VarDecl (Use ("A"), "a")))))
    }

    test ("parse a variable declaration with a qualified type") {
        assertParseOk ("{ A.B.C a; }", program,
            Program (Block (List (VarDecl (Dot (Dot (Use ("A"), Use ("B")), Use ("C")), "a")))))
    }

    test ("generate an error for a qualified variable declaration") {
        assertParseError ("{ A.B.C a.b; }", program, 1, 10,
            "`;' expected but `.' found")
    }

    test ("parse a simple assignment statement") {
        assertParseOk ("{ a = b; }", program,
            Program (Block (List (AssignStmt (Use ("a"), Use ("b"))))))
    }

    test ("parse an assignment statement with a qualified left-hand side") {
        assertParseOk ("{ a.b.c = b; }", program,
            Program (Block (List (AssignStmt (Dot (Dot (Use ("a"), Use ("b")), Use ("c")), Use ("b"))))))
    }

    test ("parse an assignment statement with a qualified right-hand side") {
        assertParseOk ("{ a = b.c.d; }", program,
            Program (Block (List (AssignStmt (Use ("a"), Dot (Dot (Use ("b"), Use ("c")), Use ("d")))))))
    }

    test ("parse a while statement") {
        assertParseOk ("{ while ( a ) a = b; }", program,
            Program (Block (List (WhileStmt (Use ("a"), AssignStmt (Use ("a"), Use ("b")))))))
    }

    test ("generate an error for a while statement with a block body") {
        assertParseError ("{ while ( a ) { a = b; } }", program, 1, 15,
            "`while' expected but `{' found")
    }

}

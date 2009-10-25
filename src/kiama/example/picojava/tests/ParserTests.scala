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

/**
 * This file is derived from a JastAdd implementation of PicoJava, created
 * in the Department of Computer Science at Lund University.  See the
 * following web site for details:
 *
 * http://jastadd.cs.lth.se/examples/PicoJava/index.shtml
 */

package kiama.example.picojava.tests

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite

class ParserTests extends JUnit3Suite {

    import kiama.example.picojava.AbstractSyntax._
    import kiama.example.picojava.Parser._

    def testValidIdentifiers {
        assertParseOk ("a", IDENTIFIER, "a")
        assertParseOk ("ab", IDENTIFIER, "ab")
        assertParseOk ("a1", IDENTIFIER, "a1")
        assertParseOk ("a1b", IDENTIFIER, "a1b")
        assertParseOk ("a1b1", IDENTIFIER, "a1b1")
    }

    def testValidComments {
        assertParseOk ("// !@#$%^&*abc\n", comment,
            List (' ', '!', '@', '#', '$', '%', '^', '&', '*', 'a', 'b', 'c'));
    }

    def testInvalidTokens {
        assertParseError ("_a", IDENTIFIER);
        assertParseError ("1", IDENTIFIER);
        assertParseError ("1a", IDENTIFIER);
        assertParseError ("/* abc */", comment);
    }

    def testSimpleBlock {
        assertParseOk ("{}", program, Program (Block (List ())))
    }

    def testSimpleSemi {
        assertParseError (";", program)
    }

    // Class declarations
    def testClassDecl {
        assertParseOk ("{ class A { } }", program,
            Program (Block (List (ClassDecl ("A", None, Block (List ()))))))
    }

    def testClassDeclWithExtends {
        assertParseOk ("{ class A extends B { } }", program,
            Program (Block (List (ClassDecl ("A", Some (Use ("B")), Block (List ()))))))
    }

    def testClassDeclWithQualifiedExtends { // TODO: should this be valid?
        assertParseError ("{ class A extends A.B { } }", program)
    }

    def testNestedClassDecl {
        assertParseOk ("{ class A { class B { } } }", program,
            Program (Block (List (ClassDecl ("A", None, Block (List (ClassDecl ("B", None, Block (List ())))))))))
    }

    // Variable declarations
    def testVarDecl {
        assertParseOk ("{ A a; }", program,
            Program (Block (List (VarDecl ("a", Use ("A"))))))
    }

    def testVarDeclQualifiedType {
        assertParseOk ("{ A.B.C a; }", program,
            Program (Block (List (VarDecl ("a", Dot (Dot (Use ("A"), Use ("B")), Use ("C")))))))
    }

    def testVarDeclComplexName {
        assertParseError ("{ A.B.C a.b; }", program)
    }

    // Assignment
    def testAssignStmt {
        assertParseOk ("{ a = b; }", program,
            Program (Block (List (AssignStmt (Use ("a"), Use ("b"))))))
    }

    def testAssignStmtQualifiedLHS {
        assertParseOk ("{ a.b.c = b; }", program,
            Program (Block (List (AssignStmt (Dot (Dot (Use ("a"), Use ("b")), Use ("c")), Use ("b"))))))
    }

    def testAssignStmtQualifiedRHS {
        assertParseOk ("{ a = b.c.d; }", program,
            Program (Block (List (AssignStmt (Use ("a"), Dot (Dot (Use ("b"), Use ("c")), Use ("d")))))))
    }

    // While statement
    def testWhileStmt {
        assertParseOk ("{ while ( a ) a = b; }", program,
            Program (Block (List (WhileStmt (Use ("a"), AssignStmt (Use ("a"), Use ("b")))))))
    }

    def testWhileStmtBlock { // TODO: should this be valid?
        assertParseError ("{ while ( a ) { a = b; } }", program)
    }

    /**
     * Try to parse str as a T, which is expected to work.  Assert a
     * failure if it doesn't.
     */
    def assertParseOk[T] (str : String, p : Parser[T], value : T) {
        parse (p, str) match {
            case Success (`value`, _) => // do nothing
            case Success (v, _)       => fail ("succeeded wrongly with " + v)
            case f @ Failure (_, _)   => fail (f.toString)
        }
    }

    /**
     * Try to parse str as a T, which is expected to fail.  Assert a
     * failure if it doesn't.
     */
    def assertParseError[T] (str : String, p : Parser[T]) {
        parse (p, str) match {
            case Success (_, _)     => fail ("expected to find parse error in " + str)
            case f @ Failure (_, _) => // do nothing
        }
    }

}

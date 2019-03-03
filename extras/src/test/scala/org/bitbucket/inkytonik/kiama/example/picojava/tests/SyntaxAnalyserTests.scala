/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

/*
 * This file is derived from a JastAdd implementation of PicoJava, created
 * in the Department of Computer Science at Lund University.  See the
 * following web site for details:
 *
 * http://jastadd.cs.lth.se/examples/PicoJava/index.shtml
 */

package org.bitbucket.inkytonik.kiama
package example.picojava.tests

import org.bitbucket.inkytonik.kiama.util.ParseTests

class SyntaxAnalyserTests extends ParseTests {

    import org.bitbucket.inkytonik.kiama.example.picojava.PicoJavaTree._
    import org.bitbucket.inkytonik.kiama.example.picojava.SyntaxAnalyser
    import org.bitbucket.inkytonik.kiama.util.Positions

    val positions = new Positions
    val parsers = new SyntaxAnalyser(positions)
    import parsers._

    val identifier = parsers.regex(IDENTIFIER)

    test("parse identifier: single letter") {
        identifier("a") should parseTo("a")
    }

    test("parse identifier: multiple letter") {
        identifier("ab") should parseTo("ab")
    }

    test("parse identifier: letter and digit") {
        identifier("a1") should parseTo("a1")
    }

    test("parse identifier: mixed letter and digit") {
        identifier("a1b") should parseTo("a1b")
    }

    test("parse identifier: multiple mixed letter and digit") {
        identifier("a1b1") should parseTo("a1b1")
    }

    test("parse after line comment") {
        identifier("// !@#$%^&*abc\nx") should parseTo("x")
    }

    test("generate errors for invalid tokens: leading underscore") {
        identifier("_a") should failParseAt(1, 1,
            "string matching regex '[a-zA-Z][a-zA-Z0-9]*' expected but '_' found")
    }

    test("generate errors for invalid tokens: digit") {
        identifier("1") should failParseAt(1, 1,
            "string matching regex '[a-zA-Z][a-zA-Z0-9]*' expected but '1' found")
    }

    test("generate errors for invalid tokens: leading digit") {
        identifier("1a") should failParseAt(1, 1,
            "string matching regex '[a-zA-Z][a-zA-Z0-9]*' expected but '1' found")
    }

    test("generate errors for invalid tokens: C-style comment") {
        identifier("/* abc */ x") should failParseAt(1, 1,
            """string matching regex '[a-zA-Z][a-zA-Z0-9]*' expected but '/' found""")
    }

    test("parse an empty block") {
        program("{}") should parseTo(Program(Block(Vector())))
    }

    test("generate a parse error for an empty program") {
        program("") should failParseAt(1, 1, "'{' expected but end of source found")
    }

    test("generate a parse error for a semi-colon only program") {
        program(";") should failParseAt(1, 1, "'{' expected but ';' found")
    }

    test("parse an empty class declaration") {
        program("{ class A { } }") should parseTo(
            Program(Block(Vector(ClassDecl("A", None, Block(Vector())))))
        )
    }

    test("parse an empty class declaration with an extends clause") {
        program("{ class A extends B { } }") should parseTo(
            Program(Block(Vector(ClassDecl("A", Some(Use("B")), Block(Vector())))))
        )
    }

    test("generate a parse error for a class declaration with a qualified extends clause") {
        program("{ class A extends A.B { } }") should failParseAt(1, 3,
            "'}' expected but 'c' found")
    }

    test("parse a nested class") {
        program("{ class A { class B { } } }") should parseTo(
            Program(Block(Vector(ClassDecl("A", None, Block(Vector(ClassDecl("B", None, Block(Vector()))))))))
        )
    }

    test("parse a variable declaration with a simple type") {
        program("{ A a; }") should parseTo(
            Program(Block(Vector(VarDecl(Use("A"), "a"))))
        )
    }

    test("parse a variable declaration with a qualified type") {
        program("{ A.B.C a; }") should parseTo(
            Program(Block(Vector(VarDecl(Dot(Dot(Use("A"), Use("B")), Use("C")), "a"))))
        )
    }

    test("generate an error for a qualified variable declaration") {
        program("{ A.B.C a.b; }") should failParseAt(1, 3,
            "'}' expected but 'A' found")
    }

    test("parse a simple assignment statement") {
        program("{ a = b; }") should parseTo(
            Program(Block(Vector(AssignStmt(Use("a"), Use("b")))))
        )
    }

    test("parse an assignment statement with a qualified left-hand side") {
        program("{ a.b.c = b; }") should parseTo(
            Program(Block(Vector(AssignStmt(Dot(Dot(Use("a"), Use("b")), Use("c")), Use("b")))))
        )
    }

    test("parse an assignment statement with a qualified right-hand side") {
        program("{ a = b.c.d; }") should parseTo(
            Program(Block(Vector(AssignStmt(Use("a"), Dot(Dot(Use("b"), Use("c")), Use("d"))))))
        )
    }

    test("parse a while statement") {
        program("{ while ( a ) a = b; }") should parseTo(
            Program(Block(Vector(WhileStmt(Use("a"), AssignStmt(Use("a"), Use("b"))))))
        )
    }

    test("generate an error for a while statement with a block body") {
        program("{ while ( a ) { a = b; } }") should failParseAt(1, 3,
            "'}' expected but 'w' found")
    }

}

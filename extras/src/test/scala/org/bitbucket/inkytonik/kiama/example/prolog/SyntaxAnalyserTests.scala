/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.prolog

import org.bitbucket.inkytonik.kiama.util.ParseTests

/**
 * Tests that check that the parser works correctly.  I.e., it accepts correct
 * input and produces the appropriate trees, and it rejects illegal input.
 */
class SyntaxAnalyserTests extends ParseTests {

    import PrologTree._
    import org.bitbucket.inkytonik.kiama.util.Positions

    val positions = new Positions
    val parsers = new SyntaxAnalyser(positions)
    import parsers._

    def parseToClause(l : Clause) = parseTo(l)
    def parseToLit(l : Literal) = parseTo(l)
    def parseToLits(l : Vector[Literal]) = parseTo(l)

    test("parsing an atom as an atom works") {
        atom("albert") should parseTo("albert")
    }

    test("parsing a non-atom as an atom gives an error") {
        atom("X") should failParseAt(1, 1, "string matching regex '[a-z][a-zA-Z]*' expected but 'X' found")
    }

    test("parsing an atomic literal produces the correct tree") {
        lit("albert") should parseToLit(Atom("albert"))
    }

    test("parsing a predicate literal produces the correct tree") {
        lit("likes(X,nigel)") should parseToLit(
            Pred("likes", Vector(Var("X"), Atom("nigel")))
        )
    }

    // Additional tests:

    // More atom tests

    test("parsing a single letter atom as an atom works") {
        atom("x") should parseTo("x")
    }

    // Variable tests

    test("parsing an atom as an var gives an error") {
        varr("albert") should failParseAt(1, 1, "string matching regex '[A-Z][a-zA-Z]*' expected but 'a' found")
    }

    test("parsing a single letter var as a var works") {
        varr("X") should parseTo("X")
    }

    test("parsing a length > 1 var as a var works") {
        varr("XYZ") should parseTo("XYZ")
    }

    test("parsing a mixed-case var as a var works") {
        varr("XxyyABc") should parseTo("XxyyABc")
    }

    // Program tests

    test("parsing an empty program gives an error") {
        program("") should failParseAt(1, 1, "string matching regex '[a-z][a-zA-Z]*' expected but end of source found")
    }

    test("parsing a single clause works") {
        program("female(mary).") should parseTo(
            Program(Vector(Fact(Pred("female", Vector(Atom("mary"))))))
        )
    }

    test("parsing multiple clauses works") {
        program("female(mary).\nmale (john).\nmale (luke).") should parseTo(
            Program(Vector(
                Fact(Pred("female", Vector(Atom("mary")))),
                Fact(Pred("male", Vector(Atom("john")))),
                Fact(Pred("male", Vector(Atom("luke"))))
            ))
        )
    }

    // Clause tests

    test("parsing a rule works") {
        clause("likes(john,X) :- likes(X,wine), likes(X,food).") should parseToClause(
            Rule(
                Pred("likes", Vector(Atom("john"), Var("X"))),
                Vector(
                    Pred("likes", Vector(Var("X"), Atom("wine"))),
                    Pred("likes", Vector(Var("X"), Atom("food")))
                )
            )
        )
    }

    test("parsing a fact works") {
        clause("bodgie (boo).") should parseToClause(
            Fact(Pred("bodgie", Vector(Atom("boo"))))
        )
    }

    // Literal tests

    test("parsing an atomic literal works") {
        lit("roger") should parseToLit(Atom("roger"))
    }

    test("parsing a variable as a literal fails") {
        lit("Vavoom") should failParseAt(1, 1, "string matching regex '[a-z][a-zA-Z]*' expected but 'V' found")
    }

    test("can't parse a predicate literal with zero arguments") {
        lit("likes ()") should parseToLit(Atom("likes"))
    }

    test("parsing a predicate literal with one argument works") {
        lit("likes (X)") should parseToLit(
            Pred("likes", Vector(Var("X")))
        )
    }

    test("parsing a predicate literal with many argument works") {
        lit("likes (X, at, VAR)") should parseToLit(
            Pred("likes", Vector(Var("X"), Atom("at"), Var("VAR")))
        )
    }

    test("parsing a predicate literal with a predicate argument works") {
        lit("likes (X, likes (Y, Z), W)") should parseToLit(
            Pred("likes", Vector(
                Var("X"),
                Pred("likes", Vector(Var("Y"), Var("Z"))),
                Var("W")
            ))
        )
    }

    test("parsing a cut works") {
        cut("!") should parseTo(Cut())
    }

    test("parsing a literal list containg a cut works") {
        lits("likes (X), !, male (Y)") should parseToLits(
            Vector(
                Pred("likes", Vector(Var("X"))),
                Cut(),
                Pred("male", Vector(Var("Y")))
            )
        )
    }

    test("can't parse a nested cut") {
        lit("likes (!)") should parseToLit(Atom("likes"))
    }

    // Literal list tests, assuming that literal tests take care of most cases
    // for the components

    test("parsing an empty literal list gives an error") {
        lits("") should failParseAt(1, 1, "'!' expected but end of source found")
    }

    test("parsing a singleton literal list works") {
        lits("nonny (harold)") should parseToLits(
            Vector(Pred("nonny", Vector(Atom("harold"))))
        )
    }

    test("parsing multiple literal list works") {
        lits("nonny (harold), ninny (tony), nanny (jane)") should parseToLits(
            Vector(
                Pred("nonny", Vector(Atom("harold"))),
                Pred("ninny", Vector(Atom("tony"))),
                Pred("nanny", Vector(Atom("jane")))
            )
        )
    }

    // Integer tests

    test("parsing a single digit integer works") {
        integer("0") should parseTo(Integer(0))
        integer("4") should parseTo(Integer(4))
        integer("7") should parseTo(Integer(7))
        integer("9") should parseTo(Integer(9))
    }

    test("parsing a non-trivial integer works") {
        integer("78") should parseTo(Integer(78))
        integer("123") should parseTo(Integer(123))
        integer("793223") should parseTo(Integer(793223))
    }

    test("parsing non-integers as integers fails") {
        integer("x") should failParseAt(1, 1,
            "string matching regex '[0-9]+' expected but 'x' found")
        integer("Eugene") should failParseAt(1, 1,
            "string matching regex '[0-9]+' expected but 'E' found")
        integer("(") should failParseAt(1, 1,
            "string matching regex '[0-9]+' expected but '(' found")
    }

    // List terms

    test("parsing an empty list works") {
        list("[]") should parseToLit(Pred("nil", Vector()))
    }

    test("parsing a singleton list works") {
        list("[a]") should parseToLit(
            Pred("cons", Vector(
                Atom("a"),
                Pred("nil", Vector())
            ))
        )
    }

    test("parsing a multiple-element list works") {
        list("[a,b,c]") should parseToLit(
            Pred("cons", Vector(
                Atom("a"),
                Pred("cons", Vector(
                    Atom("b"),
                    Pred("cons", Vector(
                        Atom("c"),
                        Pred("nil", Vector())
                    ))
                ))
            ))
        )
    }

}

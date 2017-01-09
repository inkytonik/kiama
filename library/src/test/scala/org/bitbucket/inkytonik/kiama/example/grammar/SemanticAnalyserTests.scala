/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2017 Anthony M Sloane, Macquarie University.
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

package org.bitbucket.inkytonik.kiama
package example.grammar

import org.bitbucket.inkytonik.kiama.util.Tests

/**
 * Tests of grammar semantic analysis.
 */
class SemanticAnalyserTests extends Tests {

    import GrammarTree._
    import org.bitbucket.inkytonik.kiama.util.Message

    def S = NonTermSym(NonTermUse("S"))
    def E = NonTermSym(NonTermUse("E"))
    def Ep = NonTermSym(NonTermUse("E'"))
    def T = NonTermSym(NonTermUse("T"))
    def Tp = NonTermSym(NonTermUse("T'"))
    def F = NonTermSym(NonTermUse("F"))

    def plus = TermSym("+")
    def star = TermSym("*")
    def lparen = TermSym("(")
    def rparen = TermSym(")")
    def id = TermSym("id")

    /**
     *   S -> E $
     *   E -> E + T | T
     *   T -> T * F | F
     *   F  -> ( E ) | id
     */

    val g1r1 = mkRule(NonTermDef("S"), mkProd(E, EOI))
    val g1r2 = mkRule(NonTermDef("E"), mkProd(E, plus, T),
        mkProd(T))
    val g1r3 = mkRule(NonTermDef("T"), mkProd(T, star, F),
        mkProd(F))
    val g1r4 = mkRule(NonTermDef("F"), mkProd(lparen, E, rparen),
        mkProd(id))

    val g1 = Grammar(g1r1, Vector(g1r2, g1r3, g1r4))

    val tree1 = new GrammarTree(g1)
    val g1analyser = new SemanticAnalyser(tree1)

    /**
     *   S -> E $
     *   E  -> T E'
     *   E' -> + T E' | e
     *   T  -> F T'
     *   T' -> * F T' | e
     *   F  -> ( E ) | id
     */

    val g2r1 = mkRule(NonTermDef("S"), mkProd(E, EOI))
    val g2r2 = mkRule(NonTermDef("E"), mkProd(T, Ep))
    val g2r3 = mkRule(NonTermDef("E'"), mkProd(plus, T, Ep),
        mkProd())
    val g2r4 = mkRule(NonTermDef("T"), mkProd(F, Tp))
    val g2r5 = mkRule(NonTermDef("T'"), mkProd(star, F, Tp),
        mkProd())
    val g2r6 = mkRule(NonTermDef("F"), mkProd(lparen, E, rparen),
        mkProd(id))

    val g2 = Grammar(g2r1, Vector(g2r2, g2r3, g2r4, g2r5, g2r6))

    val tree2 = new GrammarTree(g2)
    val g2analyser = new SemanticAnalyser(tree2)

    /**
     *   S -> E F $
     *   E -> +
     *   E -> *
     */

    val g3F = NonTermUse("F")
    val g3E1 = NonTermDef("E")
    val g3E2 = NonTermDef("E")

    val g3r1 = mkRule(NonTermDef("S"), mkProd(E, NonTermSym(g3F), EOI))
    val g3r2 = mkRule(g3E1, mkProd(plus))
    val g3r3 = mkRule(g3E2, mkProd(star))

    val g3 = Grammar(g3r1, Vector(g3r2, g3r3))

    val tree3 = new GrammarTree(g3)
    val g3analyser = new SemanticAnalyser(tree3)

    test("g1: has no semantic errors") {
        g1analyser.errors.length shouldBe 0
    }

    test("g1: S is not nullable") {
        g1analyser.nullable(g1r1) shouldBe false
    }

    test("g1: E is not nullable") {
        g1analyser.nullable(g1r2) shouldBe false
    }

    test("g1: T is not nullable") {
        g1analyser.nullable(g1r3) shouldBe false
    }

    test("g1: F is not nullable") {
        g1analyser.nullable(g1r4) shouldBe false
    }

    test("g1: FIRST (S) is correct") {
        g1analyser.first(g1r1) shouldBe Set(lparen, id)
    }

    test("g1: FIRST (E) is correct") {
        g1analyser.first(g1r2) shouldBe Set(lparen, id)
    }

    test("g1: FIRST (T) is correct") {
        g1analyser.first(g1r3) shouldBe Set(lparen, id)
    }

    test("g1: FIRST (F) is correct") {
        g1analyser.first(g1r4) shouldBe Set(lparen, id)
    }

    test("g1: FOLLOW (S) is correct") {
        g1analyser.follow(g1r1.lhs) shouldBe Set()
    }

    test("g1: FOLLOW (E) is correct") {
        g1analyser.follow(g1r2.lhs) shouldBe Set(EOI, rparen, plus)
    }

    test("g1: FOLLOW (T) is correct") {
        g1analyser.follow(g1r3.lhs) shouldBe Set(EOI, rparen, plus, star)
    }

    test("g1: FOLLOW (F) is correct") {
        g1analyser.follow(g1r4.lhs) shouldBe Set(EOI, rparen, plus, star)
    }

    test("g2: has no semantic errors") {
        g2analyser.errors.length shouldBe 0
    }

    test("g2: S is not nullable") {
        g2analyser.nullable(g2r1) shouldBe false
    }

    test("g2: E is not nullable") {
        g2analyser.nullable(g2r2) shouldBe false
    }

    test("g2: Ep is nullable") {
        g2analyser.nullable(g2r3) shouldBe true
    }

    test("g2: T is not nullable") {
        g2analyser.nullable(g2r4) shouldBe false
    }

    test("g2: Tp is nullable") {
        g2analyser.nullable(g2r5) shouldBe true
    }

    test("g2: F is not nullable") {
        g2analyser.nullable(g2r6) shouldBe false
    }

    test("g2: FIRST (S) is correct") {
        g2analyser.first(g2r1) shouldBe Set(lparen, id)
    }

    test("g2: FIRST (E) is correct") {
        g2analyser.first(g2r2) shouldBe Set(lparen, id)
    }

    test("g2: FIRST (Ep) is correct") {
        g2analyser.first(g2r3) shouldBe Set(plus)
    }

    test("g2: FIRST (T) is correct") {
        g2analyser.first(g2r4) shouldBe Set(lparen, id)
    }

    test("g2: FIRST (Tp) is correct") {
        g2analyser.first(g2r5) shouldBe Set(star)
    }

    test("g2: FIRST (F) is correct") {
        g2analyser.first(g2r6) shouldBe Set(lparen, id)
    }

    test("g2: FOLLOW (S) is correct") {
        g2analyser.follow(g2r1.lhs) shouldBe Set()
    }

    test("g2: FOLLOW (E) is correct") {
        g2analyser.follow(g2r2.lhs) shouldBe Set(EOI, rparen)
    }

    test("g2: FOLLOW (Ep) is correct") {
        g2analyser.follow(g2r3.lhs) shouldBe Set(EOI, rparen)
    }

    test("g2: FOLLOW (T) is correct") {
        g2analyser.follow(g2r4.lhs) shouldBe Set(EOI, rparen, plus)
    }

    test("g2: FOLLOW (Tp) is correct") {
        g2analyser.follow(g2r5.lhs) shouldBe Set(EOI, rparen, plus)
    }

    test("g2: FOLLOW (F) is correct") {
        g2analyser.follow(g2r6.lhs) shouldBe Set(EOI, rparen, plus, star)
    }

    test("g3: has the expected semantic errors") {
        val errors = g3analyser.errors.sorted
        errors.length shouldBe 3
        errors(0).label shouldBe "E is defined more than once"
        errors(1).label shouldBe "E is defined more than once"
        Vector(errors(0).value, errors(1).value) should haveSameElementsAs(Vector[AnyRef](g3E1, g3E2))
        errors(2).label shouldBe "F is not declared"
        errors(2).value should be theSameInstanceAs g3F
    }

}

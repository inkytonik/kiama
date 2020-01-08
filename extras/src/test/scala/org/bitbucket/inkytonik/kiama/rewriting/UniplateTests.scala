/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package rewriting

import org.bitbucket.inkytonik.kiama.util.KiamaTests
import org.bitbucket.inkytonik.kiama.example.imperative.Generator

/**
 * Tests based on examples from the paper "Uniform boilerplate and list
 * processing" by Mitchell and Runciman, from Haskell Workshop 2007.
 */
class UniplateTests extends KiamaTests with Generator {

    import org.bitbucket.inkytonik.kiama.example.imperative.ImperativeTree._
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
    import org.scalacheck._
    import org.scalacheck.Prop.{collect => _, _}

    /**
     * A simple numeric test expression.
     */
    val numexp = Num(42)

    /**
     * A simple test expression involving variables.
     */
    val varexp = Div(Mul(Var("var1"), Var("var2")), Var("var1"))

    test("collection of variable references: direct style") {
        /*
         *  Direct style: local management of the collection.
         */
        def variables(e : Exp) : Set[String] = {
            var vars = Set[String]()
            everywhere(query[Var] { case Var(s) => vars += s })(e)
            vars
        }
        check((e : Exp) => variables(e) == e.vars)
    }

    {
        val variabless = collect[Set, String] { case Var(s) => s }
        val variablesv = collect[Vector, String] { case Var(s) => s }
        val variablesl = collect[List, String] { case Var(s) => s }

        test("singleton collection of variable references: indirect style") {
            check((e : Exp) => variabless(e) == e.vars)
        }

        test("singleton collection of variable references: indirect style on sets and lists") {
            variabless(numexp) shouldBe Set()
            variablesv(numexp) shouldBe Vector()
            variablesl(numexp) shouldBe Nil
            variabless(varexp) shouldBe Set("var1", "var2")
            variablesv(varexp) shouldBe Vector("var1", "var2", "var1")
            variablesl(varexp) shouldBe List("var1", "var2", "var1")
        }
    }

    {
        val variabless = collectall { case Var(s) => Set(s) }
        val variablesv = collectall { case Var(s) => Vector(s) }
        val variablesl = collectall { case Var(s) => List(s) }

        test("all collection of variable references: indirect style") {
            // Indirect: using the collects combinator to manage the set
            check((e : Exp) => variabless(e) == e.vars)
        }

        test("all collection of variable references: indirect style on sets and lists") {
            variabless(numexp) shouldBe Set()
            variablesv(numexp) shouldBe Vector()
            variablesl(numexp) shouldBe Nil
            variabless(varexp) shouldBe Set("var1", "var2")
            variablesv(varexp) shouldBe Vector("var1", "var2", "var1")
            variablesl(varexp) shouldBe List("var1", "var2", "var1")
        }
    }

    test("search for division by zero") {
        object TestDivsByZero extends Generator {
            override def genDiv(sz : Int) : Gen[Div] =
                Gen.frequency((3, genDivByZero(sz)), (1, super.genDiv(sz)))
            def genDivByZero(sz : Int) : Gen[Div] =
                for { l <- genExp(sz / 2) } yield Div(l, Num(0))
            val divsbyzero = count { case Div(_, Num(0)) => 1 }
            divsbyzero(numexp) shouldBe 0
            divsbyzero(varexp) shouldBe 0
            check((e : Exp) => divsbyzero(e) == e.divsbyzero)
        }
        TestDivsByZero
    }

    test("arithmetic simplification") {
        def simplify : Exp => Exp =
            rewrite(everywhere(rule[Exp] {
                case Sub(x, y)           => simplify(Add(x, Neg(y)))
                case Add(x, y) if x == y => Mul(Num(2), x)
            }))
        simplify(numexp) shouldBe numexp
        simplify(varexp) shouldBe varexp

        val e = Sub(
            Add(Var("a"), Var("a")),
            Add(Sub(Var("b"), Num(1)), Sub(Var("b"), Num(1)))
        )
        val simpe = Add(
            Mul(Num(2), Var("a")),
            Neg(Mul(Num(2), Add(Var("b"), Neg(Num(1)))))
        )
        simplify(e) shouldBe simpe

        val f = Sub(Neg(Num(1)), Num(1))
        val simpf = Mul(Num(2), Neg(Num(1)))
        simplify(f) shouldBe simpf

        check((e : Exp) => simplify(e).value == e.value)
    }

    test("remove double negations") {
        object TestDoubleNegSimplification extends Generator {
            override def genNeg(sz : Int) : Gen[Neg] =
                Gen.frequency((1, genDoubleNeg(sz)), (1, super.genNeg(sz)))
            def genDoubleNeg(sz : Int) : Gen[Neg] =
                for { e <- super.genNeg(sz) } yield Neg(e)
            def doubleneg : Exp => Exp =
                rewrite(everywherebu(rule[Exp] { case Neg(Neg(x)) => x }))
            doubleneg(numexp) shouldBe numexp
            doubleneg(varexp) shouldBe varexp
            check((e : Exp) => doubleneg(e).value == e.value)
        }
        TestDoubleNegSimplification
    }

    test("reciprocal division to multiplication conversion") {
        def reciprocal : Exp => Exp =
            rewrite(everywherebu(rule[Exp] {
                case Div(n, m) => Mul(n, Div(Num(1), m))
            }))

        val e1 = Div(Num(1), Num(2))
        reciprocal(e1).value shouldBe 0.5

        val e2 = Mul(Num(2), Div(Num(3), Num(4)))
        reciprocal(e2).value shouldBe 1.5
    }

    test("unique variable renaming") {
        def uniquevars : Exp => Exp = {
            var count = 0
            rewrite({
                everywhere(rule[Var] { case _ => count = count + 1; Var(s"x$count") })
            })
        }
        uniquevars(numexp) shouldBe numexp
        // Run this twice to make sure that count is not shared
        uniquevars(varexp) shouldBe Div(Mul(Var("x1"), Var("x2")), Var("x3"))
        uniquevars(varexp) shouldBe Div(Mul(Var("x1"), Var("x2")), Var("x3"))
        check((e : Exp) => uniquevars(e).value == e.value)
    }

    test("calculate expression depth") {
        val depth = para[Int] { case (t, cs) => 1 + (cs :+ 0).max }
        depth(numexp) shouldBe 2
        depth(varexp) shouldBe 4
        check((e : Exp) => depth(e) == e.depth)
    }

    test("variable renaming") {
        def rename : Exp => Exp =
            rewrite(everywhere(rule[Var] { case Var(s) => Var(s"_$s") }))
        check((e : Exp) => rename(e).vars == e.vars.map("_" + _))
    }

    test("optimisation of integer addition") {
        object OptimiseAdd extends Generator {
            override def genAdd(sz : Int) : Gen[Add] =
                Gen.frequency((1, genIntAdd(sz)), (1, super.genAdd(sz)))
            def genIntAdd(sz : Int) : Gen[Add] =
                for { l <- genNum; r <- genNum } yield Add(l, r)
            def optimiseadd : Exp => Exp =
                rewrite(everywherebu(rule[Exp] {
                    case Add(Num(n), Num(m)) => Num(n + m)
                }))
            check((e : Exp) => {
                val eopt = optimiseadd(e)
                (eopt.intadds == 0) && (eopt.value == e.value)
            })
        }
        OptimiseAdd
    }

}

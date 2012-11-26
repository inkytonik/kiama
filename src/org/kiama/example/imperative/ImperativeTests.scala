/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2012 Anthony M Sloane, Macquarie University.
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
package example.imperative

import org.kiama.util.GeneratingREPL
import org.kiama.util.Tests
import org.scalatest.junit.JUnitRunner

/**
 * Imperative language tests pretty-printer tests.
 * Quite a few of the tests of other modules also use the imperative
 * language.
 */
class ImperativeTests extends Tests {

    import AST._
    import PrettyPrinter._
    
    test ("pretty-print imperative variable") {
        expectResult ("xyz123") (pretty (Var ("xyz123")))
    }

    test ("pretty-print imperative variable - product") {
        expectResult ("""Var ("xyz123")""") (pretty_any (Var ("xyz123")))
    }

    test ("pretty-print imperative assignment") {
        expectResult ("i = (0.0 * j);") (
            pretty (Asgn (Var ("i"), Mul (Num (0), Var ("j"))))
        )
    }

    test ("pretty-print imperative assignment - product") {
        expectResult ("""Asgn (Var ("i"), Mul (Num (0.0), Var ("j")))""") (
            pretty_any (Asgn (Var ("i"), Mul (Num (0), Var ("j"))))
        )
    }

    // { i = 10; count = 0; while (i) { count = count + 1; i = 1 + i; } }
    val p = 
        Seqn (List (
            Asgn (Var ("i"), Num (10)),
            Asgn (Var ("count"), Num (0)),
            While (Var ("i"),
                Seqn (List (
                    Asgn (Var ("count"), Add (Var ("count"), Num (1))),
                    Asgn (Var ("i"), Add (Num (1), Var ("i"))))))))
    
    val pp1 = 
        """{
          |    i = 10.0;
          |    count = 0.0;
          |    while (i) { count = (count + 1.0); i = (1.0 + i); }
          |}""".stripMargin

    val pp2 =
        """{
          |    i = 10.0;
          |    count = 0.0;
          |    while (i)
          |        {
          |            count = (count + 1.0);
          |            i = (1.0 + i);
          |        }
          |}""".stripMargin
    
    val ppp =
        """Seqn (
          |    List (
          |        Asgn (Var ("i"), Num (10.0)),
          |        Asgn (Var ("count"), Num (0.0)),
          |        While (
          |            Var ("i"),
          |            Seqn (
          |                List (
          |                    Asgn (
          |                        Var ("count"),
          |                        Add (Var ("count"), Num (1.0))),
          |                    Asgn (Var ("i"), Add (Num (1.0), Var ("i"))))))))""".stripMargin

    test ("pretty-print non-trivial imperative program (default width)") {
        expectResult (pp1) (pretty (p)) 
    }

    test ("pretty-print non-trivial imperative program (narrow)") {
        expectResult (pp2) (pretty (group (show (p)), 40))
    }

    test ("pretty-print non-trivial imperative program (product)") {
        expectResult (ppp) (pretty_any (p))
    }
    
}

/**
 * ScalaCheck generators for programs in the imperative language.
 */
trait Generator {

    import org.scalacheck._
    import AST._

    val genInteger = for (i <- Gen.choose (1, 100)) yield Num (i)
    val genDouble = for (i <- Gen.choose (1.0, 1000000.0)) yield Num (i)
    val genNum = Gen.frequency ((3, genInteger), (1, genDouble))

    implicit def arbNum : Arbitrary[Num] =
        Arbitrary (genNum)

    val genIdn : Gen[String] = for (s <- Gen.identifier) yield (s.take (5))
    val genVar = for (v <- genIdn) yield Var (v)

    val genLeafExp = Gen.oneOf (genNum, genVar)

    def genNeg (sz : Int) : Gen[Neg] =
        for { e <- genExp (sz/2) } yield Neg (e)

    def genAdd (sz : Int) : Gen[Add] =
        for { l <- genExp (sz/2); r <- genExp (sz/2) } yield Add (l, r)

    def genSub (sz : Int) : Gen[Sub] =
        for { l <- genExp (sz/2); r <- genExp (sz/2) } yield Sub (l, r)

    def genMul (sz : Int) : Gen[Mul] =
        for { l <- genExp (sz/2); r <- genExp (sz/2) } yield Mul (l, r)

    def genDiv (sz : Int) : Gen[Div] =
        for { l <- genExp (sz/2); r <- genExp (sz/2) } yield Div (l, r)

    def genInternalExp (sz : Int) : Gen[Exp] =
        Gen.oneOf (genAdd (sz), genSub (sz), genMul (sz), genDiv (sz))

    def genExp (sz : Int) : Gen[Exp] =
        if (sz <= 0)
            genLeafExp
        else
            Gen.frequency ((1, genLeafExp), (3, genInternalExp (sz)))

    implicit def arbExp : Arbitrary[Exp] =
        Arbitrary { Gen.sized (sz => genExp (sz)) }

    val genLeafStmt = Gen.value (Null ())

    def genSeqn (sz : Int) : Gen[Seqn] =
        for { len <- Gen.choose (1,sz)
              ss <- Gen.containerOfN[List,Stmt] (len, genStmt (sz / len)) }
            yield Seqn (ss)

    implicit def arbSeqn : Arbitrary[Seqn] =
        Arbitrary { Gen.sized (sz => genSeqn (sz)) }

    def genAsgn (sz : Int) : Gen[Asgn] =
        for { v <- genVar; e <- genExp (sz-1) } yield Asgn (v, e)

    implicit def arbAsgn : Arbitrary[Asgn] =
        Arbitrary { Gen.sized (sz => genAsgn (sz)) }

    def genWhile (sz : Int) : Gen[While] =
        for { e <- genExp (sz/3); b <- genStmt (sz - 1) } yield While (e, b)

    implicit def arbWhile : Arbitrary[While] =
        Arbitrary { Gen.sized (sz => genWhile (sz)) }

    def genInternalStmt (sz : Int) : Gen[Stmt] =
        Gen.frequency ((1, genSeqn (sz)), (5, genAsgn (sz)), (3, genWhile (sz)))

    def genStmt (sz : Int) : Gen[Stmt] =
        if (sz <= 0)
            genLeafStmt
        else
            Gen.frequency ((1, genLeafStmt), (9, genInternalStmt (sz)))

    implicit def arbStmt : Arbitrary[Stmt] =
        Arbitrary { Gen.sized (sz => genStmt (sz)) }

}

/**
 * A read-eval-print loop for generating random imperative statements.
 */
object ImperativeGen extends GeneratingREPL[AST.Stmt] with Generator {

    import org.scalacheck.Arbitrary

    def generator : Arbitrary[AST.Stmt] =
        arbStmt

    override def process (s : AST.Stmt) {
        emitter.emitln (s)
        emitter.emitln (PrettyPrinter.pretty (s))
    }

}

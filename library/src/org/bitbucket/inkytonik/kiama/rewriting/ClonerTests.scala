/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2016 Anthony M Sloane, Macquarie University.
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
package rewriting

import org.bitbucket.inkytonik.kiama.util.Tests

/**
 * Cloning tests.
 */
class ClonerTests extends Tests {

    import org.bitbucket.inkytonik.kiama.example.imperative.ImperativeTree._
    import org.bitbucket.inkytonik.kiama.relation.Tree
    import org.bitbucket.inkytonik.kiama.rewriting.Cloner.{deepclone, lazyclone}
    import org.scalatest.matchers.{Matcher, MatchResult}

    /**
     * Predicate for trees that works by making a `Tree` and trapping the
     * exception that results if it's not actually a tree structure.
     */
    def isATree[T <: Product](value : T) : Boolean =
        try {
            new Tree[T, T](value)
            true
        } catch {
            case e : RuntimeException =>
                false
        }

    test("deep cloning a term gives an equal but not eq term") {
        val t = Add(
            Mul(
                Add(Num(1), Num(2)),
                Sub(
                    Add(Num(1), Num(2)),
                    Add(Num(1), Num(2))
                )
            ),
            Add(
                Add(
                    Add(Num(3), Num(4)),
                    Num(5)
                ),
                Add(Num(3), Num(4))
            )
        )

        val ttree = new Tree[Exp, Exp](t)
        val ct : Add = deepclone(t)

        isATree(ct) shouldBe true
        ct shouldBe t
        ct should not(be theSameInstanceAs t)
        ct.l should not(be theSameInstanceAs t.l)
        ct.r should not(be theSameInstanceAs t.r)
    }

    test("deep cloning a term containing a sequence works") {
        val t = Seqn(Vector(
            Asgn(Var("a"), Num(1)),
            Asgn(Var("b"), Num(2)),
            Asgn(Var("c"), Num(3))
        ))

        val ttree = new Tree[ImperativeNode, Seqn](t)
        val ct : Seqn = deepclone(t)

        isATree(ct) shouldBe true
        ct shouldBe t
        ct should not(be theSameInstanceAs t)
        ct.ss should not(be theSameInstanceAs t.ss)
        ct.ss(0) should not(be theSameInstanceAs t.ss(0))
        ct.ss(1) should not(be theSameInstanceAs t.ss(1))
        ct.ss(2) should not(be theSameInstanceAs t.ss(2))
    }

    test("lazy cloning a term with no sharing gives that term") {
        val t = Add(Num(1), Num(2))
        val ttree = new Tree[Exp, Exp](t)
        val ct : Add = lazyclone(t)

        isATree(ct) shouldBe true
        ct shouldBe t
        ct should be theSameInstanceAs t
    }

    test("lazy cloning a term with sharing clones the shared sub-term") {
        val u = Add(Num(1), Num(2))
        val t = Add(u, u)
        val ttree = new Tree[Exp, Exp](t)
        val ct : Add = lazyclone(t)

        isATree(ct) shouldBe true
        ct shouldBe t
        ct should not(be theSameInstanceAs t)
        ct.l should be theSameInstanceAs t.l
        ct.r should not(be theSameInstanceAs t.r)
    }

}

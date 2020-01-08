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

/**
 * Cloning tests.
 */
class ClonerTests extends KiamaTests {

    import org.bitbucket.inkytonik.kiama.example.imperative.ImperativeTree._
    import org.bitbucket.inkytonik.kiama.relation.Tree
    import org.bitbucket.inkytonik.kiama.rewriting.Cloner.{deepclone, lazyclone}

    /**
     * Predicate for trees that works by making a `Tree` and trapping the
     * exception that results if it's not actually a tree structure.
     */
    def isATree[T <: AnyRef with Product](value : T) : Boolean =
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
        val ct : Add = lazyclone(t)

        isATree(ct) shouldBe true
        ct shouldBe t
        ct should be theSameInstanceAs t
    }

    test("lazy cloning a term with sharing clones the shared sub-term") {
        val u = Add(Num(1), Num(2))
        val t = Add(u, u)
        val ct : Add = lazyclone(t)

        isATree(ct) shouldBe true
        ct shouldBe t
        ct should not(be theSameInstanceAs t)
        ct.l should be theSameInstanceAs t.l
        ct.r should not(be theSameInstanceAs t.r)
    }

}

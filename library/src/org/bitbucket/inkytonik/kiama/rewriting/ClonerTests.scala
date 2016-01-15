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

    def assertCloned[T <: Product, R <: T](t : R, ct : R) {

        // Must get the right answer (==)
        assertResult(t)(ct)

        // Make sure that the new term is actually a tree. If it's not, trying
        // to make a Tree from it will throw a RuntimeException.
        try {
            new Tree[T, R](ct)
        } catch {
            case e : RuntimeException =>
                fail(s"cloning didn't produce a tree: $ct")
        }

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

        assertCloned[Exp, Exp](t, ct)
        assertNotSame(t)(ct)
        assertNotSame(t.l)(ct.l)
        assertNotSame(t.r)(ct.r)
    }

    test("deep cloning a term containing a sequence works") {
        val t = Seqn(Vector(
            Asgn(Var("a"), Num(1)),
            Asgn(Var("b"), Num(2)),
            Asgn(Var("c"), Num(3))
        ))

        val ttree = new Tree[ImperativeNode, Seqn](t)
        val ct : Seqn = deepclone(t)

        assertCloned[ImperativeNode, Seqn](t, ct)
        assertNotSame(t)(ct)
        assertNotSame(t.ss)(ct.ss)
        assertNotSame(t.ss(0))(ct.ss(0))
        assertNotSame(t.ss(1))(ct.ss(1))
        assertNotSame(t.ss(2))(ct.ss(2))
    }

    test("lazy cloning a term with no sharing gives that term") {
        val t = Add(Num(1), Num(2))
        val ttree = new Tree[Exp, Exp](t)
        val ct : Add = lazyclone(t)

        assertCloned[Exp, Exp](t, ct)
        assertSame(t)(ct)
    }

    test("lazy cloning a term with sharing clones the shared sub-term") {
        val u = Add(Num(1), Num(2))
        val t = Add(u, u)
        val ttree = new Tree[Exp, Exp](t)
        val ct : Add = lazyclone(t)

        assertCloned[Exp, Exp](t, ct)
        assertNotSame(t)(ct)
        assertSame(t.l)(ct.l)
        assertNotSame(t.r)(ct.r)
    }

}

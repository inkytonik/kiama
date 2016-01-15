/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2016 Anthony M Sloane, Macquarie University.
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
 * Positioned rewriting tests.
 */
class PositionedRewriterTests extends Tests {

    import PositionedRewriter._
    import SupportPositionedRewriterTests._
    import org.bitbucket.inkytonik.kiama.util.{Position, StringSource}

    val source = StringSource("dummy")
    override val positions = PositionedRewriter.positions

    /**
     * Don't do anything to the positions before the next test so that
     * settings are preserved.
     */
    override def initialisePositions() {
        // Do nothing
    }

    val pl1s = Position(1, 2, source)
    val pl1f = Position(3, 4, source)
    val l1 = Leaf(42)
    positions.setStart(l1, pl1s)
    positions.setFinish(l1, pl1f)
    val pl2s = Position(5, 6, source)
    val pl2f = Position(7, 8, source)
    val l2 = Leaf(99)
    positions.setStart(l2, pl2s)
    positions.setFinish(l2, pl2f)

    val pts = Position(9, 10, source)
    val ptf = Position(11, 12, source)
    val t = Two(l1, l2)
    positions.setStart(t, pts)
    positions.setFinish(t, ptf)

    val pos = Position(13, 14, source)
    val pof = Position(15, 16, source)
    val o = One(t)
    positions.setStart(o, pos)
    positions.setFinish(o, pof)

    val r = everywhere(rule[Leaf] {
        case Leaf(i) => Leaf(i + 1)
    })

    def check(no : One) {
        assertResult(One(Two(Leaf(43), Leaf(100))))(no)
        assertOptSame(Some(pos))(positions.getStart(no))
        assertOptSame(Some(pof))(positions.getFinish(no))
        assertOptSame(Some(pts))(positions.getStart(no.a))
        assertOptSame(Some(ptf))(positions.getFinish(no.a))
        assertOptSame(Some(pl1s))(positions.getStart(no.a.asInstanceOf[Two].l))
        assertOptSame(Some(pl1f))(positions.getFinish(no.a.asInstanceOf[Two].l))
        assertOptSame(Some(pl2s))(positions.getStart(no.a.asInstanceOf[Two].r))
        assertOptSame(Some(pl2f))(positions.getFinish(no.a.asInstanceOf[Two].r))
    }

    test("positioned rewriting with positions and strategyf works") {
        val r = everywhere(strategyf {
            case Leaf(i) => Some(Leaf(i + 1))
            case n       => Some(n)
        })
        val no = rewrite(r)(o)
        check(no)
    }

    test("positioned rewriting with positions and strategy works") {
        val r = everywhere(strategy[Leaf] {
            case Leaf(i) => Some(Leaf(i + 1))
        })
        val no = rewrite(r)(o)
        check(no)
    }

    test("positioned rewriting with positions and rule works") {
        val no = rewrite(r)(o)
        check(no)
    }

    test("positioned rewriting with positions and rulefs works") {
        val r = everywhere(rulefs[Leaf] {
            case Leaf(i) => build(Leaf(i + 1))
        })
        val no = rewrite(r)(o)
        check(no)
    }

    test("positioned rewriting with no positions works") {
        val oo = One(Two(Leaf(42), Leaf(99)))
        val noo = rewrite(r)(oo)
        assertResult(None)(positions.getStart(noo))
        assertResult(None)(positions.getStart(noo.a))
        assertResult(None)(positions.getStart(noo.a.asInstanceOf[Two].l))
        assertResult(None)(positions.getStart(noo.a.asInstanceOf[Two].r))
        assertResult(None)(positions.getFinish(noo))
        assertResult(None)(positions.getFinish(noo.a))
        assertResult(None)(positions.getFinish(noo.a.asInstanceOf[Two].l))
        assertResult(None)(positions.getFinish(noo.a.asInstanceOf[Two].r))
    }

}

/**
 * Support for PositionedRewriterTests.  These need to be here rather
 * than in the PositionedRewriterTests class since the latter would
 * require them to be instantiated with an instance of that class.
 */
object SupportPositionedRewriterTests {

    trait Node
    case class One(a : Node) extends Node
    case class Two(l : Node, r : Node) extends Node
    case class Leaf(i : Int) extends Node

}

/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2014 Anthony M Sloane, Macquarie University.
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
package rewriting

import org.kiama.util.Tests

/**
 * Positional rewriting tests.
 */
class PositionalRewriterTests extends Tests {

    import SupportPositionalRewriterTests._
    import org.kiama.rewriting.PositionalRewriter._
    import org.kiama.util.Positions.positionAt
    import scala.util.parsing.input.NoPosition

    val pl1 = positionAt (1, 2)
    val l1 = Leaf (42).setPos (pl1)
    val pl2 = positionAt (3, 4)
    val l2 = Leaf (99).setPos (pl2)

    val pt = positionAt (9, 10)
    val t = Two (l1, l2).setPos (pt)

    val po = positionAt (7, 8)
    val o = One (t).setPos (po)

    val r = everywhere (rule[Leaf] {
                case Leaf (i) => Leaf (i + 1)
            })

    def check (no : One) {
        assertResult (One (Two (Leaf (43), Leaf (100)))) (no)
        assertSame (po) (no.pos)
        assertSame (pt) (no.a.pos)
        assertSame (pl1) (no.a.asInstanceOf[Two].l.pos)
        assertSame (pl2) (no.a.asInstanceOf[Two].r.pos)
    }

    test ("positional rewriting with positions and strategyf works") {
        val r = everywhere (strategyf {
                    case Leaf (i) => Some (Leaf (i + 1))
                    case n        => Some (n)
                })
        val no = rewrite (r) (o)
        check (no)
    }

    test ("positional rewriting with positions and strategy works") {
        val r = everywhere (strategy[Leaf] {
                    case Leaf (i) => Some (Leaf (i + 1))
                })
        val no = rewrite (r) (o)
        check (no)
    }

    test ("positional rewriting with positions and rule works") {
        val no = rewrite (r) (o)
        check (no)
    }

    test ("positional rewriting with positions and rulefs works") {
        val r = everywhere (rulefs[Leaf] {
                    case Leaf (i) => build (Leaf (i + 1))
                })
        val no = rewrite (r) (o)
        check (no)
    }

    test ("positional rewriting with no positions works") {
        val oo = One (Two (Leaf (42), Leaf (99)))
        val noo = rewrite (r) (oo)
        assertResult (One (Two (Leaf (43), Leaf (100)))) (noo)
        assertSame (NoPosition) (noo.pos)
        assertSame (NoPosition) (noo.a.pos)
        assertSame (NoPosition) (noo.a.asInstanceOf[Two].l.pos)
        assertSame (NoPosition) (noo.a.asInstanceOf[Two].r.pos)
    }

}

/**
 * Support for PositionalRewriterTests.  These need to be here rather
 * than in the PositionalRewriterTests class since the latter would
 * require them to be instantiated with an instance of that class.
 */
object SupportPositionalRewriterTests {

    import scala.util.parsing.input.Positional

    trait Node extends Positional
    case class One (a : Node) extends Node
    case class Two (l : Node, r : Node) extends Node
    case class Leaf (i : Int) extends Node

}

/**
 * Positioned rewriting tests.
 */
class PositionedRewriterTests extends Tests {

    import SupportPositionedRewriterTests._
    import org.kiama.rewriting.PositionedRewriter._
    import org.kiama.util.Positions._
    import scala.util.parsing.input.NoPosition

    val pl1s = positionAt (1, 2)
    val pl1f = positionAt (3, 4)
    val l1 = Leaf (42)
    setStart (l1, pl1s)
    setFinish (l1, pl1f)
    val pl2s = positionAt (5, 6)
    val pl2f = positionAt (7, 8)
    val l2 = Leaf (99)
    setStart (l2, pl2s)
    setFinish (l2, pl2f)

    val pts = positionAt (9, 10)
    val ptf = positionAt (11, 12)
    val t = Two (l1, l2)
    setStart (t, pts)
    setFinish (t, ptf)

    val pos = positionAt (13, 14)
    val pof = positionAt (15, 16)
    val o = One (t)
    setStart (o, pos)
    setFinish (o, pof)

    val r = everywhere (rule[Leaf] {
                case Leaf (i) => Leaf (i + 1)
            })

    def check (no : One) {
        assertResult (One (Two (Leaf (43), Leaf (100)))) (no)
        assertSame (pos) (getStart (no))
        assertSame (pof) (getFinish (no))
        assertSame (pts) (getStart (no.a))
        assertSame (ptf) (getFinish (no.a))
        assertSame (pl1s) (getStart (no.a.asInstanceOf[Two].l))
        assertSame (pl1f) (getFinish (no.a.asInstanceOf[Two].l))
        assertSame (pl2s) (getStart (no.a.asInstanceOf[Two].r))
        assertSame (pl2f) (getFinish (no.a.asInstanceOf[Two].r))
    }

    test ("positioned rewriting with positions and strategyf works") {
        val r = everywhere (strategyf {
                    case Leaf (i) => Some (Leaf (i + 1))
                    case n        => Some (n)
                })
        val no = rewrite (r) (o)
        check (no)
    }

    test ("positioned rewriting with positions and strategy works") {
        val r = everywhere (strategy[Leaf] {
                    case Leaf (i) => Some (Leaf (i + 1))
                })
        val no = rewrite (r) (o)
        check (no)
    }

    test ("positioned rewriting with positions and rule works") {
        val no = rewrite (r) (o)
        check (no)
    }

    test ("positioned rewriting with positions and rulefs works") {
        val r = everywhere (rulefs[Leaf] {
                    case Leaf (i) => build (Leaf (i + 1))
                })
        val no = rewrite (r) (o)
        check (no)
    }

    test ("positioned rewriting with no positions works") {
        val oo = One (Two (Leaf (42), Leaf (99)))
        val noo = rewrite (r) (oo)
        assertSame (NoPosition) (getStart (noo))
        assertSame (NoPosition) (getStart (noo.a))
        assertSame (NoPosition) (getStart (noo.a.asInstanceOf[Two].l))
        assertSame (NoPosition) (getStart (noo.a.asInstanceOf[Two].r))
        assertSame (NoPosition) (getFinish (noo))
        assertSame (NoPosition) (getFinish (noo.a))
        assertSame (NoPosition) (getFinish (noo.a.asInstanceOf[Two].l))
        assertSame (NoPosition) (getFinish (noo.a.asInstanceOf[Two].r))
   }

}

/**
 * Support for PositionedRewriterTests.  These need to be here rather
 * than in the PositionedRewriterTests class since the latter would
 * require them to be instantiated with an instance of that class.
 */
object SupportPositionedRewriterTests {

    trait Node
    case class One (a : Node) extends Node
    case class Two (l : Node, r : Node) extends Node
    case class Leaf (i : Int) extends Node

}

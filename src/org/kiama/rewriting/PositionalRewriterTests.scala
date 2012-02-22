/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2012 Anthony M Sloane, Macquarie University.
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
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Positional rewriting tests.
 */
@RunWith(classOf[JUnitRunner])
class PositionalRewriterTests extends Tests {

    import SupportPositionalRewriterTests._
    import org.kiama.rewriting.PositionalRewriter.{test => rwtest, _}
    import scala.util.parsing.input.NoPosition

    val pl1 = new TestPosition { def line = 1; def column = 2 }
    val l1 = Leaf (42).setPos (pl1)
    val pl2 = new TestPosition { def line = 3; def column = 4 }
    val l2 = Leaf (99).setPos (pl2)

    val pt = new TestPosition { def line = 9; def column = 10 }
    val t = Two (l1, l2).setPos (pt)

    val po = new TestPosition { def line = 7; def column = 8 }
    val o = One (t).setPos (po)

    val r = everywhere (rule {
                case Leaf (i) => Leaf (i + 1)
            })

    test ("positional rewriting with positions and strategyf works") {
        val r = everywhere (strategyf {
                    case Leaf (i) => Some (Leaf (i + 1))
                    case n        => Some (n)
                })
        val no = rewrite (r) (o)
        expect (One (Two (Leaf (43), Leaf (100)))) (no)
        expectsame (po) (no.pos)
        expectsame (pt) (no.a.pos)
        expectsame (pl1) (no.a.asInstanceOf[Two].l.pos)
        expectsame (pl2) (no.a.asInstanceOf[Two].r.pos)
    }

    test ("positional rewriting with positions and strategy works") {
        val r = everywhere (strategy {
                    case Leaf (i) => Some (Leaf (i + 1))
                })
        val no = rewrite (r) (o)
        expect (One (Two (Leaf (43), Leaf (100)))) (no)
        expectsame (po) (no.pos)
        expectsame (pt) (no.a.pos)
        expectsame (pl1) (no.a.asInstanceOf[Two].l.pos)
        expectsame (pl2) (no.a.asInstanceOf[Two].r.pos)
    }

    test ("positional rewriting with positions and rule works") {
        val no = rewrite (r) (o)
        expect (One (Two (Leaf (43), Leaf (100)))) (no)
        expectsame (po) (no.pos)
        expectsame (pt) (no.a.pos)
        expectsame (pl1) (no.a.asInstanceOf[Two].l.pos)
        expectsame (pl2) (no.a.asInstanceOf[Two].r.pos)
    }

    test ("positional rewriting with positions and rulefs works") {
        val r = everywhere (rulefs {
                    case Leaf (i) => build (Leaf (i + 1))
                })
        val no = rewrite (r) (o)
        expect (One (Two (Leaf (43), Leaf (100)))) (no)
        expectsame (po) (no.pos)
        expectsame (pt) (no.a.pos)
        expectsame (pl1) (no.a.asInstanceOf[Two].l.pos)
        expectsame (pl2) (no.a.asInstanceOf[Two].r.pos)
    }

    test ("positional rewriting with no positions works") {
        val oo = One (Two (Leaf (42), Leaf (99)))
        val noo = rewrite (r) (oo)
        expect (One (Two (Leaf (43), Leaf (100)))) (noo)
        expectsame (NoPosition) (noo.pos)
        expectsame (NoPosition) (noo.a.pos)
        expectsame (NoPosition) (noo.a.asInstanceOf[Two].l.pos)
        expectsame (NoPosition) (noo.a.asInstanceOf[Two].r.pos)
    }

}

/**
 * Support for PositionalRewriterTests.  These need to be here rather
 * than in the PositionalRewriterTests class since the latter would
 * require them to be instantiated with an instance of that class.
 */
object SupportPositionalRewriterTests {

    import scala.util.parsing.input.{Positional, Position}

    trait Node extends Positional
    case class One (a : Node) extends Node
    case class Two (l : Node, r : Node) extends Node
    case class Leaf (i : Int) extends Node

    trait TestPosition extends Position {
        def lineContents = ""
    }

}

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
import org.scalatest.junit.JUnitRunner

/**
 * Positional rewriting tests.
 */
class PositionalRewriterTests extends Tests {

    import SupportPositionalRewriterTests._
    import org.kiama.rewriting.PositionalRewriter.{test => rwtest, _}
    import scala.util.parsing.input.NoPosition

    val pl1 = new TestPosition { val line = 1; val column = 2 }
    val l1 = Leaf (42).setPos (pl1)
    val pl2 = new TestPosition { val line = 3; val column = 4 }
    val l2 = Leaf (99).setPos (pl2)

    val pt = new TestPosition { val line = 9; val column = 10 }
    val t = Two (l1, l2).setPos (pt)

    val po = new TestPosition { val line = 7; val column = 8 }
    val o = One (t).setPos (po)

    val r = everywhere (rule {
                case Leaf (i) => Leaf (i + 1)
            })

    def check (no : One) {
        expectResult (One (Two (Leaf (43), Leaf (100)))) (no)
        expectsame (po) (no.pos)
        expectsame (pt) (no.a.pos)
        expectsame (pl1) (no.a.asInstanceOf[Two].l.pos)
        expectsame (pl2) (no.a.asInstanceOf[Two].r.pos)
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
        val r = everywhere (strategy {
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
        val r = everywhere (rulefs {
                    case Leaf (i) => build (Leaf (i + 1))
                })
        val no = rewrite (r) (o)
        check (no)
    }

    test ("positional rewriting with no positions works") {
        val oo = One (Two (Leaf (42), Leaf (99)))
        val noo = rewrite (r) (oo)
        expectResult (One (Two (Leaf (43), Leaf (100)))) (noo)
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
        val lineContents = ""
    }

}

/**
 * Positioned rewriting tests.
 */
class PositionedRewriterTests extends Tests {

    import SupportPositionedRewriterTests._
    import org.kiama.rewriting.PositionedRewriter.{test => rwtest, _}
    import scala.util.parsing.input.NoPosition

    val pl1s = new TestPosition { val line = 1; val column = 2 }
    val pl1f = new TestPosition { val line = 3; val column = 4 }
    val l1 = Leaf (42).setStart (pl1s).setFinish (pl1f)
    val pl2s = new TestPosition { val line = 5; val column = 6 }
    val pl2f = new TestPosition { val line = 7; val column = 8 }
    val l2 = Leaf (99).setStart (pl2s).setFinish (pl2f)

    val pts = new TestPosition { val line = 9; val column = 10 }
    val ptf = new TestPosition { val line = 11; val column = 12 }
    val t = Two (l1, l2).setStart (pts).setFinish (ptf)

    val pos = new TestPosition { val line = 13; val column = 14 }
    val pof = new TestPosition { val line = 15; val column = 16 }
    val o = One (t).setStart (pos).setFinish (pof)

    val r = everywhere (rule {
                case Leaf (i) => Leaf (i + 1)
            })

    def check (no : One) {
        expectResult (One (Two (Leaf (43), Leaf (100)))) (no)
        expectsame (pos) (no.start)
        expectsame (pof) (no.finish)
        expectsame (pts) (no.a.start)
        expectsame (ptf) (no.a.finish)
        expectsame (pl1s) (no.a.asInstanceOf[Two].l.start)
        expectsame (pl1f) (no.a.asInstanceOf[Two].l.finish)
        expectsame (pl2s) (no.a.asInstanceOf[Two].r.start)
        expectsame (pl2f) (no.a.asInstanceOf[Two].r.finish)
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
        val r = everywhere (strategy {
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
        val r = everywhere (rulefs {
                    case Leaf (i) => build (Leaf (i + 1))
                })
        val no = rewrite (r) (o)
        check (no)
    }

    test ("positioned rewriting with no positions works") {
        val oo = One (Two (Leaf (42), Leaf (99)))
        val noo = rewrite (r) (oo)
        expectsame (NoPosition) (noo.start)
        expectsame (NoPosition) (noo.a.start)
        expectsame (NoPosition) (noo.a.asInstanceOf[Two].l.start)
        expectsame (NoPosition) (noo.a.asInstanceOf[Two].r.start)
        expectsame (NoPosition) (noo.finish)
        expectsame (NoPosition) (noo.a.finish)
        expectsame (NoPosition) (noo.a.asInstanceOf[Two].l.finish)
        expectsame (NoPosition) (noo.a.asInstanceOf[Two].r.finish)
   }

}

/**
 * Support for PositionedRewriterTests.  These need to be here rather
 * than in the PositionedRewriterTests class since the latter would
 * require them to be instantiated with an instance of that class.
 */
object SupportPositionedRewriterTests {

    import org.kiama.util.Positioned
    import scala.util.parsing.input.Position

    trait Node extends Positioned
    case class One (a : Node) extends Node
    case class Two (l : Node, r : Node) extends Node
    case class Leaf (i : Int) extends Node

    trait TestPosition extends Position {
        val lineContents = ""
    }

}

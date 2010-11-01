/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2010 Anthony M Sloane, Macquarie University.
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
package attribution

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 * Tests of basic attribution.
 */
@RunWith(classOf[JUnitRunner])
class AttributionTests extends FunSuite {

    abstract class Tree extends Attributable
    case class Pair (left : Tree, right : Tree) extends Tree
    case class Leaf (value : Int) extends Tree

    test ("cached attributes are only evaluated once") {
        import Attribution._

        var count = 0

        lazy val maximum : Tree ==> Int =
            attr {
                case Pair (l,r) => count = count + 1; (l->maximum).max (r->maximum)
                case Leaf (v)   => v
            }

        val t = Pair (Leaf (3), Pair (Leaf (1), Leaf (10)))

        expect (10, "first value") (t->maximum)
        expect (10, "second value") (t->maximum)
        expect (2, "evaluation count") (count)
    }

    test ("cached attributes are distinct for nodes that are equal") {
        import Attribution._

        var count = 0

        lazy val maximum : Tree ==> Int =
            attr {
                case Pair (l,r) => count = count + 1; (l->maximum).max (r->maximum)
                case Leaf (v)   => v
            }

        val t = Pair (Leaf (3), Pair (Leaf (1), Leaf (10)))
        val s = Pair (Leaf (3), Pair (Leaf (1), Leaf (10)))

        expect (10, "first value") (t->maximum)
        expect (10, "second value") (s->maximum)
        expect (4, "evaluation count") (count)
    }

    test ("uncached attributes are evaluated each time") {
        import UncachedAttribution._

        var count = 0

        lazy val maximum : Tree ==> Int =
            attr {
                case Pair (l,r) => count = count + 1; (l->maximum).max (r->maximum)
                case Leaf (v)   => v
            }

        val t = Pair (Leaf (3), Pair (Leaf (1), Leaf (10)))

        expect (10, "first value") (t->maximum)
        expect (10, "second value") (t->maximum)
        expect (4, "evaluation count") (count)
    }

    test ("circularities are detected for cached attributes") {
        import Attribution._

        lazy val direct : Tree ==> Int =
            attr {
                case t => t->direct
            }
        lazy val indirect : Tree ==> Int =
            attr {
                case t => t->indirect2
            }
        lazy val indirect2 : Tree ==> Int =
            attr {
                case t => t->indirect
            }

        val t = Pair (Leaf (3), Pair (Leaf (1), Leaf (10)))

        try {
            t->direct
            fail ("direct circular computation finished without exception")
        } catch {
            case e : IllegalStateException =>
                // succeed
        }
        try {
            t->indirect
            fail ("indirect circular computation finished without exception")
        } catch {
            case e : IllegalStateException =>
                // succeed
        }
    }

    test ("circularities are detected for uncached attributes") {
        import UncachedAttribution._

        lazy val direct : Tree ==> Int =
            attr {
                case t => t->direct
            }
        lazy val indirect : Tree ==> Int =
            attr {
                case t => t->indirect2
            }
        lazy val indirect2 : Tree ==> Int =
            attr {
                case t => t->indirect
            }

        val t = Pair (Leaf (3), Pair (Leaf (1), Leaf (10)))

        try {
            t->direct
            fail ("direct circular computation finished without exception")
        } catch {
            case e : IllegalStateException =>
                // succeed
        }
        try {
            t->indirect
            fail ("indirect circular computation finished without exception")
        } catch {
            case e : IllegalStateException =>
                // succeed
        }
    }

}

/**
 * Tests of collection attributes.
 */
@RunWith(classOf[JUnitRunner])
class CollectionAttributionTests extends FunSuite
                                 with org.kiama.example.lambda2.Parser {

    import Attribution._
    import org.kiama.example.lambda2.AST._
    import org.kiama.example.lambda2.Analysis._

    def process (s : String, r : Set[(Int,Int)]) {
        parseAll (start, s) match {
            case Success (e : Lam, in) if in.atEnd =>
                expect (r, "uses for " + e) (e->uses)
            case Success (e, _) =>
                fail ("non-Lam " + e + " parsed in test input '" + s + "'")
            case _ =>
                fail ("can't parse test input '" + s + "'")
        }
    }

//    test ("collection attribute: no collected node") {
//
//
//    }

//    test ("collection attribute: single collected node") {
//        process ("""\x : Int . x""", Set ((1,12)))
//    }

//    test ("collection attribute: multiple collected nodes") {
//        process ("""\\x : Int . x + (\\y : Int . x + y) 5""")
//    }

}

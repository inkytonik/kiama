/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2013 Anthony M Sloane, Macquarie University.
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

import org.kiama.util.Tests

/**
 * Tests of dynamic attribution.
 */
class DynamicAttributionTests extends Tests {

    import Attribution._

    abstract class Tree extends Attributable
    case class Pair (left : Tree, right : Tree) extends Tree
    case class Leaf (value : Int) extends Tree
    case class Unused (b : Boolean) extends Tree

    var count = 0

    lazy val sumleafDef : Tree ==> Int =
        {
            case Leaf (v) => count = count + 1; v
            case _        => -1
        }

    before {
        count = 0
    }

    test ("dynamic attribution base works on Leafs") {
        val sumleaf = dynAttr (sumleafDef)
        assertResult (2) (sumleaf (Leaf (2)))
    }

    test ("dynamic attribution base defaults on Pairs") {
        val sumleaf = dynAttr (sumleafDef)
        assertResult (-1) (sumleaf (Pair (Leaf (1), Leaf (2))))
    }

    test ("dynamic attribute are re-evaluated when reset") {
        val sumleaf = dynAttr (sumleafDef)
        val t = Leaf (2)

        assertResult (false, "hasBeenComputedAt") (sumleaf.hasBeenComputedAt (t))
        assertResult (2) (sumleaf (t))
        assertResult (true, "hasBeenComputedAt") (sumleaf.hasBeenComputedAt (t))
        assertResult (2) (sumleaf (t))
        assertResult (1, "evaluation count") (count)
        assertResult (true, "hasBeenComputedAt") (sumleaf.hasBeenComputedAt (t))
        sumleaf.reset ()
        assertResult (false, "hasBeenComputedAt") (sumleaf.hasBeenComputedAt (t))
        assertResult (2) (sumleaf (t))
        assertResult (true, "hasBeenComputedAt") (sumleaf.hasBeenComputedAt (t))
        assertResult (2, "evaluation count") (count)
    }

    test ("dynamic attribute can be extended and reduced manually") {
        val sumleaf = dynAttr (sumleafDef)
        val newcase : Tree ==> Int =
            {
                case Leaf (88)   => 77
                case Pair (l, r) => (l->sumleaf) + (r->sumleaf)
            }
        val func : Tree ==> Int =
            {
                case Pair (l, r) => 99
            }

        // No modification
        assertResult (2) (sumleaf (Leaf (2)))
        assertResult (-1) (sumleaf (Pair (Leaf (1), Leaf (2))))

        // Add a partial function and take away again
        sumleaf += newcase
        assertResult (4) (sumleaf (Leaf (4)))
        assertResult (8) (sumleaf (Pair (Leaf (3), Leaf (5))))
        assertResult (154) (sumleaf (Pair (Leaf (88), Leaf (88))))
        sumleaf -= newcase
        assertResult (6) (sumleaf (Leaf (6)))
        assertResult (-1) (sumleaf (Pair (Leaf (1), Leaf (2))))

        // Add another partial function and take away again
        sumleaf += func
        assertResult (6) (sumleaf (Leaf (6)))
        assertResult (99) (sumleaf (Pair (Leaf (1), Leaf (2))))
        sumleaf -= func
        assertResult (6) (sumleaf (Leaf (6)))
        assertResult (-1) (sumleaf (Pair (Leaf (1), Leaf (2))))

        // Multiple additions and out of order removal
        sumleaf += newcase
        sumleaf += func
        assertResult (6) (sumleaf (Leaf (6)))
        assertResult (99) (sumleaf (Pair (Leaf (1), Leaf (2))))
        assertResult (77) (sumleaf (Leaf (88)))
        sumleaf -= newcase
        assertResult (6) (sumleaf (Leaf (6)))
        assertResult (99) (sumleaf (Pair (Leaf (1), Leaf (2))))
        assertResult (88) (sumleaf (Leaf (88)))
        sumleaf -= func
        assertResult (6) (sumleaf (Leaf (6)))
        assertResult (-1) (sumleaf (Pair (Leaf (1), Leaf (2))))
        assertResult (88) (sumleaf (Leaf (88)))
    }

    test ("dynamic attribute can be extended and reduced with a using operation") {
        val sumleaf = dynAttr (sumleafDef)

        assertResult (2) (sumleaf (Leaf (2)))
        assertResult (-1) (sumleaf (Pair (Leaf (1), Leaf (2))))

        sumleaf.block {

            sumleaf +=
                {
                    case Pair (l, r) => (l->sumleaf) + (r->sumleaf)
                }

            assertResult (4) (sumleaf (Leaf (4)))
            assertResult (8) (sumleaf (Pair (Leaf (3), Leaf (5))))

            sumleaf.block {

                sumleaf +=
                    {
                        case Pair (l, r) => 42
                    }

                assertResult (4) (sumleaf (Leaf (4)))
                assertResult (42) (sumleaf (Pair (Leaf (3), Leaf (5))))

            }

            assertResult (4) (sumleaf (Leaf (4)))
            assertResult (9) (sumleaf (Pair (Leaf (3), Leaf (6))))

        }

        assertResult (6) (sumleaf (Leaf (6)))
        assertResult (-1) (sumleaf (Pair (Leaf (1), Leaf (2))))
    }

    test ("using a dynamic attribute outside its domain raises an exception") {

        val sumleafDef : Tree ==> Int =
            {
                case Leaf (v) => v
            }
        val sumleaf = dynAttr (sumleafDef)

        val i = intercept[MatchError] {
                    sumleaf (Pair (Leaf (1), Leaf (2)))
                }
        assertResult ("Pair(Leaf(1),Leaf(2)) (of class org.kiama.attribution.DynamicAttributionTests$Pair)") (
            i.getMessage
        )

        sumleaf.block {
            sumleaf +=
                {
                    case Pair (Leaf (1), Leaf (2)) => 100
                }

            assertResult (100) (sumleaf (Pair (Leaf (1), Leaf (2))))
            val i = intercept[MatchError] {
                        sumleaf (Pair (Leaf (3), Leaf (1)))
                    }
            assertResult ("Pair(Leaf(3),Leaf(1)) (of class org.kiama.attribution.DynamicAttributionTests$Pair)") (
                i.getMessage
            )
        }

    }

    test ("circularities are detected for dynamic attributes") {
        lazy val direct : Tree => Int =
            dynAttr (t => t->direct)
        lazy val indirect : Tree => Int =
            dynAttr (t => t->indirect2)
        lazy val indirect2 : Tree => Int =
            dynAttr (t => t->indirect)

        val t = Pair (Leaf (3), Pair (Leaf (1), Leaf (10)))

        val i1 = intercept[IllegalStateException] {
                    t->direct
                }
        assertResult ("Cycle detected in attribute evaluation 'direct' at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i1.getMessage)

        val i2 = intercept[IllegalStateException] {
                     t->indirect
                 }
        assertResult ("Cycle detected in attribute evaluation 'indirect' at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i2.getMessage)

        val i3 = intercept[IllegalStateException] {
                     t->indirect2
                 }
        assertResult ("Cycle detected in attribute evaluation 'indirect2' at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i3.getMessage)
    }

}

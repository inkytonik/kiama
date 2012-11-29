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
package attribution

import org.kiama.util.Tests
import org.scalatest.junit.JUnitRunner

/**
 * Tests of dynamic attribution.
 */
class DynamicAttributionTests extends Tests {

    import DynamicAttribution._

    abstract class Tree extends Attributable
    case class Pair (left : Tree, right : Tree) extends Tree
    case class Leaf (value : Int) extends Tree
    case class Unused (b : Boolean) extends Tree

    val sumleafbase : Tree ==> Int =
        attr {
            case Leaf (v) => v
            case _        => -1
        }

    test ("dynamic attribution base works on Leafs") {
        expectResult (2) (sumleafbase (Leaf (2)))
    }

    test ("dynamic attribution base defaults on Pairs") {
        expectResult (-1) (sumleafbase (Pair (Leaf (1), Leaf (2))))
    }

    test ("dynamic attributes are defined where they should be") {
        lazy val maximum : Tree ==> Int =
            attr {
                case Pair (l,r) => 0
                case Leaf (v)   => 0
            }

        expectResult (true, "isDefinedAt Leaf") (maximum.isDefinedAt (Leaf (1)))
        expectResult (true, "isDefinedAt Pair") (maximum.isDefinedAt (Pair (Leaf (1), Leaf (2))))
        expectResult (false, "isDefinedAt Unused") (maximum.isDefinedAt (Unused (false)))
    }

    test ("dynamic attribute are re-evaluated when reset") {

        var count = 0

        val sumleaf : Tree ==> Int =
            attr {
                case Leaf (v) => count = count + 1; v
                case _        => -1
            }

        val t = Leaf (2)

        expectResult (2) (sumleaf (t))
        expectResult (2) (sumleaf (t))
        expectResult (1, "evaluation count") (count)
        sumleaf.asInstanceOf[DynamicAttribute[Tree,Int]].reset ()
        expectResult (2) (sumleaf (Leaf (2)))
        expectResult (2, "evaluation count") (count)

    }

    test ("dynamic attribute can be extended and reduced manually") {

        val sumleaf : Tree ==> Int =
            attr {
                case Leaf (v) => v
                case _        => -1
            }

        val newcase : Tree ==> Int =
            attr {
                case Leaf (88)   => 77
                case Pair (l, r) => (l->sumleaf) + (r->sumleaf)
            }

        val func : Tree ==> Int =
            {
                case Pair (l, r) => 99
            }

        // No modification
        expectResult (2) (sumleaf (Leaf (2)))
        expectResult (-1) (sumleaf (Pair (Leaf (1), Leaf (2))))

        // Add a dynamic attribute and take away again
        sumleaf += newcase
        expectResult (4) (sumleaf (Leaf (4)))
        expectResult (8) (sumleaf (Pair (Leaf (3), Leaf (5))))
        expectResult (154) (sumleaf (Pair (Leaf (88), Leaf (88))))
        sumleaf -= newcase
        expectResult (6) (sumleaf (Leaf (6)))
        expectResult (-1) (sumleaf (Pair (Leaf (1), Leaf (2))))

        // Add a partial function and take away again
        sumleaf += func
        expectResult (6) (sumleaf (Leaf (6)))
        expectResult (99) (sumleaf (Pair (Leaf (1), Leaf (2))))
        sumleaf -= func
        expectResult (6) (sumleaf (Leaf (6)))
        expectResult (-1) (sumleaf (Pair (Leaf (1), Leaf (2))))

        // Multiple additions and out of order removal
        sumleaf += newcase
        sumleaf += func
        expectResult (6) (sumleaf (Leaf (6)))
        expectResult (99) (sumleaf (Pair (Leaf (1), Leaf (2))))
        expectResult (77) (sumleaf (Leaf (88)))
        sumleaf -= newcase
        expectResult (6) (sumleaf (Leaf (6)))
        expectResult (99) (sumleaf (Pair (Leaf (1), Leaf (2))))
        expectResult (88) (sumleaf (Leaf (88)))
        sumleaf -= func
        expectResult (6) (sumleaf (Leaf (6)))
        expectResult (-1) (sumleaf (Pair (Leaf (1), Leaf (2))))
        expectResult (88) (sumleaf (Leaf (88)))

    }

    test ("can't extend partial function as dynamic attribute") {

        val sumleaf : Tree ==> Int =
            attr {
                case Leaf (v) => v
                case _        => -1
            }

        val func : Tree ==> Int =
            {
                case Pair (l, r) => 99
            }

        val i = intercept[UnsupportedOperationException] {
                    func += sumleaf
                }
        expectResult ("Can only add partial functions to existing attributes") (i.getMessage)

    }

    test ("dynamic attribute can be extended and reduced with a using operation") {

        val sumleaf : Tree ==> Int =
            attr {
                case Leaf (v) => v
                case _        => -1
            }

        object ExtensionOne {
            sumleaf +=
                attr {
                    case Pair (l, r) => (l->sumleaf) + (r->sumleaf)
                }
        }

        object ExtensionTwo {
            sumleaf +=
                attr {
                    case Pair (l, r) => 42
                }
        }

        expectResult (2) (sumleaf (Leaf (2)))
        expectResult (-1) (sumleaf (Pair (Leaf (1), Leaf (2))))

        using (ExtensionOne) {
            expectResult (4) (sumleaf (Leaf (4)))
            expectResult (8) (sumleaf (Pair (Leaf (3), Leaf (5))))

            using (ExtensionTwo) {
                expectResult (4) (sumleaf (Leaf (4)))
                expectResult (42) (sumleaf (Pair (Leaf (3), Leaf (5))))
            }

            expectResult (4) (sumleaf (Leaf (4)))
            expectResult (9) (sumleaf (Pair (Leaf (3), Leaf (6))))
        }

        expectResult (6) (sumleaf (Leaf (6)))
        expectResult (-1) (sumleaf (Pair (Leaf (1), Leaf (2))))

    }

    test ("using a dynamic attribute outside its domain raises an exception") {

        val sumleaf : Tree ==> Int =
            attr {
                case Leaf (v) => v
            }

        val i = intercept[MatchError] {
                    sumleaf (Pair (Leaf (1), Leaf (2)))
                }
        expectResult ("Pair(Leaf(1),Leaf(2)) (of class org.kiama.attribution.DynamicAttributionTests$Pair)") (
            i.getMessage
        )

        object Extension {
            sumleaf +=
                attr {
                    case Pair (Leaf (1), Leaf (2)) => 100
                }
        }

        using (Extension) {
            expectResult (100) (sumleaf (Pair (Leaf (1), Leaf (2))))
            val i = intercept[MatchError] {
                        sumleaf (Pair (Leaf (3), Leaf (1)))
                    }
            expectResult ("Pair(Leaf(3),Leaf(1)) (of class org.kiama.attribution.DynamicAttributionTests$Pair)") (
                i.getMessage
            )
        }

    }

    test ("circularities are detected for dynamic attributes") {
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

        val i1 = intercept[IllegalStateException] {
                    t->direct
                }
        expectResult ("Cycle detected in attribute evaluation at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i1.getMessage)

        val i2 = intercept[IllegalStateException] {
                     t->indirect
                 }
        expectResult ("Cycle detected in attribute evaluation at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i2.getMessage)
    }

}

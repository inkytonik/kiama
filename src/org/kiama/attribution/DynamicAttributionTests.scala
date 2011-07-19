/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011 Anthony M Sloane, Macquarie University.
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
import org.kiama.util.Tests
import org.scalatest.junit.JUnitRunner

/**
 * Tests of dynamic attribution.
 */
@RunWith(classOf[JUnitRunner])
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
        expect (2) (sumleafbase (Leaf (2)))
    }
        
    test ("dynamic attribution base defaults on Pairs") {
        expect (-1) (sumleafbase (Pair (Leaf (1), Leaf (2))))
    }

    test ("dynamic attributes are defined where they should be") {
        lazy val maximum : Tree ==> Int =
            attr {
                case Pair (l,r) => 0
                case Leaf (v)   => 0
            }
            
        expect (true, "isDefinedAt Leaf") (maximum.isDefinedAt (Leaf (1)))
        expect (true, "isDefinedAt Pair") (maximum.isDefinedAt (Pair (Leaf (1), Leaf (2))))
        expect (false, "isDefinedAt Unused") (maximum.isDefinedAt (Unused (false)))
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
        expect (2) (sumleaf (Leaf (2)))
        expect (-1) (sumleaf (Pair (Leaf (1), Leaf (2))))
        
        // Add a dynamic attribute and take away again
        sumleaf += newcase
        expect (4) (sumleaf (Leaf (4)))
        expect (8) (sumleaf (Pair (Leaf (3), Leaf (5))))
        expect (154) (sumleaf (Pair (Leaf (88), Leaf (88))))
        sumleaf -= newcase
        expect (6) (sumleaf (Leaf (6)))
        expect (-1) (sumleaf (Pair (Leaf (1), Leaf (2))))
        
        // Add a partial function and take away again
        sumleaf += func
        expect (6) (sumleaf (Leaf (6)))
        expect (99) (sumleaf (Pair (Leaf (1), Leaf (2))))
        sumleaf -= func
        expect (6) (sumleaf (Leaf (6)))
        expect (-1) (sumleaf (Pair (Leaf (1), Leaf (2))))
        
        // Multiple additions and out of order removal
        sumleaf += newcase
        sumleaf += func
        expect (6) (sumleaf (Leaf (6)))
        expect (99) (sumleaf (Pair (Leaf (1), Leaf (2))))
        expect (77) (sumleaf (Leaf (88)))
        sumleaf -= newcase
        expect (6) (sumleaf (Leaf (6)))
        expect (99) (sumleaf (Pair (Leaf (1), Leaf (2))))
        expect (88) (sumleaf (Leaf (88)))
        sumleaf -= func
        expect (6) (sumleaf (Leaf (6)))
        expect (-1) (sumleaf (Pair (Leaf (1), Leaf (2))))
        expect (88) (sumleaf (Leaf (88)))

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
        expect ("Can only add partial functions to existing attributes") (i.getMessage)

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

        expect (2) (sumleaf (Leaf (2)))
        expect (-1) (sumleaf (Pair (Leaf (1), Leaf (2))))
        
        using (ExtensionOne) {
            expect (4) (sumleaf (Leaf (4)))
            expect (8) (sumleaf (Pair (Leaf (3), Leaf (5))))
            
            using (ExtensionTwo) {
                expect (4) (sumleaf (Leaf (4)))
                expect (42) (sumleaf (Pair (Leaf (3), Leaf (5))))
            }

            expect (4) (sumleaf (Leaf (4)))
            expect (9) (sumleaf (Pair (Leaf (3), Leaf (6))))
        }
        
        expect (6) (sumleaf (Leaf (6)))
        expect (-1) (sumleaf (Pair (Leaf (1), Leaf (2))))

    }
    
    test ("using a dynamic attribute outside its domain raises an exception") {

        val sumleaf : Tree ==> Int =
            attr {
                case Leaf (v) => v
            }

        val i = intercept[MatchError] {
                    sumleaf (Pair (Leaf (1), Leaf (2)))
                }
        expect ("Pair(Leaf(1),Leaf(2)) (of class org.kiama.attribution.DynamicAttributionTests$Pair)") (
            i.getMessage
        )
        
        object Extension {
            sumleaf += 
                attr {
                    case Pair (Leaf (1), Leaf (2)) => 100
                }
        }

        using (Extension) {
            expect (100) (sumleaf (Pair (Leaf (1), Leaf (2))))
            val i = intercept[MatchError] {
                        sumleaf (Pair (Leaf (3), Leaf (1)))
                    }
            expect ("Pair(Leaf(3),Leaf(1)) (of class org.kiama.attribution.DynamicAttributionTests$Pair)") (
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
        expect ("Cycle detected in attribute evaluation at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i1.getMessage)

        val i2 = intercept[IllegalStateException] {
                     t->indirect
                 }
        expect ("Cycle detected in attribute evaluation at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i2.getMessage)
    }

}

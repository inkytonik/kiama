/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package attribution

import org.bitbucket.inkytonik.kiama.util.KiamaTests

/**
 * Tests of dynamic attribution.
 */
class DynamicAttributionTests extends Attribution with KiamaTests {

    abstract class TestNode
    case class Pair(left : TestNode, right : TestNode) extends TestNode
    case class Leaf(value : Int) extends TestNode
    case class Unused(b : Boolean) extends TestNode

    /**
     * Definitions of the attributes that will be tested below. We package
     * them in a class so that each test can have its own instance of the
     * attributes so that there is no shared state.
     */
    class Definitions {

        var count = 0

        lazy val sumleaf : TestNode => Int =
            dynAttr {
                case Leaf(v) =>
                    count = count + 1; v
                case _ => -1
            }

    }

    test("dynamic attribution base works on Leafs") {
        val definitions = new Definitions
        import definitions._
        sumleaf(Leaf(2)) shouldBe 2
    }

    test("dynamic attribution base defaults on Pairs") {
        val definitions = new Definitions
        import definitions._
        sumleaf(Pair(Leaf(1), Leaf(2))) shouldBe -1
    }

    test("dynamic attribute are re-evaluated when reset") {
        val definitions = new Definitions
        import definitions._

        val t = Leaf(2)

        sumleaf.hasBeenComputedAt(t) shouldBe false
        sumleaf(t) shouldBe 2
        sumleaf.hasBeenComputedAt(t) shouldBe true
        sumleaf(t) shouldBe 2
        count shouldBe 1
        sumleaf.hasBeenComputedAt(t) shouldBe true
        sumleaf.reset()
        sumleaf.hasBeenComputedAt(t) shouldBe false
        sumleaf(t) shouldBe 2
        sumleaf.hasBeenComputedAt(t) shouldBe true
        count shouldBe 2
    }

    test("dynamic attribute can be extended and reduced manually") {
        val definitions = new Definitions
        import definitions._

        val newcase : TestNode ==> Int =
            {
                case Leaf(88)   => 77
                case Pair(l, r) => sumleaf(l) + sumleaf(r)
            }
        val func : TestNode ==> Int =
            {
                case Pair(l, r) => 99
            }

        // No modification
        sumleaf(Leaf(2)) shouldBe 2
        sumleaf(Pair(Leaf(1), Leaf(2))) shouldBe -1

        // Add a partial function and take away again
        sumleaf += newcase
        sumleaf(Leaf(4)) shouldBe 4
        sumleaf(Pair(Leaf(3), Leaf(5))) shouldBe 8
        sumleaf(Pair(Leaf(88), Leaf(88))) shouldBe 154
        sumleaf -= newcase
        sumleaf(Leaf(6)) shouldBe 6
        sumleaf(Pair(Leaf(1), Leaf(2))) shouldBe -1

        // Add another partial function and take away again
        sumleaf += func
        sumleaf(Leaf(6)) shouldBe 6
        sumleaf(Pair(Leaf(1), Leaf(2))) shouldBe 99
        sumleaf -= func
        sumleaf(Leaf(6)) shouldBe 6
        sumleaf(Pair(Leaf(1), Leaf(2))) shouldBe -1

        // Multiple additions and out of order removal
        sumleaf += newcase
        sumleaf += func
        sumleaf(Leaf(6)) shouldBe 6
        sumleaf(Pair(Leaf(1), Leaf(2))) shouldBe 99
        sumleaf(Leaf(88)) shouldBe 77
        sumleaf -= newcase
        sumleaf(Leaf(6)) shouldBe 6
        sumleaf(Pair(Leaf(1), Leaf(2))) shouldBe 99
        sumleaf(Leaf(88)) shouldBe 88
        sumleaf -= func
        sumleaf(Leaf(6)) shouldBe 6
        sumleaf(Pair(Leaf(1), Leaf(2))) shouldBe -1
        sumleaf(Leaf(88)) shouldBe 88
    }

    test("dynamic attribute can be extended and reduced with a using operation") {
        val definitions = new Definitions
        import definitions._

        sumleaf(Leaf(2)) shouldBe 2
        sumleaf(Pair(Leaf(1), Leaf(2))) shouldBe -1

        sumleaf.block {

            sumleaf +=
                {
                    case Pair(l, r) => sumleaf(l) + sumleaf(r)
                }

            sumleaf(Leaf(4)) shouldBe 4
            sumleaf(Pair(Leaf(3), Leaf(5))) shouldBe 8

            sumleaf.block {

                sumleaf +=
                    {
                        case Pair(l, r) => 42
                    }

                sumleaf(Leaf(4)) shouldBe 4
                sumleaf(Pair(Leaf(3), Leaf(5))) shouldBe 42

            }

            sumleaf(Leaf(4)) shouldBe 4
            sumleaf(Pair(Leaf(3), Leaf(6))) shouldBe 9

        }

        sumleaf(Leaf(6)) shouldBe 6
        sumleaf(Pair(Leaf(1), Leaf(2))) shouldBe -1
    }

    test("using a dynamic attribute outside its domain raises an exception") {

        val sumleafDef : TestNode ==> Int =
            {
                case Leaf(v) => v
            }
        val sumleaf = dynAttr(sumleafDef)

        val i = intercept[MatchError] {
            sumleaf(Pair(Leaf(1), Leaf(2)))
        }
        i.getMessage shouldBe s"Pair(Leaf(1),Leaf(2)) (of class org.bitbucket.inkytonik.kiama.attribution.DynamicAttributionTests$$Pair)"

        sumleaf.block {
            sumleaf +=
                {
                    case Pair(Leaf(1), Leaf(2)) => 100
                }

            sumleaf(Pair(Leaf(1), Leaf(2))) shouldBe 100
            val i = intercept[MatchError] {
                sumleaf(Pair(Leaf(3), Leaf(1)))
            }
            i.getMessage shouldBe s"Pair(Leaf(3),Leaf(1)) (of class org.bitbucket.inkytonik.kiama.attribution.DynamicAttributionTests$$Pair)"
        }

    }

    test("circularities are detected for dynamic attributes") {
        lazy val direct : TestNode => Int =
            dynAttr(t => direct(t))
        lazy val indirect : TestNode => Int =
            dynAttr(t => indirect2(t))
        lazy val indirect2 : TestNode => Int =
            dynAttr(t => indirect(t))

        val t = Pair(Leaf(3), Pair(Leaf(1), Leaf(10)))

        val i1 = intercept[IllegalStateException] {
            direct(t)
        }
        i1.getMessage shouldBe "Cycle detected in attribute evaluation 'direct' at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))"

        val i2 = intercept[IllegalStateException] {
            indirect(t)
        }
        i2.getMessage shouldBe "Cycle detected in attribute evaluation 'indirect' at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))"

        val i3 = intercept[IllegalStateException] {
            indirect2(t)
        }
        i3.getMessage shouldBe "Cycle detected in attribute evaluation 'indirect2' at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))"
    }

}

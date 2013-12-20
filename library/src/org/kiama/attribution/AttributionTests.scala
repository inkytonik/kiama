/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2013 Anthony M Sloane, Macquarie University.
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
 * Tests of basic attribution.
 */
class AttributionTests extends Tests {

    import Attribution._
    import Decorators._
    import org.kiama.util.Tree
    import scala.collection.GenSeq

    abstract class TestTree extends Tree
    case class Pair (left : TestTree, right : TestTree) extends TestTree
    case class Leaf (value : Int) extends TestTree
    case class Unused (b : Boolean) extends TestTree
    case class EitherTree (e : Either[Pair,Leaf]) extends TestTree
    case class ListTree (l : List[TestTree]) extends TestTree
    case class SetTree (s : Set[TestTree]) extends TestTree
    case class GenSeqTree (v : GenSeq[TestTree]) extends TestTree
    case class MapTree (m : Map[TestTree,TestTree]) extends TestTree
    case class PairTree (p : (TestTree,TestTree)) extends TestTree
    case class TripleTree (p : (TestTree,TestTree,TestTree)) extends TestTree
    case class QuadTree (p : (TestTree,TestTree,TestTree,TestTree)) extends TestTree

    val l = Leaf (3)
    val s = Pair (Leaf (3), Pair (Leaf (1), Leaf (10)))
    val t = Pair (Leaf (3), Pair (Leaf (1), Leaf (10)))
    val u = Pair (Leaf (1), Leaf (2))
    val v = Pair (Leaf (3), Pair (Leaf (1), Leaf (3)))

    /**
     * Definitions of the attributes that will be tested below. We package
     * them in a class so that each test can have its own instance of the
     * attributes so that there is no shared state.
     */
    class Definitions {

        var count = 0

        lazy val incDef : Int => Int =
            {
                case i => count = count + 1; i + 1
            }

        lazy val inc =
            attr (incDef)

        lazy val concatDef : Int => Int => Int =
            {
                case 1 => {
                    case i => count = count + 1; i + 1
                }
                case 2 => {
                    case 0 => count = count + 1; 999
                    case i => count = count + 1; i + 2
                }
                case n => {
                    case i => count = count + 1; i * i
                }
            }

        lazy val concat =
            paramAttr (concatDef)

        lazy val maximumDef : TestTree => Int =
            {
                case Pair (l,r) => count = count + 1; (l->maximum).max (r->maximum)
                case Leaf (v)   => v
            }

        lazy val maximum =
            attr (maximumDef)

        lazy val leafComputedDef : TestTree => Boolean =
            {
                case t @ Leaf (v) =>
                    leafComputed.hasBeenComputedAt (t)
            }

        lazy val leafComputed =
            attr (leafComputedDef)

        lazy val cattrDef : TestTree => Attributable => Int =
            {
                case Pair (l, r) => {
                    case Pair (l, r) => 0
                    case Leaf (v)    => 1
                    case _           => 2
                }
                case Leaf (v) => {
                    case Pair (l, r) => 3
                    case Leaf (v)    => 4
                    case _           => 5
                }
                case _ => {
                    case _ => 6
                }
            }

        lazy val pattrDef : String => Attributable => Int =
            {
                case "hello" => {
                    case Pair (l, r) => count = count + 1; 0
                    case Leaf (v)    => 1
                    case _           => 2
                }
                case "goodbye" => {
                    case _ => 3
                }
            }

        lazy val answer : TestTree => Int =
            constant { count = count + 1; 42 }

    }

    test ("attributes of a value type are correctly evaluated") {
        val definitions = new Definitions
        import definitions._
        assertResult (false, "hasBeenComputedAt") (inc.hasBeenComputedAt (1))
        assertResult (false, "hasBeenComputedAt") (inc.hasBeenComputedAt (2))
        assertResult (0, "evaluation count") (count)
        assertResult (2, "first inc of 1") (inc (1))
        assertResult (1, "evaluation count") (count)
        assertResult (true, "hasBeenComputedAt") (inc.hasBeenComputedAt (1))
        assertResult (false, "hasBeenComputedAt") (inc.hasBeenComputedAt (2))
        assertResult (3, "first inc of 2") (inc (2))
        assertResult (2, "evaluation count") (count)
        assertResult (true, "hasBeenComputedAt") (inc.hasBeenComputedAt (1))
        assertResult (true, "hasBeenComputedAt") (inc.hasBeenComputedAt (2))
        assertResult (2, "second inc of 1") (inc (1))
        assertResult (3, "second inc of 2") (inc (2))
        assertResult (2, "evaluation count") (count)
    }

    test ("parameterised attributes of a value type are correctly evaluated") {
        val definitions = new Definitions
        import definitions._

        assertResult (false, "hasBeenComputedAt") (concat.hasBeenComputedAt (1, 0))
        assertResult (false, "hasBeenComputedAt") (concat.hasBeenComputedAt (2, 0))
        assertResult (false, "hasBeenComputedAt") (concat.hasBeenComputedAt (3, 0))
        assertResult (false, "hasBeenComputedAt") (concat.hasBeenComputedAt (4, 0))
        assertResult (false, "hasBeenComputedAt") (concat.hasBeenComputedAt (1, 1))
        assertResult (false, "hasBeenComputedAt") (concat.hasBeenComputedAt (2, 1))
        assertResult (false, "hasBeenComputedAt") (concat.hasBeenComputedAt (3, 1))
        assertResult (false, "hasBeenComputedAt") (concat.hasBeenComputedAt (4, 1))
        assertResult (false, "hasBeenComputedAt") (concat.hasBeenComputedAt (1, 2))
        assertResult (false, "hasBeenComputedAt") (concat.hasBeenComputedAt (2, 2))
        assertResult (false, "hasBeenComputedAt") (concat.hasBeenComputedAt (3, 2))
        assertResult (false, "hasBeenComputedAt") (concat.hasBeenComputedAt (4, 2))
        assertResult (0, "evaluation count") (count)

        assertResult (1) (concat (1) (0))
        assertResult (999) (concat (2) (0))
        assertResult (0) (concat (3) (0))
        assertResult (2) (concat (1) (1))
        assertResult (3) (concat (2) (1))
        assertResult (1) (concat (3) (1))
        assertResult (3) (concat (1) (2))
        assertResult (4) (concat (2) (2))
        assertResult (4) (concat (3) (2))

        assertResult (true, "hasBeenComputedAt") (concat.hasBeenComputedAt (1, 0))
        assertResult (true, "hasBeenComputedAt") (concat.hasBeenComputedAt (2, 0))
        assertResult (true, "hasBeenComputedAt") (concat.hasBeenComputedAt (3, 0))
        assertResult (false, "hasBeenComputedAt") (concat.hasBeenComputedAt (4, 0))
        assertResult (true, "hasBeenComputedAt") (concat.hasBeenComputedAt (1, 1))
        assertResult (true, "hasBeenComputedAt") (concat.hasBeenComputedAt (2, 1))
        assertResult (true, "hasBeenComputedAt") (concat.hasBeenComputedAt (3, 1))
        assertResult (false, "hasBeenComputedAt") (concat.hasBeenComputedAt (4, 1))
        assertResult (true, "hasBeenComputedAt") (concat.hasBeenComputedAt (1, 2))
        assertResult (true, "hasBeenComputedAt") (concat.hasBeenComputedAt (2, 2))
        assertResult (true, "hasBeenComputedAt") (concat.hasBeenComputedAt (3, 2))
        assertResult (false, "hasBeenComputedAt") (concat.hasBeenComputedAt (4, 2))
        assertResult (9, "evaluation count") (count)

        assertResult (1) (concat (1) (0))
        assertResult (999) (concat (2) (0))
        assertResult (0) (concat (3) (0))
        assertResult (2) (concat (1) (1))
        assertResult (3) (concat (2) (1))
        assertResult (1) (concat (3) (1))
        assertResult (3) (concat (1) (2))
        assertResult (4) (concat (2) (2))
        assertResult (4) (concat (3) (2))
        assertResult (9, "evaluation count") (count)

    }

    test ("cached attributes are correctly evaluated") {
        val definitions = new Definitions
        import definitions._
        assertResult (false, "hasBeenComputedAt") (maximum.hasBeenComputedAt (t))
        assertResult (10, "first value") (t->maximum)
        assertResult (true, "hasBeenComputedAt") (maximum.hasBeenComputedAt (t))
        assertResult (10, "second value") (t->maximum)
        assertResult (true, "hasBeenComputedAt") (maximum.hasBeenComputedAt (t))
        assertResult (2, "evaluation count") (count)
    }

    test ("resetMemo resets the hasBeenComputedAt state") {
        val definitions = new Definitions
        import definitions._
        assertResult (false, "hasBeenComputedAt") (maximum.hasBeenComputedAt (t))
        t->maximum
        assertResult (true, "hasBeenComputedAt") (maximum.hasBeenComputedAt (t))
        resetMemo ()
        assertResult (false, "hasBeenComputedAt") (maximum.hasBeenComputedAt (t))
    }

    test ("hasBeenComputedAt returns false while an attribute is being evaluated") {
        val definitions = new Definitions
        import definitions._
        assertResult (false, "hasBeenComputedAt during") (l->leafComputed)
        assertResult (true, "hasBeenComputedAt after") (leafComputed.hasBeenComputedAt (l))
    }

    test ("constant attributes are only evaluated once") {
        val definitions = new Definitions
        import definitions._
        assertResult (42, "first value") (t->answer)
        assertResult (42, "second value") (t->answer)
        assertResult (1, "evaluation count") (count)
    }

    test ("cached attributes are re-evaluated after a reset") {
        val definitions = new Definitions
        import definitions._
        assertResult (10, "first value") (t->maximum)
        assertResult (10, "first value") (t->maximum)
        assertResult (2, "evaluation count") (count)
        maximum.reset ()
        assertResult (10, "second value") (t->maximum)
        assertResult (4, "evaluation count") (count)
    }

    test ("cached attributes are distinct for nodes that are equal") {
        val definitions = new Definitions
        import definitions._
        assertResult (10, "first value") (t->maximum)
        assertResult (10, "second value") (s->maximum)
        assertResult (4, "evaluation count") (count)
    }

    test ("cached attributes can be reset") {
        val definitions = new Definitions
        import definitions._
        assertResult (10, "first value") (t->maximum)
        resetMemo
        assertResult (10, "second value") (t->maximum)
        assertResult (4, "evaluation count") (count)
    }

    test ("uncached attributes are evaluated each time") {
        val definitions = new Definitions
        import definitions._
        import UncachedAttribution._

        lazy val maximum : TestTree => Int =
            attr {
                case Pair (l,r) => count = count + 1; (l->maximum).max (r->maximum)
                case Leaf (v)   => v
            }

        assertResult (10, "first value") (t->maximum)
        assertResult (10, "second value") (t->maximum)
        assertResult (4, "evaluation count") (count)
    }

    test ("cached child attributes work") {
        val definitions = new Definitions
        import definitions._

        lazy val cattr =
            childAttr (cattrDef)

        val f = Leaf (4)
        val e = Leaf (3)
        val d = Leaf (2)
        val c = Leaf (1)
        val b = Pair (d, e)
        val a = Pair (b, c)
        initTree (a)

        assertResult (0, "cached childAttr Pair Pair") (cattr (b))
        assertResult (2, "cached childAttr Pair top") (cattr (a))
        assertResult (3, "cached childAttr Leaf Pair") (cattr (c))
        assertResult (5, "cached childAttr Leaf top") (cattr (f))
    }

    test ("uncached child attributes work") {
        val definitions = new Definitions
        import definitions._
        import UncachedAttribution._

        lazy val cattr : TestTree => Int =
            childAttr (cattrDef)

        val f = Leaf (4)
        val e = Leaf (3)
        val d = Leaf (2)
        val c = Leaf (1)
        val b = Pair (d, e)
        val a = Pair (b, c)
        initTree (a)

        assertResult (0, "uncached childAttr Pair Pair") (cattr (b))
        assertResult (2, "uncached childAttr Pair top") (cattr (a))
        assertResult (3, "uncached childAttr Leaf Pair") (cattr (c))
        assertResult (5, "uncached childAttr Leaf top") (cattr (f))
    }

    test ("cached parameterised attributes work") {
        val definitions = new Definitions
        import definitions._

        lazy val pattr =
            paramAttr (pattrDef)

        assertResult (0, "cached paramAttr Pair hello") (
            pattr ("hello") (Pair (Leaf (1), Leaf (2)))
        )
        assertResult (3, "cached paramAttr Pair goodbye") (
            pattr ("goodbye") (Pair (Leaf (1), Leaf (2)))
        )
        assertResult (1, "cached paramAttr Leaf hello") (pattr ("hello") (Leaf (1)))
        assertResult (3, "cached paramAttr Leaf goodbye") (pattr ("goodbye") (Leaf (1)))
    }

    test ("cached parameterised attributes are re-evaluated after reset") {
        val definitions = new Definitions
        import definitions._

        lazy val pattr =
            paramAttr (pattrDef)

        assertResult (false, "hasBeenComputedAt") (pattr.hasBeenComputedAt ("hello", u))
        assertResult (0, "cached paramAttr Pair hello") (pattr ("hello") (u))
        assertResult (true, "hasBeenComputedAt") (pattr.hasBeenComputedAt ("hello", u))
        assertResult (0, "cached paramAttr Pair hello") (pattr ("hello") (u))
        assertResult (1, "evaluation count") (count)
        assertResult (true, "hasBeenComputedAt") (pattr.hasBeenComputedAt ("hello", u))
        pattr.reset ()
        assertResult (false, "hasBeenComputedAt") (pattr.hasBeenComputedAt ("hello", u))
        assertResult (0, "cached paramAttr Pair hello") (pattr ("hello") (u))
        assertResult (2, "evaluation count") (count)
        assertResult (true, "hasBeenComputedAt") (pattr.hasBeenComputedAt ("hello", u))
    }

    test ("uncached parameterised attributes work") {
        val definitions = new Definitions
        import definitions._
        import UncachedAttribution._

        lazy val pattr =
            paramAttr (pattrDef)

        assertResult (0, "uncached paramAttr Pair hello") (
            pattr ("hello") (Pair (Leaf (1), Leaf (2)))
        )
        assertResult (3, "uncached paramAttr Pair goodbye") (
            pattr ("goodbye") (Pair (Leaf (1), Leaf (2)))
        )
        assertResult (1, "uncached paramAttr Leaf hello") (pattr ("hello") (Leaf (1)))
        assertResult (3, "uncached paramAttr Leaf goodbye") (pattr ("goodbye") (Leaf (1)))
    }

    test ("circularities are detected for cached attributes") {
        lazy val direct : TestTree => Int =
            attr (t => t->direct)
        lazy val indirect : TestTree => Int =
            attr (t => t->indirect2)
        lazy val indirect2 : TestTree => Int =
            attr (t => t->indirect)

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

    test ("circularities are detected for uncached attributes") {
        import UncachedAttribution._

        lazy val direct : TestTree => Int =
            attr (t => t->direct)
        lazy val indirect : TestTree => Int =
            attr (t => t->indirect2)
        lazy val indirect2 : TestTree => Int =
            attr (t => t->indirect)

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

    test ("circularities are detected for parameterised attributes") {
        lazy val direct : Int => Tree => Int =
            paramAttr (i => (t => t->direct (i)))
        lazy val indirect : Int => Tree => Int =
            paramAttr (i => (t => t->indirect2 (i)))
        lazy val indirect2 : Int => Tree => Int =
            paramAttr (i => (t => t->indirect (i)))

        val t = Pair (Leaf (3), Pair (Leaf (1), Leaf (10)))

        val i1 = intercept[IllegalStateException] {
                     t->direct (1)
                 }
        assertResult ("Cycle detected in attribute evaluation 'direct' (1) at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i1.getMessage)

        val i2 = intercept[IllegalStateException] {
                     t->indirect (8)
                 }
        assertResult ("Cycle detected in attribute evaluation 'indirect' (8) at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i2.getMessage)

        val i3 = intercept[IllegalStateException] {
                     t->indirect2 (9)
                 }
        assertResult ("Cycle detected in attribute evaluation 'indirect2' (9) at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i3.getMessage)
    }

    test ("parameterised attribute keys compare correctly") {
        val n = Leaf (1)
        val k1 = new ParamAttributeKey ("hello", n)
        val k2 = new ParamAttributeKey ("hello", n)
        val k3 = new ParamAttributeKey ("hello", Leaf (1))
        val k4 = new ParamAttributeKey ("goodbye", n)
        val k5 = new ParamAttributeKey ("goodbye", Leaf (1))
        val k6 = new ParamAttributeKey ("hello", null)
        val k7 = new ParamAttributeKey ("hello", null)
        val k8 = new ParamAttributeKey ("goodbye", null)
        assertResult (false) (n equals k1)
        assertResult (false) (k1 equals n)
        assertResult (true) (k1 equals k2)
        assertResult (true) (k2 equals k1)
        assertResult (false) (k1 equals k3)
        assertResult (false) (k3 equals k1)
        assertResult (false) (k1 equals k4)
        assertResult (false) (k4 equals k1)
        assertResult (false) (k1 equals k5)
        assertResult (false) (k5 equals k1)
        assertResult (false) (k1 equals k6)
        assertResult (false) (k6 equals k1)
        assertResult (false) (k1 equals k7)
        assertResult (false) (k7 equals k1)
        assertResult (false) (k1 equals k8)
        assertResult (false) (k8 equals k1)
        assertResult (true) (k6 equals k7)
        assertResult (true) (k7 equals k6)
        assertResult (false) (k6 equals k8)
        assertResult (false) (k8 equals k6)
    }

    test ("a normal child's properties are set correctly") {
        val c1 = Leaf (3)
        val c2 = Leaf (1)
        val c3 = Leaf (10)
        val c4 = Pair (c2, c3)
        val t = Pair (c1, c4)
        for (i <- 1 to 2) {

            initTree (t)

            val tchildren = t.children.toSeq
            assertResult (2) (tchildren.length)
            assertSame (c1) (tchildren (0))
            assertSame (c4) (tchildren (1))
            assertSame (c1) (t.firstChild)
            assertResult (true) (t.hasChildren)
            assertResult (-1) (t.index)
            assertResult (true) (t.isFirst)
            assertResult (true) (t.isLast)
            assertResult (true) (t.isRoot)
            assertSame (c4) (t.lastChild)
            assertResult (null) (t.next)
            assertResult (null) (t.parent)
            assertResult (null) (t.prev)

            val c1children = c1.children.toSeq
            assertResult (0) (c1children.length)
            assertResult (false) (c1.hasChildren)
            assertResult (0) (c1.index)
            assertResult (true) (c1.isFirst)
            assertResult (false) (c1.isLast)
            assertResult (false) (c1.isRoot)
            assertSame (c4) (c1.next)
            assertSame (t) (c1.parent)
            assertResult (null) (c1.prev)

            val c2children = c2.children.toSeq
            assertResult (0) (c2children.length)
            assertResult (false) (c2.hasChildren)
            assertResult (0) (c2.index)
            assertResult (true) (c2.isFirst)
            assertResult (false) (c2.isLast)
            assertResult (false) (c2.isRoot)
            assertSame (c3) (c2.next)
            assertSame (c4) (c2.parent)
            assertResult (null) (c2.prev)

            val c3children = c3.children.toSeq
            assertResult (0) (c3children.length)
            assertResult (false) (c3.hasChildren)
            assertResult (1) (c3.index)
            assertResult (false) (c3.isFirst)
            assertResult (true) (c3.isLast)
            assertResult (false) (c3.isRoot)
            assertResult (null) (c3.next)
            assertSame (c4) (c3.parent)
            assertSame (c2) (c3.prev)

            val c4children = c4.children.toSeq
            assertResult (2) (c4children.length)
            assertSame (c2) (c4children (0))
            assertSame (c3) (c4children (1))
            assertSame (c2) (c4.firstChild)
            assertResult (true) (c4.hasChildren)
            assertResult (1) (c4.index)
            assertResult (false) (c4.isFirst)
            assertResult (true) (c4.isLast)
            assertResult (false) (c4.isRoot)
            assertSame (c3) (c4.lastChild)
            assertResult (null) (c4.next)
            assertSame (t) (c4.parent)
            assertSame (c1) (c4.prev)

        }
    }

    test ("an either child's parent property is set correctly") {
        val c1 = Leaf (3)
        val c2 = Leaf (1)
        val c3 = Pair (c1, c2)
        val t1 = EitherTree (Left (c3))
        val c4 = Leaf (6)
        val t2 = EitherTree (Right (c4))
        initTree (t1)
        assertSame (null) (t1.parent)
        assertSame (t1) (c3.parent)
        assertSame (c3) (c1.parent)
        assertSame (c3) (c2.parent)
        initTree (t2)
        assertSame (null) (t2.parent)
        assertSame (t2) (c4.parent)
    }

    test ("a list child's parent property is set correctly") {
        val c1 = Leaf (3)
        val c2 = Leaf (1)
        val c3 = Leaf (10)
        val c4 = ListTree (List (c2, c3))
        val t = Pair (c1, c4)
        initTree (t)
        assertSame (null) (t.parent)
        assertSame (t) (c1.parent)
        assertSame (t) (c4.parent)
        assertSame (c4) (c2.parent)
        assertSame (c4) (c3.parent)
    }

    test ("a set child's parent property is set correctly") {
        val c1 = Leaf (3)
        val c2 = Leaf (1)
        val c3 = Leaf (10)
        val c4 = SetTree (Set (c2, c3))
        val t = Pair (c1, c4)
        initTree (t)
        assertSame (null) (t.parent)
        assertSame (t) (c1.parent)
        assertSame (t) (c4.parent)
        assertSame (c4) (c2.parent)
        assertSame (c4) (c3.parent)
    }

    test ("a sequential vector child's parent property is set correctly") {
        val c1 = Leaf (3)
        val c2 = Leaf (1)
        val c3 = Leaf (10)
        val c4 = GenSeqTree (Vector (c2, c3))
        val t = Pair (c1, c4)
        initTree (t)
        assertSame (null) (t.parent)
        assertSame (t) (c1.parent)
        assertSame (t) (c4.parent)
        assertSame (c4) (c2.parent)
        assertSame (c4) (c3.parent)
    }

    test ("a parallel vector child's parent property is set correctly") {
        val c1 = Leaf (3)
        val c2 = Leaf (1)
        val c3 = Leaf (10)
        val c4 = GenSeqTree (Vector (c2, c3).par)
        val t = Pair (c1, c4)
        initTree (t)
        assertSame (null) (t.parent)
        assertSame (t) (c1.parent)
        assertSame (t) (c4.parent)
        assertSame (c4) (c2.parent)
        assertSame (c4) (c3.parent)
    }

    test ("a map's tuple parent properties are set correctly") {
        val c1 = Leaf (3)
        val c2 = Leaf (1)
        val c3 = Leaf (10)
        val c4 = Leaf (11)
        val c5 = Leaf (12)
        val c6 = MapTree (Map (c4 -> c5))
        val t = MapTree (Map (c1 -> c2, c3 -> c6))
        initTree (t)
        assertSame (null) (t.parent)
        assertSame (t) (c1.parent)
        assertSame (t) (c2.parent)
        assertSame (t) (c3.parent)
        assertSame (t) (c6.parent)
        assertSame (c6) (c4.parent)
        assertSame (c6) (c5.parent)
    }

    test ("a pair's component parent properties are set correctly") {
        val c1 = Leaf (3)
        val c2 = Leaf (1)
        val c3 = Leaf (10)
        val c4 = PairTree (c2, c3)
        val t = PairTree (c1, c4)
        initTree (t)
        assertSame (null) (t.parent)
        assertSame (t) (c1.parent)
        assertSame (t) (c4.parent)
        assertSame (c4) (c2.parent)
        assertSame (c4) (c3.parent)
    }

    test ("a triple's component parent properties are set correctly") {
        val c1 = Leaf (3)
        val c2 = Leaf (1)
        val c3 = Leaf (10)
        val c4 = Leaf (11)
        val c5 = TripleTree (c2, c3, c4)
        val t = PairTree (c5, c1)
        initTree (t)
        assertSame (null) (t.parent)
        assertSame (t) (c1.parent)
        assertSame (t) (c5.parent)
        assertSame (c5) (c2.parent)
        assertSame (c5) (c4.parent)
        assertSame (c5) (c4.parent)
    }

    test ("a quad's component parent properties are set correctly") {
        val c1 = Leaf (3)
        val c2 = Leaf (1)
        val c3 = Leaf (10)
        val c4 = Leaf (11)
        val c5 = Leaf (12)
        val c6 = QuadTree (c2, c3, c4, c5)
        val t = PairTree (c1, c6)
        initTree (t)
        assertSame (null) (t.parent)
        assertSame (t) (c1.parent)
        assertSame (t) (c6.parent)
        assertSame (c6) (c2.parent)
        assertSame (c6) (c3.parent)
        assertSame (c6) (c4.parent)
        assertSame (c6) (c5.parent)
    }

    test ("a collectl that collects the Ints from Leafs works and is cached") {
        var count = 0
        val collectNum = collectl { case Leaf (i) => count = count + 1; i }
        assertResult (false) (collectNum.hasBeenComputedAt (s))
        assertResult (0) (count)
        assertResult (List (3, 1, 10)) (collectNum (s))
        assertResult (true) (collectNum.hasBeenComputedAt (s))
        assertResult (3) (count)
        assertResult (List (3, 1, 10)) (collectNum (s))
        assertResult (true) (collectNum.hasBeenComputedAt (s))
        assertResult (3) (count)
    }

    test ("a collects that collects the Ints from Leafs works and is cached") {
        var count = 0
        val collectNum = collects { case Leaf (i) => count = count + 1; i }
        assertResult (false) (collectNum.hasBeenComputedAt (v))
        assertResult (0) (count)
        assertResult (Set (3, 1)) (collectNum (v))
        assertResult (true) (collectNum.hasBeenComputedAt (v))
        assertResult (3) (count)
        assertResult (Set (1, 3)) (collectNum (v))
        assertResult (true) (collectNum.hasBeenComputedAt (v))
        assertResult (3) (count)
    }

    {
        val t = Pair (Leaf (3), Pair (l, Leaf (10)))
        initTree (t)

        test ("a down attribute that is defined returns the computed value") {
            val dattr = down[TestTree,Int] (99) { case _ : Pair => 42 }
            assertResult (42) (l->dattr)
        }

        test ("a down attribute that is not defined returns the default value") {
            val dattr = down[TestTree,Int] (99) { case _ : Unused => 42 }
            assertResult (99) (l->dattr)
        }

        test ("a downErr attribute that is defined returns the computed value") {
            val dattr = downErr[TestTree,Int] { case _ : Pair => 42 }
            assertResult (42) (l->dattr)
        }

        test ("a downErr attribute that is not defined throws an error") {
            val dattr = downErr[TestTree,Int] { case _ : Unused => 42 }
            val i = intercept[RuntimeException] {
                        l->dattr
                    }
            assertResult ("downErr: function is not defined on path to root") (i.getMessage)
        }

        test ("a downOpt attribute that is defined returns Some of the computed value") {
            val dattr = downOpt[TestTree,Int] { case _ : Pair => 42 }
            assertResult (Some (42)) (l->dattr)
        }

        test ("a downOpt attribute that is not defined returns None") {
            val dattr = downOpt[TestTree,Int] { case _ : Unused => 42 }
            assertResult (None) (l->dattr)
        }

    }

    test ("a chain that is only defined at the root returns the root value") {
        val t = Pair (Leaf (3), Pair (Leaf (1), Leaf (10)))
        initTree (t)
        def rootupd (in : TestTree => Int) : TestTree ==> Int = {
            case n if n.isRoot => 42
        }
        val rootchain = chain (rootupd)
        assertResult (42) (t->(rootchain.in))
        assertResult (42) (t->(rootchain.out))
    }

    test ("a chain with no updates throws appropriate exceptions") {
        val t = Pair (Leaf (3), Pair (Leaf (1), Leaf (10)))
        initTree (t)

        // A chain with only identiy update functions
        val idchain = chain[Tree,Int] ()
        val i1 = intercept[RuntimeException] {
                     t->(idchain.in)
                 }
        assertResult ("chain root of tree reached at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i1.getMessage)
        val i2 = intercept[RuntimeException] {
                     t->(idchain.out)
                 }
        assertResult ("chain root of tree reached at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i2.getMessage)

        // A chain with refusing-all-in update function. This exercises a
        // different path in the 'in' attribute to the previous checks.
        def refuse (in : TestTree => Int) : TestTree ==> Int =
            new (TestTree ==> Int) {
                def apply (t : TestTree) : Int = in (t) // Never used
                def isDefinedAt (t : TestTree) : Boolean = false
            }
        val refchain = chain (refuse)
        val i3 = intercept[RuntimeException] {
                     t->(refchain.in)
                 }
        assertResult ("chain root of tree reached at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i3.getMessage)
        val i4 = intercept[RuntimeException] {
                     t->(refchain.out)
                 }
        assertResult ("chain root of tree reached at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i4.getMessage)

    }

    test ("deep cloning a term with sharing gives an equal but not eq term") {
        import AttributableSupport.deepclone
        import org.kiama.example.imperative.ImperativeTree._

        val c = Add (Num (1), Num (2))
        val d = Add (Num (1), Num (2))
        val e = Add (Num (3), Num (4))
        val t = Add (Mul (c,
                          Sub (c,
                               d)),
                     Add (Add (e,
                               Num (5)),
                          e))
        val u = Add (Mul (Add (Num (1), Num (2)),
                          Sub (Add (Num (1), Num (2)),
                               d)),
                     Add (Add (Add (Num (3), Num (4)),
                               Num (5)),
                          Add (Num (3), Num (4))))

        initTree (t)
        val ct = deepclone (t)

        // Must get the right answer (==)
        assertResult (u) (ct)

        // Must not get the original term (eq)
        assertNotSame (t) (ct)

        // Make sure that the parents proerpties are set correctly
        // (for the top level)
        def isTree (ast : Attributable) : Boolean =
            ast.children.forall (c => (c.parent eq ast) && isTree (c))
        assert (isTree (ct.asInstanceOf[Attributable]),
                "deep cloned tree has invalid parent properties")

        // Check the terms at the positions of the two c occurrences
        // against each other, since they are eq to start but should
        // not be after
        val mul = ct.asInstanceOf[Add].l.asInstanceOf[Mul]
        val c1 = mul.l
        val mulsub = mul.r.asInstanceOf[Sub]
        val c2 = mulsub.l
        assertNotSame (c1) (c2)

        // Check the terms at the positions of the two c ocurrences
        // against the one at the position of the d occurrence (which
        // is == but not eq to the two original c's)
        val d1 = mulsub.r
        assertNotSame (c1) (d1)
        assertNotSame (c2) (d1)
    }

    test ("a circular attribute that never changes evaluates to initial value") {
        import org.kiama.example.imperative.ImperativeTree.Num

        lazy val zero : CircularAttribute[Num,Double] =
            circular (0.0) (_ => 0)

        val n = Num (1)
        assertResult (false) (zero.hasBeenComputedAt (n))
        assertResult (0) (zero (n))
        assertResult (true) (zero.hasBeenComputedAt (n))
    }

    test ("two circular attributes that never change from initial value do converge") {

        import org.kiama.example.imperative.ImperativeTree.Num

        lazy val counter : CircularAttribute[Num,Double] =
            circular (0.0) (
                (n : Num) =>
                    pass (n)
            )

        lazy val pass : CircularAttribute[Num,Double] =
            circular (0.0) (
                (n : Num) =>
                    counter (n)
            )

        val n = Num (1)
        assertResult (false) (counter.hasBeenComputedAt (n))
        assertResult (false) (pass.hasBeenComputedAt (n))
        assertResult (0.0) (counter (n))
        assertResult (true) (counter.hasBeenComputedAt (n))
        assertResult (true) (pass.hasBeenComputedAt (n))
        assertResult (0.0) (pass (n))
        assertResult (true) (counter.hasBeenComputedAt (n))
        assertResult (true) (pass.hasBeenComputedAt (n))
    }

    test ("a directly circular attribute can count") {
        import org.kiama.example.imperative.ImperativeTree.Num

        lazy val counter : CircularAttribute[Num,Double] =
            circular (0.0) (
                (n : Num) => {
                    val current = counter (n)
                    current + (if (current < 10) n.d else 0)
                }
            )

        val n = Num (1)
        assertResult (false) (counter.hasBeenComputedAt (n))
        assertResult (10.0) (counter (n))
        assertResult (true) (counter.hasBeenComputedAt (n))
    }

    test ("a cycle of two circular attributes can count") {
        import org.kiama.example.imperative.ImperativeTree.Num

        lazy val counter : Num => Double =
            circular (0.0) (
                (n : Num) => {
                    val current = pass (n)
                    current + (if (current < 10) n.d else 0)
                }
            )

        lazy val pass : Num => Double =
            circular (0.0) (
                (n : Num) =>
                    counter (n)
            )

        val n = Num (1)
        assertResult (10.0) (counter (n))
        assertResult (10.0) (pass (n))
    }

    test ("a cycle of three circular attributes can count") {
        import org.kiama.example.imperative.ImperativeTree.Num

        lazy val counter : Num => Double =
            circular (0.0) (
                (n : Num) => {
                    val current = double (n)
                    current + (if (current < 10) n.d else 0)
                }
            )

        lazy val double : Num => Double =
            circular (0.0) (
                (n : Num) => {
                    val current = pass (n)
                    if (current < 10) current * 2 else current
                }
            )

        lazy val pass : Num => Double =
            circular (0.0) (
                (n : Num) =>
                    counter (n)
            )

        val n = Num (1)
        assertResult (14.0) (counter (n))
        assertResult (14.0) (double (n))
        assertResult (14.0) (pass (n))
    }

    test ("a single circular attribute plus a cycle of two circular attributes can count") {
        import org.kiama.example.imperative.ImperativeTree.Num

        lazy val entry : Num => Double =
            circular (0.0) (
                (n : Num) =>
                    counter (n)
            )

        lazy val counter : Num => Double =
            circular (0.0) (
                (n : Num) => {
                    val current = double (n)
                    current + (if (current < 10) n.d else 0)
                }
            )

        lazy val double : Num => Double =
            circular (0.0) (
                (n : Num) => {
                    val current = counter (n)
                    if (current < 10) current * 2 else current
                }
            )

        val n = Num (1)
        assertResult (14.0) (entry (n))
        assertResult (14.0) (counter (n))
        assertResult (14.0) (double (n))
    }

    test ("a single circular attribute plus a cycle of two trivial circular attributes converges") {
        import org.kiama.example.imperative.ImperativeTree.Num

        lazy val entry : Num => Double =
            circular (0.0) (
                (n : Num) =>
                    pass1 (n)
            )

        lazy val pass1 : Num => Double =
            circular (0.0) (
                (n : Num) =>
                    pass2 (n)
            )

        lazy val pass2 : Num => Double =
            circular (0.0) (
                (n : Num) =>
                    pass1 (n)
            )

        val n = Num (1)
        assertResult (0.0) (entry (n))
        assertResult (0.0) (pass1 (n))
        assertResult (0.0) (pass2 (n))
    }

}


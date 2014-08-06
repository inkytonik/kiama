/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2014 Anthony M Sloane, Macquarie University.
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
    import org.kiama.relation.Tree
    import scala.collection.GenSeq

    abstract class TestTree extends Product
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
                case Pair (l,r) => count = count + 1; maximum (l).max (maximum (r))
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

        lazy val pattrDef : String => TestTree => Int =
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
        assertResult (10, "first value") (maximum (t))
        assertResult (true, "hasBeenComputedAt") (maximum.hasBeenComputedAt (t))
        assertResult (10, "second value") (maximum (t))
        assertResult (true, "hasBeenComputedAt") (maximum.hasBeenComputedAt (t))
        assertResult (2, "evaluation count") (count)
    }

    test ("resetMemo resets the hasBeenComputedAt state") {
        val definitions = new Definitions
        import definitions._
        assertResult (false, "hasBeenComputedAt") (maximum.hasBeenComputedAt (t))
        maximum (t)
        assertResult (true, "hasBeenComputedAt") (maximum.hasBeenComputedAt (t))
        resetMemo ()
        assertResult (false, "hasBeenComputedAt") (maximum.hasBeenComputedAt (t))
    }

    test ("hasBeenComputedAt returns false while an attribute is being evaluated") {
        val definitions = new Definitions
        import definitions._
        assertResult (false, "hasBeenComputedAt during") (leafComputed (l))
        assertResult (true, "hasBeenComputedAt after") (leafComputed.hasBeenComputedAt (l))
    }

    test ("constant attributes are only evaluated once") {
        val definitions = new Definitions
        import definitions._
        assertResult (42, "first value") (answer (t))
        assertResult (42, "second value") (answer (t))
        assertResult (1, "evaluation count") (count)
    }

    test ("cached attributes are re-evaluated after a reset") {
        val definitions = new Definitions
        import definitions._
        assertResult (10, "first value") (maximum (t))
        assertResult (10, "first value") (maximum (t))
        assertResult (2, "evaluation count") (count)
        maximum.reset ()
        assertResult (10, "second value") (maximum (t))
        assertResult (4, "evaluation count") (count)
    }

    test ("cached attributes are distinct for nodes that are equal") {
        val definitions = new Definitions
        import definitions._
        assertResult (10, "first value") (maximum (t))
        assertResult (10, "second value") (maximum (s))
        assertResult (4, "evaluation count") (count)
    }

    test ("cached attributes can be reset") {
        val definitions = new Definitions
        import definitions._
        assertResult (10, "first value") (maximum (t))
        resetMemo
        assertResult (10, "second value") (maximum (t))
        assertResult (4, "evaluation count") (count)
    }

    test ("uncached attributes are evaluated each time") {
        val definitions = new Definitions
        import definitions._
        import UncachedAttribution._

        lazy val maximum : TestTree => Int =
            attr {
                case Pair (l,r) => count = count + 1; maximum (l).max (maximum (r))
                case Leaf (v)   => v
            }

        assertResult (10, "first value") (maximum (t))
        assertResult (10, "second value") (maximum (t))
        assertResult (4, "evaluation count") (count)
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
            attr (t => direct (t))
        lazy val indirect : TestTree => Int =
            attr (t => indirect2 (t))
        lazy val indirect2 : TestTree => Int =
            attr (t => indirect (t))

        val t = Pair (Leaf (3), Pair (Leaf (1), Leaf (10)))

        val i1 = intercept[IllegalStateException] {
                     direct (t)
                 }
        assertResult ("Cycle detected in attribute evaluation 'direct' at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i1.getMessage)

        val i2 = intercept[IllegalStateException] {
                     indirect (t)
                 }
        assertResult ("Cycle detected in attribute evaluation 'indirect' at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i2.getMessage)

        val i3 = intercept[IllegalStateException] {
                     indirect2 (t)
                 }
        assertResult ("Cycle detected in attribute evaluation 'indirect2' at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i3.getMessage)
    }

    test ("circularities are detected for uncached attributes") {
        import UncachedAttribution._

        lazy val direct : TestTree => Int =
            attr (t => direct (t))
        lazy val indirect : TestTree => Int =
            attr (t => indirect2 (t))
        lazy val indirect2 : TestTree => Int =
            attr (t => indirect (t))

        val t = Pair (Leaf (3), Pair (Leaf (1), Leaf (10)))

        val i1 = intercept[IllegalStateException] {
                     direct (t)
                 }
        assertResult ("Cycle detected in attribute evaluation 'direct' at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i1.getMessage)

        val i2 = intercept[IllegalStateException] {
                     indirect (t)
                 }
        assertResult ("Cycle detected in attribute evaluation 'indirect' at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i2.getMessage)

        val i3 = intercept[IllegalStateException] {
                     indirect2 (t)
                 }
        assertResult ("Cycle detected in attribute evaluation 'indirect2' at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i3.getMessage)
    }

    test ("circularities are detected for parameterised attributes") {
        lazy val direct : Int => TestTree => Int =
            paramAttr (i => (t => direct (i) (t)))
        lazy val indirect : Int => TestTree => Int =
            paramAttr (i => (t => indirect2 (i) (t)))
        lazy val indirect2 : Int => TestTree => Int =
            paramAttr (i => (t => indirect (i) (t)))

        val t = Pair (Leaf (3), Pair (Leaf (1), Leaf (10)))

        val i1 = intercept[IllegalStateException] {
                     direct (1) (t)
                 }
        assertResult ("Cycle detected in attribute evaluation 'direct' (1) at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i1.getMessage)

        val i2 = intercept[IllegalStateException] {
                     indirect (8) (t)
                 }
        assertResult ("Cycle detected in attribute evaluation 'indirect' (8) at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i2.getMessage)

        val i3 = intercept[IllegalStateException] {
                     indirect2 (9) (t)
                 }
        assertResult ("Cycle detected in attribute evaluation 'indirect2' (9) at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i3.getMessage)
    }

    test ("circularities are detected for uncached parameterised attributes") {
        import UncachedAttribution._

        lazy val direct : Int => TestTree => Int =
            paramAttr (i => (t => direct (i) (t)))
        lazy val indirect : Int => TestTree => Int =
            paramAttr (i => (t => indirect2 (i) (t)))
        lazy val indirect2 : Int => TestTree => Int =
            paramAttr (i => (t => indirect (i) (t)))

        val t = Pair (Leaf (3), Pair (Leaf (1), Leaf (10)))

        val i1 = intercept[IllegalStateException] {
                     direct (1) (t)
                 }
        assertResult ("Cycle detected in attribute evaluation 'direct' (1) at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i1.getMessage)

        val i2 = intercept[IllegalStateException] {
                     indirect (8) (t)
                 }
        assertResult ("Cycle detected in attribute evaluation 'indirect' (8) at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i2.getMessage)

        val i3 = intercept[IllegalStateException] {
                     indirect2 (9) (t)
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
        assert (!(n equals k1))
        assert (!(k1 equals n))
        assert (k1 equals k2)
        assert (k2 equals k1)
        assert (!(k1 equals k3))
        assert (!(k3 equals k1))
        assert (!(k1 equals k4))
        assert (!(k4 equals k1))
        assert (!(k1 equals k5))
        assert (!(k5 equals k1))
        assert (!(k1 equals k6))
        assert (!(k6 equals k1))
        assert (!(k1 equals k7))
        assert (!(k7 equals k1))
        assert (!(k1 equals k8))
        assert (!(k8 equals k1))
        assert (k6 equals k7)
        assert (k7 equals k6)
        assert (!(k6 equals k8))
        assert (!(k8 equals k6))
    }

    {
        val t = Pair (Leaf (3), Pair (l, Leaf (10)))

        val tree = new Tree[TestTree,Pair] (t)
        val decorators = new Decorators (tree)
        import decorators._

        test ("a constant atRoot attribute returns the constant value") {
            val rattr = atRoot[Int] (_ => 99)
            assertResult (99) (rattr (t))
            assertResult (99) (rattr (l))
        }

        test ("a variable atRoot attribute returns the value from the root") {
            val rattr = atRoot[Int] {
                            case tree.parent (_) =>
                                99
                            case _ =>
                                42
                        }
            assertResult (42) (rattr (t))
            assertResult (42) (rattr (l))
        }

        test ("a down attribute with default function returns the computed value") {
            val dattr = down[Int] (
                            (n : TestTree) =>
                                if (tree.parent (n) == Seq ()) 42 else 66
                        ) {
                            case tree.parent (_) =>
                                99
                        }
            assertResult (42) (dattr (t))
            assertResult (99) (dattr (l))
        }

        test ("a down attribute that is defined returns the computed value") {
            val dattr = down[Int] (99) { case _ : Pair => 42 }
            assertResult (42) (dattr (l))
        }

        test ("a down attribute that is not defined returns the default value") {
            val dattr = down[Int] (99) { case _ : Unused => 42 }
            assertResult (99) (dattr (l))
        }

        test ("a downErr attribute that is defined returns the computed value") {
            val dattr = downErr[Int] { case _ : Pair => 42 }
            assertResult (42) (dattr (l))
        }

        test ("a downErr attribute that is not defined throws an error") {
            val dattr = downErr[Int] { case _ : Unused => 42 }
            val i = intercept[RuntimeException] {
                        dattr (l)
                    }
            assertResult ("downErr: function is not defined on path to root") (i.getMessage)
        }

        test ("a downOpt attribute that is defined returns Some of the computed value") {
            val dattr = downOpt[Int] { case _ : Pair => 42 }
            assertResult (Some (42)) (dattr (l))
        }

        test ("a downOpt attribute that is not defined returns None") {
            val dattr = downOpt[Int] { case _ : Unused => 42 }
            assertResult (None) (dattr (l))
        }

    }

    test ("a chain that is only defined at the root returns the root value") {
        val t = Pair (Leaf (3), Pair (Leaf (1), Leaf (10)))

        val tree = new Tree[TestTree,Pair] (t)
        val decorators = new Decorators (tree)
        import decorators._

        def rootupd (in : TestTree => Int) : TestTree ==> Int = {
            case n if tree.isRoot (n) => 42
        }
        val rootchain = chain (rootupd)
        assertResult (42) (rootchain.in (t))
        assertResult (42) (rootchain.out (t))
    }

    test ("a chain with no updates throws appropriate exceptions") {
        val t = Pair (Leaf (3), Pair (Leaf (1), Leaf (10)))

        val tree = new Tree[TestTree,Pair] (t)
        val decorators = new Decorators (tree)
        import decorators._

        // A chain with only identiy update functions
        val idchain = chain [Int] ()
        val i1 = intercept[RuntimeException] {
                     idchain.in (t)
                 }
        assertResult ("chain root of tree reached at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i1.getMessage)
        val i2 = intercept[RuntimeException] {
                     idchain.out (t)
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
                     refchain.in (t)
                 }
        assertResult ("chain root of tree reached at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i3.getMessage)
        val i4 = intercept[RuntimeException] {
                     refchain.out (t)
                 }
        assertResult ("chain root of tree reached at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))") (i4.getMessage)

    }

    test ("a circular attribute that never changes evaluates to initial value") {
        import org.kiama.example.imperative.ImperativeTree.Num

        lazy val zero : CircularAttribute[Num,Double] =
            circular (0.0) (_ => 0)

        val n = Num (1)
        assert (!zero.hasBeenComputedAt (n))
        assertResult (0) (zero (n))
        assert (zero.hasBeenComputedAt (n))
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
        assert (!counter.hasBeenComputedAt (n))
        assert (!pass.hasBeenComputedAt (n))
        assertResult (0.0) (counter (n))
        assert (counter.hasBeenComputedAt (n))
        assert (pass.hasBeenComputedAt (n))
        assertResult (0.0) (pass (n))
        assert (counter.hasBeenComputedAt (n))
        assert (pass.hasBeenComputedAt (n))
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
        assert (!counter.hasBeenComputedAt (n))
        assertResult (10.0) (counter (n))
        assert (counter.hasBeenComputedAt (n))
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


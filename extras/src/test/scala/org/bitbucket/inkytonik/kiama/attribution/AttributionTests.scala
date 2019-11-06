/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package attribution

import org.bitbucket.inkytonik.kiama.util.KiamaTests

/**
 * Tests of basic attribution.
 */
class AttributionTests extends KiamaTests {

    import org.bitbucket.inkytonik.kiama.relation.Tree

    abstract class TestTree extends Product
    case class Pair(left : TestTree, right : TestTree) extends TestTree
    case class Leaf(value : Int) extends TestTree
    case class Unused(b : Boolean) extends TestTree

    val l = Leaf(3)
    val s = Pair(Leaf(3), Pair(Leaf(1), Leaf(10)))
    val t = Pair(Leaf(3), Pair(Leaf(1), Leaf(10)))
    val u = Pair(Leaf(1), Leaf(2))
    val v = Pair(Leaf(3), Pair(Leaf(1), Leaf(3)))

    /**
     * Definitions of the attributes that will be tested below. We package
     * them in a class so that each test can have its own instance of the
     * attributes so that there is no shared state.
     */
    class Definitions extends Attribution {

        var count = 0

        lazy val incDef : Int => Int =
            {
                case i => count = count + 1; i + 1
            }

        lazy val inc =
            attr(incDef)

        lazy val concatDef : Int => Int => Int =
            {
                case 1 => {
                    case i => count = count + 1; i + 1
                }
                case 2 => {
                    case 0 =>
                        count = count + 1; 999
                    case i => count = count + 1; i + 2
                }
                case n => {
                    case i => count = count + 1; i * i
                }
            }

        lazy val concat =
            paramAttr(concatDef)

        lazy val maximumDef : TestTree => Int =
            {
                case Pair(l, r) =>
                    count = count + 1; maximum(l).max(maximum(r))
                case Leaf(v) => v
            }

        lazy val maximum =
            attr(maximumDef)

        lazy val leafComputedDef : TestTree => Boolean =
            {
                case t @ Leaf(v) =>
                    leafComputed.hasBeenComputedAt(t)
            }

        lazy val leafComputed =
            attr(leafComputedDef)

        lazy val pattrDef : String => TestTree => Int =
            {
                case "hello" => {
                    case Pair(l, r) =>
                        count = count + 1; 0
                    case Leaf(v) => 1
                    case _       => 2
                }
                case "goodbye" => {
                    case _ => 3
                }
            }

        lazy val answer : TestTree => Int =
            constant { count = count + 1; 42 }

        lazy val exception : CachedAttribute[TestTree, Int] =
            attr {
                case _ =>
                    throw new Exception("exception in attribute definition")
            }

        lazy val exceptionParam : CachedParamAttribute[Int, TestTree, Int] =
            paramAttr(i => {
                case _ =>
                    throw new Exception("exception in attribute definition")
            })

    }

    test("cached attributes are correctly evaluated") {
        val definitions = new Definitions
        import definitions._

        maximum.hasBeenComputedAt(t) shouldBe false
        maximum(t) shouldBe 10
        maximum.hasBeenComputedAt(t) shouldBe true
        maximum(t) shouldBe 10
        maximum.hasBeenComputedAt(t) shouldBe true
        count shouldBe 2
    }

    test("reset resets the hasBeenComputedAt state") {
        val definitions = new Definitions
        import definitions._

        maximum.hasBeenComputedAt(t) shouldBe false
        maximum(t)
        maximum.hasBeenComputedAt(t) shouldBe true
        maximum.reset()
        maximum.hasBeenComputedAt(t) shouldBe false
    }

    test("hasBeenComputedAt returns false while an attribute is being evaluated") {
        val definitions = new Definitions
        import definitions._

        leafComputed(l) shouldBe false
        leafComputed.hasBeenComputedAt(l) shouldBe true
    }

    test("constant attributes are only evaluated once") {
        val definitions = new Definitions
        import definitions._

        answer(t) shouldBe 42
        answer(t) shouldBe 42
        count shouldBe 1
    }

    test("cached attributes are re-evaluated after a reset") {
        val definitions = new Definitions
        import definitions._

        maximum(t) shouldBe 10
        maximum(t) shouldBe 10
        count shouldBe 2
        maximum.reset()
        maximum(t) shouldBe 10
        count shouldBe 4
    }

    test("cached attributes are distinct for nodes that are equal") {
        val definitions = new Definitions
        import definitions._

        maximum(t) shouldBe 10
        maximum(s) shouldBe 10
        count shouldBe 4
    }

    test("uncached attributes are evaluated each time") {
        var count = 0

        lazy val maximum : TestTree => Int =
            UncachedAttribution.attr {
                case Pair(l, r) =>
                    count = count + 1; maximum(l).max(maximum(r))
                case Leaf(v) => v
            }

        maximum(t) shouldBe 10
        maximum(t) shouldBe 10
        count shouldBe 4
    }

    test("cached parameterised attributes work") {
        val definitions = new Definitions
        import definitions._

        lazy val pattr =
            paramAttr(pattrDef)

        pattr("hello")(Leaf(1)) shouldBe 1
        pattr("goodbye")(Leaf(1)) shouldBe 3
    }

    test("cached parameterised attributes are re-evaluated after reset") {
        val definitions = new Definitions
        import definitions._

        lazy val pattr =
            paramAttr(pattrDef)

        pattr.hasBeenComputedAt("hello", u) shouldBe false
        pattr("hello")(u) shouldBe 0
        pattr.hasBeenComputedAt("hello", u) shouldBe true
        pattr("hello")(u) shouldBe 0
        count shouldBe 1
        pattr.hasBeenComputedAt("hello", u) shouldBe true
        pattr.reset()
        pattr.hasBeenComputedAt("hello", u) shouldBe false
        pattr("hello")(u) shouldBe 0
        count shouldBe 2
        pattr.hasBeenComputedAt("hello", u) shouldBe true
    }

    test("cached parameterised attributes can be reset at specific keys") {
        val definitions = new Definitions
        import definitions._

        lazy val pattr =
            paramAttr(pattrDef)

        pattr.hasBeenComputedAt("hello", u) shouldBe false
        pattr.hasBeenComputedAt("goodbye", u) shouldBe false
        pattr("hello")(u) shouldBe 0
        pattr("goodbye")(u) shouldBe 3
        count shouldBe 1
        pattr.hasBeenComputedAt("hello", u) shouldBe true
        pattr.hasBeenComputedAt("goodbye", u) shouldBe true
        pattr.resetAt("hello", u)
        pattr.hasBeenComputedAt("hello", u) shouldBe false
        pattr.hasBeenComputedAt("goodbye", u) shouldBe true
        pattr("hello")(u) shouldBe 0
        pattr("goodbye")(u) shouldBe 3
        count shouldBe 2
        pattr.hasBeenComputedAt("hello", u) shouldBe true
        pattr.hasBeenComputedAt("goodbye", u) shouldBe true
    }

    test("uncached parameterised attributes work") {
        val definitions = new Definitions
        import definitions.pattrDef

        lazy val pattr =
            UncachedAttribution.paramAttr(pattrDef)

        pattr("hello")(Pair(Leaf(1), Leaf(2))) shouldBe 0
        pattr("goodbye")(Pair(Leaf(1), Leaf(2))) shouldBe 3
        pattr("hello")(Leaf(1)) shouldBe 1
        pattr("goodbye")(Leaf(1)) shouldBe 3
    }

    test("circularities are detected for cached attributes") {
        val definitions = new Definitions
        import definitions._

        lazy val direct : TestTree => Int =
            attr(t => direct(t))
        lazy val indirect : TestTree => Int =
            attr(t => indirect2(t))
        lazy val indirect2 : TestTree => Int =
            attr(t => indirect(t))

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

    test("circularities are detected for uncached attributes") {
        lazy val direct : TestTree => Int =
            UncachedAttribution.attr(t => direct(t))
        lazy val indirect : TestTree => Int =
            UncachedAttribution.attr(t => indirect2(t))
        lazy val indirect2 : TestTree => Int =
            UncachedAttribution.attr(t => indirect(t))

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

    test("circularities are detected for parameterised attributes") {
        val definitions = new Definitions
        import definitions._

        lazy val direct : Int => TestTree => Int =
            paramAttr(i => (t => direct(i)(t)))
        lazy val indirect : Int => TestTree => Int =
            paramAttr(i => (t => indirect2(i)(t)))
        lazy val indirect2 : Int => TestTree => Int =
            paramAttr(i => (t => indirect(i)(t)))

        val t = Pair(Leaf(3), Pair(Leaf(1), Leaf(10)))

        val i1 = intercept[IllegalStateException] {
            direct(1)(t)
        }
        i1.getMessage shouldBe "Cycle detected in attribute evaluation 'direct' (1) at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))"

        val i2 = intercept[IllegalStateException] {
            indirect(8)(t)
        }
        i2.getMessage shouldBe "Cycle detected in attribute evaluation 'indirect' (8) at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))"

        val i3 = intercept[IllegalStateException] {
            indirect2(9)(t)
        }
        i3.getMessage shouldBe "Cycle detected in attribute evaluation 'indirect2' (9) at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))"
    }

    test("circularities are detected for uncached parameterised attributes") {
        lazy val direct : Int => TestTree => Int =
            UncachedAttribution.paramAttr(i => (t => direct(i)(t)))
        lazy val indirect : Int => TestTree => Int =
            UncachedAttribution.paramAttr(i => (t => indirect2(i)(t)))
        lazy val indirect2 : Int => TestTree => Int =
            UncachedAttribution.paramAttr(i => (t => indirect(i)(t)))

        val t = Pair(Leaf(3), Pair(Leaf(1), Leaf(10)))

        val i1 = intercept[IllegalStateException] {
            direct(1)(t)
        }
        i1.getMessage shouldBe "Cycle detected in attribute evaluation 'direct' (1) at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))"

        val i2 = intercept[IllegalStateException] {
            indirect(8)(t)
        }
        i2.getMessage shouldBe "Cycle detected in attribute evaluation 'indirect' (8) at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))"

        val i3 = intercept[IllegalStateException] {
            indirect2(9)(t)
        }
        i3.getMessage shouldBe "Cycle detected in attribute evaluation 'indirect2' (9) at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))"
    }

    test("parameterised attribute keys compare correctly") {
        val n = Leaf(1)
        val k1 = new ParamAttributeKey("hello", n)
        val k2 = new ParamAttributeKey("hello", n)
        val k3 = new ParamAttributeKey("hello", Leaf(1))
        val k4 = new ParamAttributeKey("goodbye", n)
        val k5 = new ParamAttributeKey("goodbye", Leaf(1))
        val k6 = new ParamAttributeKey("hello", null)
        val k7 = new ParamAttributeKey("hello", null)
        val k8 = new ParamAttributeKey("goodbye", null)

        n should not equal k1
        k1 should not equal k3
        k1 should not equal k4
        k1 should not equal k5
        k1 should not equal k6
        k1 should not equal k7
        k1 should not equal k8
        k6 should not equal k8

        k1 shouldEqual k2
        k6 shouldEqual k7
    }

    {
        val t = Pair(Leaf(3), Pair(l, Leaf(10)))

        val tree = new Tree[TestTree, Pair](t)
        val decorators = new Decorators(tree)
        import decorators._

        test("a constant atRoot attribute returns the constant value") {
            val rattr = atRoot[Int](_ => 99)
            rattr(t) shouldBe 99
            rattr(l) shouldBe 99
        }

        test("a variable atRoot attribute returns the value from the root") {
            val rattr = atRoot[Int] {
                case tree.parent(_) =>
                    99
                case _ =>
                    42
            }
            rattr(t) shouldBe 42
            rattr(l) shouldBe 42
        }

        test("a down attribute with default function returns the computed value") {
            val dattr = down[Int](
                (n : TestTree) =>
                    if (tree.parent(n) == Nil) 42 else 66
            ) {
                    case tree.parent(_) =>
                        99
                }
            dattr(t) shouldBe 42
            dattr(l) shouldBe 99
        }

        test("a down attribute that is defined returns the computed value") {
            val dattr = down[Int](99) { case _ : Pair => 42 }
            dattr(l) shouldBe 42
        }

        test("a down attribute that is not defined returns the default value") {
            val dattr = down[Int](99) { case _ : Unused => 42 }
            dattr(l) shouldBe 99
        }

        test("a downErr attribute that is defined returns the computed value") {
            val dattr = downErr[Int] { case _ : Pair => 42 }
            dattr(l) shouldBe 42
        }

        test("a downErr attribute that is not defined throws an error") {
            val dattr = downErr[Int] { case _ : Unused => 42 }
            val i = intercept[RuntimeException] {
                dattr(l)
            }
            i.getMessage shouldBe "downErr: function is not defined on path to root"
        }

        test("a downOpt attribute that is defined returns Some of the computed value") {
            val dattr = downOpt[Int] { case _ : Pair => 42 }
            dattr(l) shouldBe Some(42)
        }

        test("a downOpt attribute that is not defined returns None") {
            val dattr = downOpt[Int] { case _ : Unused => 42 }
            dattr(l) shouldBe None
        }

    }

    test("a chain that is only defined at the root returns the root value") {
        val t = Pair(Leaf(3), Pair(Leaf(1), Leaf(10)))

        val tree = new Tree[TestTree, Pair](t)
        val decorators = new Decorators(tree)
        import decorators._

        def rootupd(in : TestTree => Int) : TestTree ==> Int = {
            case n if tree.isRoot(n) => 42
        }
        val rootchain = chain(rootupd)
        rootchain.in(t) shouldBe 42
        rootchain.out(t) shouldBe 42
    }

    test("a chain with no updates throws appropriate exceptions") {
        val t = Pair(Leaf(3), Pair(Leaf(1), Leaf(10)))

        val tree = new Tree[TestTree, Pair](t)
        val decorators = new Decorators(tree)
        import decorators._

        // A chain with only identiy update functions
        val idchain = chain[Int]()
        val i1 = intercept[RuntimeException] {
            idchain.in(t)
        }
        i1.getMessage shouldBe "chain root of tree reached at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))"
        val i2 = intercept[RuntimeException] {
            idchain.out(t)
        }
        i2.getMessage shouldBe "chain root of tree reached at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))"

        // A chain with refusing-all-in update function. This exercises a
        // different path in the 'in' attribute to the previous checks.
        def refuse(in : TestTree => Int) : TestTree ==> Int =
            new (TestTree ==> Int) {
                def apply(t : TestTree) : Int = in(t) // Never used
                def isDefinedAt(t : TestTree) : Boolean = false
            }
        val refchain = chain(refuse)
        val i3 = intercept[RuntimeException] {
            refchain.in(t)
        }
        i3.getMessage shouldBe "chain root of tree reached at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))"
        val i4 = intercept[RuntimeException] {
            refchain.out(t)
        }
        i4.getMessage shouldBe "chain root of tree reached at Pair(Leaf(3),Pair(Leaf(1),Leaf(10)))"

    }

    test("a circular attribute that never changes evaluates to initial value") {
        val definitions = new Definitions
        import definitions._

        import org.bitbucket.inkytonik.kiama.example.imperative.ImperativeTree.Num

        lazy val zero : CircularAttribute[Num, Double] =
            circular(0.0)(_ => 0)

        val n = Num(1)
        zero.hasBeenComputedAt(n) shouldBe false
        zero(n) shouldBe 0
        zero.hasBeenComputedAt(n) shouldBe true
    }

    test("two circular attributes that never change from initial value do converge") {
        val definitions = new Definitions
        import definitions._

        import org.bitbucket.inkytonik.kiama.example.imperative.ImperativeTree.Num

        lazy val counter : CircularAttribute[Num, Double] =
            circular(0.0)(
                (n : Num) =>
                    pass(n)
            )

        lazy val pass : CircularAttribute[Num, Double] =
            circular(0.0)(
                (n : Num) =>
                    counter(n)
            )

        val n = Num(1)
        counter.hasBeenComputedAt(n) shouldBe false
        pass.hasBeenComputedAt(n) shouldBe false
        counter(n) shouldBe 0.0
        counter.hasBeenComputedAt(n) shouldBe true
        pass(n) shouldBe 0.0
        counter.hasBeenComputedAt(n) shouldBe true
        pass.hasBeenComputedAt(n) shouldBe true
    }

    test("a directly circular attribute can count") {
        val definitions = new Definitions
        import definitions._

        import org.bitbucket.inkytonik.kiama.example.imperative.ImperativeTree.Num

        lazy val counter : CircularAttribute[Num, Double] =
            circular(0.0)(
                (n : Num) => {
                    val current = counter(n)
                    current + (if (current < 10) n.d else 0)
                }
            )

        val n = Num(1)
        counter.hasBeenComputedAt(n) shouldBe false
        counter(n) shouldBe 10.0
        counter.hasBeenComputedAt(n) shouldBe true
    }

    test("a cycle of two circular attributes can count") {
        val definitions = new Definitions
        import definitions._

        import org.bitbucket.inkytonik.kiama.example.imperative.ImperativeTree.Num

        lazy val counter : Num => Double =
            circular(0.0)(
                (n : Num) => {
                    val current = pass(n)
                    current + (if (current < 10) n.d else 0)
                }
            )

        lazy val pass : Num => Double =
            circular(0.0)(
                (n : Num) =>
                    counter(n)
            )

        val n = Num(1)
        counter(n) shouldBe 10.0
        pass(n) shouldBe 10.0
    }

    test("a cycle of three circular attributes can count") {
        val definitions = new Definitions
        import definitions._

        import org.bitbucket.inkytonik.kiama.example.imperative.ImperativeTree.Num

        lazy val counter : Num => Double =
            circular(0.0)(
                (n : Num) => {
                    val current = double(n)
                    current + (if (current < 10) n.d else 0)
                }
            )

        lazy val double : Num => Double =
            circular(0.0)(
                (n : Num) => {
                    val current = pass(n)
                    if (current < 10) current * 2 else current
                }
            )

        lazy val pass : Num => Double =
            circular(0.0)(
                (n : Num) =>
                    counter(n)
            )

        val n = Num(1)
        counter(n) shouldBe 14.0
        double(n) shouldBe 14.0
        pass(n) shouldBe 14.0
    }

    test("a single circular attribute plus a cycle of two circular attributes can count") {
        val definitions = new Definitions
        import definitions._

        import org.bitbucket.inkytonik.kiama.example.imperative.ImperativeTree.Num

        lazy val entry : Num => Double =
            circular(0.0)(
                (n : Num) =>
                    counter(n)
            )

        lazy val counter : Num => Double =
            circular(0.0)(
                (n : Num) => {
                    val current = double(n)
                    current + (if (current < 10) n.d else 0)
                }
            )

        lazy val double : Num => Double =
            circular(0.0)(
                (n : Num) => {
                    val current = counter(n)
                    if (current < 10) current * 2 else current
                }
            )

        val n = Num(1)
        entry(n) shouldBe 14.0
        counter(n) shouldBe 14.0
        double(n) shouldBe 14.0
    }

    test("a single circular attribute plus a cycle of two trivial circular attributes converges") {
        val definitions = new Definitions
        import definitions._

        import org.bitbucket.inkytonik.kiama.example.imperative.ImperativeTree.Num

        lazy val entry : Num => Double =
            circular(0.0)(
                (n : Num) =>
                    pass1(n)
            )

        lazy val pass1 : Num => Double =
            circular(0.0)(
                (n : Num) =>
                    pass2(n)
            )

        lazy val pass2 : Num => Double =
            circular(0.0)(
                (n : Num) =>
                    pass1(n)
            )

        val n = Num(1)
        entry(n) shouldBe 0.0
        pass1(n) shouldBe 0.0
        pass2(n) shouldBe 0.0
    }

    test("cached attribute definitions that throw exceptions reset evaluation status") {
        val definitions = new Definitions
        import definitions._

        val i = intercept[Exception] {
            exception(t)
        }
        i.getMessage shouldBe "exception in attribute definition"
        exception.memo.hasBeenComputedAt(t) shouldBe false
    }

    test("cached parameterised attribute definitions that throw exceptions reset evaluation status") {
        val definitions = new Definitions
        import definitions._

        val arg = 1
        val i = intercept[Exception] {
            exceptionParam(arg)(t)
        }
        i.getMessage shouldBe "exception in attribute definition"
        val key = new ParamAttributeKey(arg, t)
        exceptionParam.memo.hasBeenComputedAt(key) shouldBe false
    }

    // Thread safety

    {
        import java.util.concurrent.Executors
        import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.collectl
        import scala.concurrent.{Await, ExecutionContext, Future}
        import scala.concurrent.duration._

        def makeContext(n : Int) =
            ExecutionContext.fromExecutor(Executors.newFixedThreadPool(n))

        // Single attributes
        // Core of the single attribute tests was contributed by Fred Teunissen

        trait AstBase extends Product
        case class Node(childs : AstBase*) extends AstBase
        case class Leaf(value : Int) extends AstBase

        val root : Node =
            Node(
                Node(
                    Node(Leaf(1), Leaf(2), Leaf(3)),
                    Node(Leaf(4), Leaf(5), Leaf(6)),
                    Node(Leaf(7), Leaf(8), Leaf(9))
                ),
                Node(
                    Node(Leaf(11), Leaf(12), Leaf(13)),
                    Node(Leaf(14), Leaf(15), Leaf(16)),
                    Node(Leaf(17), Leaf(18), Leaf(19))
                ),
                Node(
                    Node(Leaf(21), Leaf(22), Leaf(23)),
                    Node(Leaf(24), Leaf(25), Leaf(26)),
                    Node(Leaf(27), Leaf(28), Leaf(29))
                )
            )

        val expectedValues : Seq[Int] =
            Seq(
                405, 45, 135, 36, 45, 54, 225, 66, 75, 26, 84, 27, 28, 29,
                405, 135, 54, 225, 66, 75, 84, 405, 135, 225, 405, 225
            )

        case class Compiler(root : Node) extends Attribution {

            def aggregateValues(astTestBase : AstBase) : Int =
                astTestBase.synchronized {
                    aggregateValuesAttr(astTestBase)
                }

            val body : AstBase => Int =
                {
                    case node : Leaf =>
                        node.value
                    case node : Node =>
                        node.childs.map(aggregateValuesAttr(_)).sum
                }

            val aggregateValuesAttr : CachedAttribute[AstBase, Int] =
                attr(body)

            val aggregateValuesParamAttr : CachedParamAttribute[Int, AstBase, Int] =
                paramAttr(_ => body)

            val aggregateValuesDynAttr : CachedDynamicAttribute[AstBase, Int] =
                dynAttr(body)

            def filterByThresholds(
                attribute : AstBase => Int,
                thresholds : Seq[Int]
            )(implicit executionContext : ExecutionContext) : Future[Seq[Int]] =
                Future.sequence(thresholds.map(
                    threshold =>
                        filterByThreshold(attribute, threshold)
                )).map(_.flatten)

            def filterByThreshold(
                attribute : AstBase => Int,
                threshold : Int
            )(implicit executionContext : ExecutionContext) : Future[Seq[Int]] =
                Future {
                    collectl {
                        case node : AstBase if aggregateValuesAttr(node) > threshold =>
                            Some(attribute(node))
                        case node =>
                            None
                    }(root).flatten
                }

            def filterByThresholdsAttr(
                thresholds : Seq[Int]
            )(implicit executionContext : ExecutionContext) : Future[Seq[Int]] =
                filterByThresholds(aggregateValuesAttr, thresholds)

            def filterByThresholdsParamAttr(
                thresholds : Seq[Int]
            )(implicit executionContext : ExecutionContext) : Future[Seq[Int]] =
                filterByThresholds(aggregateValuesParamAttr(1), thresholds)

            def filterByThresholdsDynAttr(
                thresholds : Seq[Int]
            )(implicit executionContext : ExecutionContext) : Future[Seq[Int]] =
                filterByThresholds(aggregateValuesParamAttr(1), thresholds)

        }

        def checkResult(filtered : Future[Seq[Int]]) : Unit = {
            val validations = Await.result(filtered, 20.seconds)
            validations should contain theSameElementsAs expectedValues
        }

        val thresholds = Seq(25, 50, 100, 200)

        for (n <- 1 to 4) {
            test(s"cached attribute thread safety ($n threads)") {
                implicit val executionContext = makeContext(n)
                checkResult(Compiler(root).filterByThresholdsAttr(thresholds))
            }

            test(s"cached parameterised attribute thread safety ($n threads)") {
                implicit val executionContext = makeContext(n)
                checkResult(Compiler(root).filterByThresholdsParamAttr(thresholds))
            }

            test(s"cached dynamic attribute thread safety ($n threads)") {
                implicit val executionContext = makeContext(n)
                checkResult(Compiler(root).filterByThresholdsDynAttr(thresholds))
            }
        }

        // Circular attributes

        import org.bitbucket.inkytonik.kiama.example.dataflow.{DataflowTests, DataflowTree}

        import DataflowTree.Stm

        val dataflowTests = new DataflowTests
        import dataflowTests.{dataflow, s1, s2, s3, s4, s5}

        def computeDataflow(
            stmts : Seq[Stm]
        )(implicit executionContext : ExecutionContext) =
            Future.sequence(stmts.map(
                stmt =>
                    Future { dataflow.out(stmt) }
            ))

        val stmts = Seq(s1, s2, s3, s4, s5)
        val expectedOuts =
            List(
                Set("y", "v", "w"), Set("v", "w"), Set("x", "w", "v"),
                Set("x", "w", "v"), Set()
            )

        for (n <- 1 to 4) {
            test(s"circular attribute thread safety ($n threads)") {
                implicit val executionContext = makeContext(n)
                val result = Await.result(computeDataflow(stmts), 20.seconds)
                result shouldBe expectedOuts
            }
        }
    }

}

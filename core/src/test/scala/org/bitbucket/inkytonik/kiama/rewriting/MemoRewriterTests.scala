/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package rewriting

/**
 * Supporting definitions for the memo rewriter tests.
 */
object MemoRewriterTestsSupport {

    abstract class N
    case class A() extends N
    case class B() extends N
    case class S(n : N) extends N
    case class T(n : N, uses : Int) extends N
    case class P(l : N, r : N) extends N

}

/**
 * Tests of memoising rewriting.
 */
class MemoRewriterTests extends {

    override val rewriter = MemoRewriter

} with RewriterTests {

    import MemoRewriterTestsSupport._
    import rewriter._

    val atob =
        rule[N] {
            case A() => B()
        }

    test("a memoising strategy actually memoises") {
        val s = everywhere(atob).asInstanceOf[MemoStrategy]
        val t : N = A()
        s.hasBeenComputedAt(t) shouldBe false
        val result = rewrite(s)(t)
        result shouldBe B()
        s.hasBeenComputedAt(t) shouldBe true
        rewrite(s)(t) should be theSameInstanceAs result
        s.reset()
        s.hasBeenComputedAt(t) shouldBe false
        rewrite(s)(t) shouldBe B()
        s.hasBeenComputedAt(t) shouldBe true
    }

    test("resetting all memoising strategies actually does reset them") {
        val r = atob.asInstanceOf[MemoStrategy]
        val s = everywheretd(atob).asInstanceOf[MemoStrategy]
        val t = P(A(), A())
        s.hasBeenComputedAt(t) shouldBe false
        r.hasBeenComputedAt(t.l) shouldBe false
        r.hasBeenComputedAt(t.r) shouldBe false
        rewrite(s)(t) shouldBe P(B(), B())
        s.hasBeenComputedAt(t) shouldBe true
        r.hasBeenComputedAt(t.l) shouldBe true
        r.hasBeenComputedAt(t.r) shouldBe true
        s.reset()
        r.reset()
        s.hasBeenComputedAt(t) shouldBe false
        r.hasBeenComputedAt(t.l) shouldBe false
        r.hasBeenComputedAt(t.r) shouldBe false
        rewrite(s)(t) shouldBe P(B(), B())
    }

    /**
     * Generate tests that ensure that if a node is rewritten more than once
     * in a traversal by `strat` the results at each stage will be shared in
     * the overall result.
     */
    def testSharingRewrite(direction : String, strat : Strategy) {

        val s = P(A(), A())
        val t = P(s, s)
        val u = P(t, t)
        val result = rewrite(strat)(u)

        test(s"memo rewriting produces correct term ($direction)") {
            val expected = P(P(P(B(), B()), P(B(), B())), P(P(B(), B()), P(B(), B())))
            result shouldBe expected
        }

        test(s"memo rewriting preserves top-level sharing ($direction)") {
            result.r should be theSameInstanceAs result.l
        }

        test(s"memo rewriting preserves second-level sharing ($direction)") {
            val lp = result.l.asInstanceOf[P]
            val rp = result.r.asInstanceOf[P]
            lp.r should be theSameInstanceAs lp.l
            rp.r should be theSameInstanceAs rp.l
        }

        test(s"memo rewriting preserves third-level sharing ($direction)") {
            val lls = result.l.asInstanceOf[P].l.asInstanceOf[P]
            val lrs = result.l.asInstanceOf[P].r.asInstanceOf[P]
            val rls = result.r.asInstanceOf[P].l.asInstanceOf[P]
            val rrs = result.r.asInstanceOf[P].r.asInstanceOf[P]
            lrs.l should be theSameInstanceAs lls.l
            rls.l should be theSameInstanceAs lrs.l
            rrs.l should be theSameInstanceAs rls.l
            lrs.r should be theSameInstanceAs lls.r
            rls.r should be theSameInstanceAs lrs.r
            rrs.r should be theSameInstanceAs rls.r
        }

    }

    testSharingRewrite("bottom-up", everywherebu(atob))
    testSharingRewrite("top-down", everywheretd(atob))

    {
        // Test based on Eric Torreborre's node fusion example from Scoobi

        val tta = T(T(A(), 1), 2)
        val root = P(T(tta, 1), T(T(tta, 1), 1))

        val fuseT =
            rule[N] {
                case T(n, 1) => S(n)
            }

        val fuse = everywheretd(fuseT)

        test("conditional node fusion preserves sharing") {
            val expected = P(S(T(S(A()), 2)), S(S(T(S(A()), 2))))
            val result = rewrite(fuse)(root)
            result shouldBe expected
            val resultls = result.l.asInstanceOf[S]
            val resultrss = result.r.asInstanceOf[S].n.asInstanceOf[S]
            resultrss.n should be theSameInstanceAs resultls.n
        }
    }

}

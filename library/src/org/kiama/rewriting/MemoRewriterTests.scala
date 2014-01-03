/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2014 Anthony M Sloane, Macquarie University.
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

/**
 * Supporting definitions for the memo rewriter tests.
 */
object MemoRewriterTestsSupport {

    abstract class N
    case class A () extends N
    case class B () extends N
    case class S (n : N) extends N
    case class T (n : N, uses : Int) extends N
    case class P (l : N, r : N) extends N

}

/**
 * Tests of memoising rewriting.
 */
class MemoRewriterTests extends {

    override val rewriter = MemoRewriter

} with RewriterTests {

    import MemoRewriterTestsSupport._
    import rewriter.{test => rwtest, _}

    val atob =
        rule[N] {
            case A () => B ()
        }

    test ("a memoising strategy actually memoises") {
        val s = everywhere (atob).asInstanceOf[MemoStrategy]
        val t : N = A ()
        assertResult (false) (s.hasBeenComputedAt (t))
        val result = rewrite (s) (t)
        assertResult (B ()) (result)
        assertResult (true) (s.hasBeenComputedAt (t))
        assertSame (result) (rewrite (s) (t))
        s.reset ()
        assertResult (false) (s.hasBeenComputedAt (t))
        assertResult (B ()) (rewrite (s) (t))
        assertResult (true) (s.hasBeenComputedAt (t))
    }

    test ("resetting all memoising strategies actually does reset them") {
        val r = atob.asInstanceOf[MemoStrategy]
        val s = everywheretd (atob).asInstanceOf[MemoStrategy]
        val t = P (A (), A ())
        assertResult (false) (s.hasBeenComputedAt (t))
        assertResult (false) (r.hasBeenComputedAt (t.l))
        assertResult (false) (r.hasBeenComputedAt (t.r))
        assertResult (P (B (), B())) (rewrite (s) (t))
        assertResult (true) (s.hasBeenComputedAt (t))
        assertResult (true) (r.hasBeenComputedAt (t.l))
        assertResult (true) (r.hasBeenComputedAt (t.r))
        resetMemo ()
        assertResult (false) (s.hasBeenComputedAt (t))
        assertResult (false) (r.hasBeenComputedAt (t.l))
        assertResult (false) (r.hasBeenComputedAt (t.r))
        assertResult (P (B (), B())) (rewrite (s) (t))
    }

    /**
     * Generate tests that ensure that if a node is rewritten more than once
     * in a traversal by `strat` the results at each stage will be shared in
     * the overall result.
     */
    def testSharingRewrite (direction : String, strat : Strategy) {

        val s = P (A (), A ())
        val t = P (s, s)
        val u = P (t, t)
        val result = rewrite (strat) (u)

        test (s"memo rewriting produces correct term ($direction)") {
            val expected = P(P(P(B(),B()),P(B(),B())),P(P(B(),B()),P(B(),B())))
            assertResult (expected) (result)
        }

        test (s"memo rewriting preserves top-level sharing ($direction)") {
            assertSame (result.l) (result.r)
        }

        test (s"memo rewriting preserves second-level sharing ($direction)") {
            val lp = result.l.asInstanceOf[P]
            val rp = result.r.asInstanceOf[P]
            assertSame (lp.l) (lp.r)
            assertSame (lp.r) (rp.l)
            assertSame (rp.l) (rp.r)
        }

        test (s"memo rewriting preserves third-level sharing ($direction)") {
            val lls = result.l.asInstanceOf[P].l.asInstanceOf[P]
            val lrs = result.l.asInstanceOf[P].r.asInstanceOf[P]
            val rls = result.r.asInstanceOf[P].l.asInstanceOf[P]
            val rrs = result.r.asInstanceOf[P].r.asInstanceOf[P]
            assertSame (lls.l) (lrs.l)
            assertSame (lrs.l) (rls.l)
            assertSame (rls.l) (rrs.l)
            assertSame (lls.r) (lrs.r)
            assertSame (lrs.r) (rls.r)
            assertSame (rls.r) (rrs.r)
        }

    }

    testSharingRewrite ("bottom-up", everywherebu (atob))
    testSharingRewrite ("top-down", everywheretd (atob))

    {
        // Test based on Eric Torreborre's node fusion example from Scoobi

        val tta = T (T (A (), 1), 2)
        val root = P (T (tta, 1), T (T (tta, 1), 1))

        val fuseT =
            rule[N] {
                case T (n, 1) => S (n)
            }

        val fuse = everywheretd (fuseT)

        test ("conditional node fusion preserves sharing") {
            val expected = P(S(T(S(A()),2)),S(S(T(S(A()),2))))
            val result = rewrite (fuse) (root)
            assertResult (expected) (result)
            val resultls = result.l.asInstanceOf[S]
            val resultrss = result.r.asInstanceOf[S].n.asInstanceOf[S]
            assertSame (resultls.n) (resultrss.n)
        }
    }

}

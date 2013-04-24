/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013 Anthony M Sloane, Macquarie University.
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

abstract class N
case class A () extends N
case class B () extends N
case class S (n : N) extends N
case class T (n : N, uses : Int) extends N
case class P (l : N, r : N) extends N

/**
 * Tests of memoising rewriting.
 */
class MemoRewriterTests extends {

    override val rewriter = MemoRewriter

} with RewriterTests {

    import rewriter.{test => rwtest, _}

    val atob =
        rule {
            case A () => B ()
        }

    test ("a memoising strategy actually memoises") {
        val s = everywhere (atob).asInstanceOf[MemoStrategy]
        val t : N = A ()
        expectResult (false) (s.hasBeenComputedAt (t))
        val result = rewrite (s) (t)
        expectResult (B ()) (result)
        expectResult (true) (s.hasBeenComputedAt (t))
        expectsame (result) (rewrite (s) (t))
        s.reset ()
        expectResult (false) (s.hasBeenComputedAt (t))
        expectResult (B ()) (rewrite (s) (t))
        expectResult (true) (s.hasBeenComputedAt (t))
    }

    test ("resetting all memoising strategies actually does reset them") {
        val r = atob.asInstanceOf[MemoStrategy]
        val s = everywheretd (atob).asInstanceOf[MemoStrategy]
        val t = P (A (), A ())
        expectResult (false) (s.hasBeenComputedAt (t))
        expectResult (false) (r.hasBeenComputedAt (t.l))
        expectResult (false) (r.hasBeenComputedAt (t.r))
        expectResult (P (B (), B())) (rewrite (s) (t))
        expectResult (true) (s.hasBeenComputedAt (t))
        expectResult (true) (r.hasBeenComputedAt (t.l))
        expectResult (true) (r.hasBeenComputedAt (t.r))
        resetMemo ()
        expectResult (false) (s.hasBeenComputedAt (t))
        expectResult (false) (r.hasBeenComputedAt (t.l))
        expectResult (false) (r.hasBeenComputedAt (t.r))
        expectResult (P (B (), B())) (rewrite (s) (t))
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
            expectResult (expected) (result)
        }

        test (s"memo rewriting preserves top-level sharing ($direction)") {
            expectsame (result.l) (result.r)
        }

        test (s"memo rewriting preserves second-level sharing ($direction)") {
            val lp = result.l.asInstanceOf[P]
            val rp = result.r.asInstanceOf[P]
            expectsame (lp.l) (lp.r)
            expectsame (lp.r) (rp.l)
            expectsame (rp.l) (rp.r)
        }

        test (s"memo rewriting preserves third-level sharing ($direction)") {
            val lls = result.l.asInstanceOf[P].l.asInstanceOf[P]
            val lrs = result.l.asInstanceOf[P].r.asInstanceOf[P]
            val rls = result.r.asInstanceOf[P].l.asInstanceOf[P]
            val rrs = result.r.asInstanceOf[P].r.asInstanceOf[P]
            expectsame (lls.l) (lrs.l)
            expectsame (lrs.l) (rls.l)
            expectsame (rls.l) (rrs.l)
            expectsame (lls.r) (lrs.r)
            expectsame (lrs.r) (rls.r)
            expectsame (rls.r) (rrs.r)
        }

    }

    testSharingRewrite ("bottom-up", everywherebu (atob))
    testSharingRewrite ("top-down", everywheretd (atob))

    {
        // Test based on Eric Torreborre's node fusion example from Scoobi

        val tta = T (T (A (), 1), 2)
        val root = P (T (tta, 1), T (T (tta, 1), 1))

        val fuseT =
            rule {
                case T (n, 1) => S (n)
            }

        val fuse = everywheretd (fuseT)

        test ("conditional node fusion preserves sharing") {
            val expected = P(S(T(S(A()),2)),S(S(T(S(A()),2))))
            val result = rewrite (fuse) (root)
            expectResult (expected) (result)
            val resultls = result.l.asInstanceOf[S]
            val resultrss = result.r.asInstanceOf[S].n.asInstanceOf[S]
            expectsame (resultls.n) (resultrss.n)
        }
    }

}

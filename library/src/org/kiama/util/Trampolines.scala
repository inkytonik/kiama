/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014 Anthony M Sloane, Macquarie University.
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
package util

/**
 * Trampolines. Useful for converting stack-heavy operations into heap-based
 * ones and thereby avoiding stack overflow.
 *
 * Based on code from "Stackless Scala With Free Monads", Runar Oli Bjarnason.
 */
object Trampolines {

    /**
     * A computation that produces an `A`.
     */
    sealed abstract class Trampoline[+A] {

        import scala.annotation.tailrec

        /**
         * Run this computation to produce its `A`. The key idea is that this
         * method is directly tail recursive so the Scala compiler can convert
         * it into a loop.
         */
        @tailrec
        final def runT : A =
            resume match {
                case Left (k)  => k ().runT
                case Right (a) => a
            }

        @tailrec
        final def resume : Either[() => Trampoline[A], A] =
            this match {
                case Done(v)       => Right (v)
                case More(k)       => Left (k)
                case FlatMap (a,f) =>
                    a match {
                        case Done (v)     => f (v).resume
                        case More (k)     => Left (() => k () flatMap f)
                        case FlatMap(b,g) => b.flatMap ((x : Any) => g (x) flatMap f).resume
                    }
            }

        def flatMap[B] (f : A => Trampoline[B]) : Trampoline[B] =
            this match {
                case FlatMap (a, g) =>
                    FlatMap (a, (x : Any) => g (x) flatMap f)
                case x =>
                    FlatMap (x, f)
            }

        def map[B] (f : A => B) : Trampoline[B] =
            flatMap (a => Done (f (a)))

    }

    /**
     * A computation whose value is `a`.
     */
    case class Done[+A] (a : A) extends Trampoline[A]

    /**
     * A computation whose value is obtained from the computation that is
     * returned by the continuation `k`.
     */
    case class More[+A] (k : () => Trampoline[A]) extends Trampoline[A]

    /**
     * A computation whose value is obtained by first runnning `ta` then
     * passing its value to the continutation `k` to get the computation
     * that provides the final value.
     */
    case class FlatMap[A,+B] (ta : Trampoline[A], k : A => Trampoline[B]) extends Trampoline[B]

    /**
     * A computation that returns `a` when it is eventually run.
     */
    def step[A] (a : => A) : Trampoline[A] =
        More (() => Done (a))

}

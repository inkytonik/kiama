/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2015-2016 Anthony M Sloane, Macquarie University.
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

package org.bitbucket.inkytonik.kiama
package parsing

/**
 * Parse results.
 */
sealed abstract class ParseResult[+T] {

    def next : Input

    def append[U >: T](r : => ParseResult[U]) : ParseResult[U]

    def flatMapWithNext[U](f : T => Input => ParseResult[U]) : ParseResult[U]

    def map[U](f : T => U) : ParseResult[U]

}

/**
 * A successful parse result.
 */
case class Success[+T](result : T, next : Input) extends ParseResult[T] {

    def append[U >: T](r : => ParseResult[U]) : ParseResult[U] =
        this

    def flatMapWithNext[U](f : T => Input => ParseResult[U]) : ParseResult[U] =
        f(result)(next)

    def map[U](f : T => U) : ParseResult[U] = {
        val u = f(result)
        Success(u, next)
    }

}

/**
 * A failure parse result.
 */
case class Failure(message : String, next : Input) extends ParseResult[Nothing] {

    def append[U >: Nothing](r : => ParseResult[U]) : ParseResult[U] = {
        val rr = r
        rr match {
            case _ : Failure =>
                if (rr.next.offset < next.offset)
                    this
                else
                    rr
            case _ =>
                rr
        }
    }

    def flatMapWithNext[U](f : Nothing => Input => ParseResult[U]) : ParseResult[U] =
        this

    def map[U](f : Nothing => U) : ParseResult[U] =
        this

}


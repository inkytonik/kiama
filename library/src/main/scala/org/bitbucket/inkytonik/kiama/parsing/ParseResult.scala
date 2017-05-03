/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2015-2017 Anthony M Sloane, Macquarie University.
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

    def kind : String

    def toMessage : String

    def next : Input

    def append[U >: T](r : => ParseResult[U]) : ParseResult[U]

    def flatMapWithNext[U](f : T => Input => ParseResult[U]) : ParseResult[U]

    def map[U](f : T => U) : ParseResult[U]

}

/**
 * A successful parse result.
 */
case class Success[+T](result : T, next : Input) extends ParseResult[T] {

    val kind = "success"

    def toMessage : String =
        s"""Parse $kind at ${next.format}"""

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
 * All parse results that are not successful.
 */
sealed abstract class NoSuccess(val message : String, val next : Input) extends ParseResult[Nothing] {

    def toMessage : String =
        s"""Parse $kind with message "$message" at ${next.format}"""

    def flatMapWithNext[U](f : Nothing => Input => ParseResult[U]) : ParseResult[U] =
        this

    def map[U](f : Nothing => U) : ParseResult[U] =
        this

}

/**
 * Support for NoSuccess.
 */
object NoSuccess {

    def unapply[T](r : ParseResult[T]) : Option[(String, Input)] =
        r match {
            case Error(m, n)   => Some((m, n))
            case Failure(m, n) => Some((m, n))
            case _             => None
        }

}

/**
 * An error parse result. Parsers that error do not backtrack.
 */
case class Error(override val message : String, override val next : Input) extends NoSuccess(message, next) {

    val kind = "error"

    def append[U >: Nothing](r : => ParseResult[U]) : ParseResult[U] =
        this

}

/**
 * A failure parse result.
 */
case class Failure(override val message : String, override val next : Input) extends NoSuccess(message, next) {

    val kind = "failure"

    def append[U >: Nothing](r : => ParseResult[U]) : ParseResult[U] = {
        val rr = r
        rr match {
            case _ : NoSuccess =>
                if (rr.next.offset < next.offset)
                    this
                else
                    rr
            case _ =>
                rr
        }
    }

}

/**
 * This file is part of Kiama.
 *
 * Copyright (C) 2011 Anthony M Sloane, Macquarie University.
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

import org.scalatest.FunSuite
import scala.util.parsing.combinator.RegexParsers

/**
 * General test support.
 */
trait Tests extends FunSuite {

    import org.kiama.util.Messaging._
    
    /**
     * Compare two values.  Use reference equality for references
     * and value equality for non-references.  If the values are
     * both Some values, perform the check on the wrapped values.
     */
    def same (v1 : Any, v2 : Any) : Boolean =
        (v1, v2) match  {
            case (Some (r1 : AnyRef), Some (r2 : AnyRef)) => r1 eq r2
            case (Some (v1), Some (v2))                   => v1 == v2
            case (None, None)                             => true
            case (r1 : AnyRef, r2 : AnyRef)               => r1 eq r2
            case _                                        => v1 == v2
        }
        
    /**
     * Analogous to ScalaTest's expect but it uses same to compare
     * the two values instead of equality.
     */
    def expectsame (expected : Any) (actual : Any) {
        if (!same (expected, actual)) {
            fail ("Expected same object as " + expected + ", but got " + actual)
        }
    }

    /**
     * Analogous to ScalaTest's expect but it uses same to compare
     * the two values instead of equality.
     */
    def expectnotsame (expected : Option[Any]) (actual : Option[Any]) {
        if (same (expected, actual)) {
            fail ("Expected not same object as " + expected + ", but got " + actual)
        }
    }

    /**
     * Assert that a message was produced at a given position.
     */
    def assertMessage (index : Int, line : Int, column : Int, msg : String) {
        val m = messages (index)
        expect (line, "wrong line number in message " + index) (m.pos.line)
        expect (column, "wrong column number in message " + index) (m.pos.column)
        expect (msg, "wrong text in message " + index) (m.message)
    }

}

/**
 * Useful test routines for RegexParsers.
 */
trait RegexParserTests extends Tests {
    
    self : RegexParsers =>

    /**
     * Try to parse str as a T, which is expected to work and produce the
     * given result.  Assert a failure if it doesn't or if it doesn't 
     * consume all of the input.
     */
    def assertParseOk[T] (str : String, p : Parser[T], result : T) {
        parseAll (p, str) match {
            case Success (r, in) =>
                if (r != result) fail ("found '" + r + "' not '" + result + "'")
                if (!in.atEnd) fail ("input remaining at " + in.pos)
            case f =>
                fail ("parse failure: " + f)
        }
    }

    /**
     * Try to parse str as a T, which is expected to fail.  Fail if it
     * doesn't.
     */
    def assertParseError[T] (str : String, p : Parser[T], line : Int, column : Int,
                             msg : String) {
        parseAll (p, str) match {
            case Success (_, _) =>
                fail ("expected to find parse error in " + str)
            case e : NoSuccess =>
                expect (line, "wrong line number in error") (e.next.pos.line)
                expect (column, "wrong column number in error") (e.next.pos.column)
                expect (msg, "wrong message in error") (e.msg)
        }
    }

}

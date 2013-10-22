/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2013 Anthony M Sloane, Macquarie University.
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

import org.scalatest.{BeforeAndAfter, FunSuiteLike}
import scala.util.parsing.combinator.RegexParsers

/**
 * General test support.
 */
trait Tests extends FunSuiteLike with BeforeAndAfter {

    import org.scalatest.Tag

    /**
     * Compare two values.  Use reference equality for references
     * and value equality for non-references.  If the values are
     * both Some values, perform the check on the wrapped values.
     */
    def same (v1 : Any, v2 : Any) : Boolean =
        (v1, v2) match  {
            case (Some (r1 : AnyRef), Some (r2 : AnyRef)) => r1 eq r2
            case (None, None)                             => true
            case (null, null)                             => true
            case (r1 : AnyRef, r2 : AnyRef)               => r1 eq r2
            case _ =>
                sys.error ("Tests.same: unexpected case: %s, %s".format (v1, v2))
        }

    /**
     * Fail a test with a message about finding something and expecting
     * something else.
     */
    def failExpectedTest[T] (expected : T, found : T, description : String = "") {
        fail ("expected %s'%s', not '%s'".format (description, expected, found))
    }

    /**
     * Analogous to ScalaTest's expect but it uses same to compare
     * the two values instead of equality.
     */
    def expectsame (expected : Any) (actual : Any) {
        if (!same (expected, actual)) {
            failExpectedTest (expected, actual, "same object as ")
        }
    }

    /**
     * Analogous to ScalaTest's expect but it uses same to compare
     * the two values instead of equality.
     */
    def expectnotsame (expected : Any) (actual : Any) {
        if (same (expected, actual)) {
            failExpectedTest (expected, actual, "not same object as ")
        }
    }

    /**
     * Assert that the given messsaging object has recorded the given messages.
     */
    def assertMessages (messaging : Messaging, messages : (Int, Message)*) {
        assert (messaging.messagecount === messages.size, "Wrong number of messages produced")
        for ((index, message) <- messages)
            assertMessage (messaging, index, message)
    }

    /**
     * Assert that a message at `index` was produced at a given position.
     */
    def assertMessage (messaging : Messaging, index : Int, message : Message) {
        val m = messaging.messages (index)
        assertResult (message.label, s"wrong text in message $index") (m.label)
        assertResult (message.line, s"wrong line number in message $index") (m.line)
        assertResult (message.column, s"wrong column number in message $index") (m.column)
    }

    /**
     * A ScalaTest tag that enables us to focus attention on particular tests
     * rather than running all of them each time. Add this as an argument to
     * the particular test methods that you want to focus on. Then you can
     * use an sbt command such as "test-only *RewriterTests -- -n FocusTest"
     * to run just the tests in that suite with this tag.
     */
    object FocusTest extends Tag ("FocusTest")

}

/**
 * Useful test routines for RegexParsers.
 */
trait RegexParserTests extends Tests {

    self : RegexParsers =>

    /**
     * Fail a test with a message about reaching the end of the input.
     */
    def failInputEnd (in : Input) {
        fail (s"input remaining at ${in.pos}")
    }

    /**
     * Fail a test with a message detailing a parse error.
     */
    def failParseError (f : Error) {
        fail (s"parse error: $f")
    }

    /**
     * Fail a test with a message detailing a parse failure.
     */
    def failParseFailure (f : Failure) {
        fail (s"parse faiure: $f")
    }

    /**
     * Assert that a parsing operation should be performed correctly.
     * Try to parse `str` as a `T` using the parser `p`, which is expected
     * to succeed and to produce the given result.  Fail if `p` doesn't
     * produce the given result or if `p` doesn't consume all of the input.
     */
    def assertParseOk[T] (str : String, p : Parser[T], result : T) {
        parseAll (p, str) match {
            case Success (r, in) =>
                if (r != result) failExpectedTest (result, r)
                if (!in.atEnd) failInputEnd (in)
            case f : Error =>
                failParseError (f)
            case f : Failure =>
                failParseFailure (f)
        }
    }

    /**
     * Assert that a parsing operation should not result in success.
     * Try to parse `str` as a `T` using the parser `p`, which is expected
     * to not succeed, giving either a fatal error or failure (as specified
     * by the `iserr` parameter, which defaults to failure). Fail the test
     * if the parsing operation succeeds. Furthermore, fail the test if it
     * fails, but the error or failure is not indicated at the given `line`
     * and `column` location or doesn't contain the given message `msg`.
     */
    def assertParseError[T] (str : String, p : Parser[T], line : Int, column : Int,
                             msg : String, iserr : Boolean = false) {
        parseAll (p, str) match {
            case Success (r, _) =>
                fail ("expected to find parse error in %s but it succeeded with %s".format (str, r))
            case e : NoSuccess =>
                if (iserr && e.isInstanceOf[Failure])
                    fail ("got parse failure when expecting parse error")
                else if (!iserr & e.isInstanceOf[Error])
                    fail ("got parse error when expecting parse failure")
                assertResult (msg, "wrong message in error") (e.msg)
                assertResult (line, "wrong line number in error") (e.next.pos.line)
                assertResult (column, "wrong column number in error") (e.next.pos.column)
        }
    }

}

/**
 * Useful test routines for transformers.
 */
trait TransformerTests extends RegexParserTests {

    self : RegexParsers =>

    /**
     * Assert that a transformation should be performed correctly.
     * Try to parse `str` as a `T` using the parser `p`, which is expected
     * to succeed while consuming all of the input. Then pass the resulting
     * `T` to the `t` transformation function. Fail the test if the value
     * produced by the transformation is not `result`.
     */
    def assertTransformOk[T] (str : String, p : Parser[T], t : T => T, result : T) {
        parseAll (p, str) match {
            case Success (r, in) =>
                if (!in.atEnd) failInputEnd (in)
                val newr = t (r)
                if (newr != result) failExpectedTest (result, newr)
            case f : Error =>
                failParseError (f)
            case f : Failure =>
                failParseFailure (f)
        }
    }

}



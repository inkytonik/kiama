/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2014 Anthony M Sloane, Macquarie University.
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

import org.scalatest.prop.Checkers
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, FunSuiteLike}
import scala.util.parsing.combinator.RegexParsers

/**
 * General test support.
 */
trait Tests extends FunSuiteLike with BeforeAndAfter with BeforeAndAfterAll with Checkers {

    import Comparison.{optsame, same, samecollection}
    import Messaging.Messages
    import org.scalatest.Tag
    import scala.collection.immutable.Seq

    /**
     * ScalaTest by default only shows the unqualified class name when
     * it displays the name of the suite. If a suite class name is
     * used in more than one package we can't tell them apart. Here
     * we override the name that is printed so that we get a project
     * relative source file name as well.
     *
     * This definition assumes that the test suite resides in the
     * library project, that the name of the suite class is the same
     * as the basename of the file and that the file is located in
     * the folder given by the package name.
     */
    override def suiteName = {
        val filename = getClass.getName.replaceAllLiterally (".", "/")
        s"${super.suiteName} in library/src/$filename"
    }

    /**
     * Fail a test with a message about finding something and expecting
     * something else.
     */
    def failExpectedTest[T] (expected : T, found : T, description : String = "") {
        fail ("expected %s'%s', not '%s'".format (description, expected, found))
    }

    /**
     * Analogous to ScalaTest's `assertResult` but it uses `same` to compare
     * the two values instead of equality.
     */
    def assertSame (expected : Any) (actual : Any) {
        if (!same (expected, actual)) {
            failExpectedTest (expected, actual, "same object as ")
        }
    }

    /**
     * Analogous to ScalaTest's `assertResult` but it uses `same` to compare
     * the two values instead of equality.
     */
    def assertNotSame (expected : Any) (actual : Any) {
        if (same (expected, actual)) {
            failExpectedTest (expected, actual, "not same object as ")
        }
    }

    /**
     * Analogous to ScalaTest's `assertResult` but it uses `optsame` to compare
     * the two values instead of equality.
     */
    def assertOptSame (expected : Any) (actual : Any) {
        if (!optsame (expected, actual)) {
            failExpectedTest (expected, actual, "same object as ")
        }
    }

    /**
     * Analogous to ScalaTest's `assertResult` but it uses `optsame` to compare
     * the two values instead of equality.
     */
    def assertNotOptSame (expected : Any) (actual : Any) {
        if (optsame (expected, actual)) {
            failExpectedTest (expected, actual, "not same object as ")
        }
    }

    /**
     * Analogous to ScalaTest's `assertResult` but it uses `samecollection` to compare
     * two collections instead of equality.
     */
    def assertSameCollection (expected : Any) (actual : Any) {
        if (!samecollection (expected, actual)) {
            failExpectedTest (expected, actual, "same collection as ")
        }
    }

    /**
     * Analogous to ScalaTest's `assertResult` but it uses `samecollection` to compare
     * two collections instead of equality.
     */
    def assertNotSameCollection (expected : Any) (actual : Any) {
        if (samecollection (expected, actual)) {
            failExpectedTest (expected, actual, "not same collection as ")
        }
    }

    /**
     * Assert that the `received` list of messsages has recorded the `expected`
     * messages in the same order.
     */
    def assertMessages (received : Messages, expected : Message*) {
        assert (received.size === expected.size, "wrong number of messages produced")
        received.zip (expected).zipWithIndex.map {
            case ((rec, exp), i) =>
                assertMessage (rec, i, exp)
        }
    }

    /**
     * Assert that a `received` message at the given zero-based `index` conforms
     * to an expected one in that it reports the same message label at the same
     * position.
     */
    def assertMessage (received : Message, index : Int, expected : Message) {
        assertResult (expected.label, s"wrong text in message $index") (received.label)
        assertResult (expected.line, s"wrong line number in message $index") (received.line)
        assertResult (expected.column, s"wrong column number in message $index") (received.column)
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
    def failParseError (error : Error) {
        fail (s"parse error: $error")
    }

    /**
     * Fail a test with a message detailing a parse failure.
     */
    def failParseFailure (failure : Failure) {
        fail (s"parse faiure: $failure")
    }

    /**
     * Parse a string and if the parse succeeds, pass the result of the parse
     * to a function for further processing or checking. `str` is the string to
     * be parsed and `parser` is the parser to parse it with. `func` accepts the
     * result value of the parse and returns whatever it likes which is returned
     * from `assertParseCheck`. Fail if the parse succeeds but doesn't consume
     * all of `str` or if the parse fails.
     */
    def assertParseCheck[T,U] (str : String, parser : Parser[T]) (func : T => U) : U =
        parseAll (parser, str) match {
            case Success (value, in) if in.atEnd =>
                func (value)
            case Success (_, in) =>
                fail (s"extraneous input at ${in.pos}: $str")
            case f =>
                fail (s"parse failure: $f")
        }

    /**
     * Assert that a parsing operation should be performed correctly.
     * Try to parse `str` as a `T` using `parser`, which is expected
     * to succeed and to produce the `expected` value.  Fail if `p` doesn't
     * produce the expected value or if `parser` doesn't consume all of the
     * input.
     */
    def assertParseOk[T] (str : String, parser : Parser[T], expected : T) {
        assertParseCheck (str, parser) {
            result =>
                if (expected != result)
                    failExpectedTest (expected, result)
        }
    }

    /**
     * Assert that a parsing operation should not result in success.
     * Try to parse `str` as a `T` using `parser`, which is expected
     * to not succeed, giving either a fatal error or failure (as specified
     * by the `iserr` parameter, which defaults to failure). Fail the test
     * if the parsing operation succeeds. Furthermore, fail the test if it
     * fails, but the error or failure is not indicated at the given `line`
     * and `column` location or doesn't contain the given message `msg`.
     */
    def assertParseError[T] (str : String, parser : Parser[T], line : Int,
                             column : Int, msg : String, iserr : Boolean = false) {
        parseAll (parser, str) match {
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

    /**
     * Parse a string and if the parse succeeds, return the result of the parse.
     * `str` is the string to be parsed and `parser` is the parser to parse it
     * with.
     */
    def assertParseReturn[T] (str : String, parser : Parser[T]) : T =
        assertParseCheck (str, parser) (identity)

}

/**
 * Useful test routines for transformers.
 */
trait TransformerTests extends RegexParserTests {

    self : RegexParsers =>

    /**
     * Assert that a transformation should be performed correctly. Try to parse
     * `str` as a `T` using the parser `parser`, which is expected to succeed
     * while consuming all of the input. Then pass the resulting `T` to the
     * `trans` transformation function. Fail the test if the value produced by
     * the transformation is not `expected`.
     */
    def assertTransformOk[T] (str : String, parser : Parser[T], trans : T => T, expected : T) {
        assertParseCheck (str, parser) {
            result =>
                val transformed = trans (result)
                if (transformed != expected) failExpectedTest (expected, transformed)
        }
    }

}



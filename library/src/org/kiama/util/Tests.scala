/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2015 Anthony M Sloane, Macquarie University.
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

import org.kiama.output.PrettyPrinter
import org.scalatest.prop.Checkers
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, BeforeAndAfterEach, FunSuiteLike}

/**
 * General test support designed to be mixed in to compilers or drivers.
 */
trait Tests extends FunSuiteLike with BeforeAndAfter with BeforeAndAfterAll
        with BeforeAndAfterEach with Checkers with PositionStore with Messaging {

    import Comparison.{optsame, same, samecollection}
    import Messaging.Messages
    import org.scalatest.Tag

    /**
     * Initialise positions before next test. By default, the positions are reset.
     */
    def initialisePositions () {
        positions.reset ()
    }

    /**
     * Set up before each test.
     */
    override def beforeEach () {
        initialisePositions ()
    }

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
        s"${super.suiteName} in src/$filename"
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
        assert (received.size == expected.size, "wrong number of messages produced")
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
        assertResult (line (expected), s"wrong line number in message $index") (line (received))
        assertResult (column (expected), s"wrong column number in message $index") (column (received))
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
 * Infrastructure for writing parser tests.
 */
trait ParseTests extends Tests {

    import org.kiama.parsing.{Failure, Input, ParsersBase, Success}
    import org.kiama.util.StringSource

    /**
     * The suite of parsers that is used by these tests.
     */
    val parsers : ParsersBase

    /**
     * Fail a test with a message about reaching the end of the input.
     */
    def failInputEnd (in : Input) {
        fail (s"input remaining at ${in.position}")
    }

    /**
     * Fail a test with a message detailing a parse failure.
     */
    def failParseFailure (failure : Failure) {
        fail (s"parse faiure: ${failure.message}")
    }

    /**
     * Parse a string and if the parse succeeds, pass the result of the parse
     * to a function for further processing or checking. `str` is the string to
     * be parsed and `parser` is the parser to parse it with. `func` accepts the
     * result value of the parse and returns whatever it likes which is returned
     * from `assertParseCheck`. Fail if the parse succeeds but doesn't consume
     * all of `str` or if the parse fails.
     */
    def assertParseCheck[T,U] (str : String, parser : parsers.Parser[T]) (func : T => U) : U = {
        parsers.parseAll (parser, StringSource (str)) match {
            case Success (value, in) if in.atEnd =>
                func (value)
            case Success (_, in) =>
                fail (s"extraneous input at ${in.position}: $str")
            case Failure (message, _) =>
                fail (s"parse failure: $message")
        }
    }

    /**
     * Assert that a parsing operation should be performed correctly.
     * Try to parse `str` as a `T` using `parser`, which is expected
     * to succeed and to produce the `expected` value.  Fail if `p` doesn't
     * produce the expected value or if `parser` doesn't consume all of the
     * input.
     */
    def assertParseOk[T] (str : String, parser : parsers.Parser[T], expected : T) {
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
    def assertParseError[T] (str : String, parser : parsers.Parser[T], line : Int,
                             column : Int, msg : String, iserr : Boolean = false) {
        parsers.parseAll (parser, StringSource (str)) match {
            case Success (r, _) =>
                fail ("expected to find parse error in %s but it succeeded with %s".format (str, r))
            case Failure (message, next) =>
                val pos = next.position
                assertResult (msg, "wrong message in error") (message)
                assertResult (line, "wrong line number in error") (pos.line)
                assertResult (column, "wrong column number in error") (pos.column)
        }
    }

    /**
     * Parse a string and if the parse succeeds, return the result of the parse.
     * `str` is the string to be parsed and `parser` is the parser to parse it
     * with.
     */
    def assertParseReturn[T] (str : String, parser : parsers.Parser[T]) : T =
        assertParseCheck (str, parser) (identity)

}

/**
 * Useful test routines for transformers.
 */
trait TransformerTests extends ParseTests {

    /**
     * Assert that a transformation should be performed correctly. Try to parse
     * `str` as a `T` using the parser `parser`, which is expected to succeed
     * while consuming all of the input. Then pass the resulting `T` to the
     * `trans` transformation function. Fail the test if the value produced by
     * the transformation is not `expected`.
     */
    def assertTransformOk[T] (str : String, parser : parsers.Parser[T], trans : T => T, expected : T) {
        assertParseCheck (str, parser) {
            result =>
                val transformed = trans (result)
                if (transformed != expected) failExpectedTest (expected, transformed)
        }
    }

}

/**
 * Useful test routines for pretty-printers.
 */

trait PrettyPrinterTests extends Tests {

    import org.kiama.output.PrettyPrinterTypes.{Document, Layout, Link, Links}

    /**
     * Assert that a doc when pretty-printed has the given layout.
     */
    def assertLayout (expected : Layout) (document : Document) {
        assertResult (expected) (document.layout)
    }

    /**
     * Assert that a doc when pretty-printed has the given links.
     */
    def assertLinks (expected : List[(AnyRef,Range)]) (document : Document) {
        for ((v, r) <- expected) {
            assertLink (r) (document.links, v)
        }
    }

    /**
     * Assert that a value has a given link in a links map.
     */
    def assertLink (expected : Range) (links : Links, value : AnyRef) {
        val optRange = links.collectFirst {
                                 case Link (k, v) if k eq value =>
                                     v
                             }
        optRange match {
            case Some (r) =>
                assertResult (expected, s"for value $value") (r)
            case None =>
                fail (s"link for $value not found")
        }
    }

}

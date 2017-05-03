/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2017 Anthony M Sloane, Macquarie University.
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
package util

import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, BeforeAndAfterEach, FunSuiteLike, Matchers}
import org.scalatest.matchers.{Matcher, MatchResult}
import org.scalatest.prop.Checkers

/**
 * General test support designed to be mixed in to compilers or drivers.
 */
trait Tests extends FunSuiteLike with BeforeAndAfter with BeforeAndAfterAll
        with BeforeAndAfterEach with Checkers with Matchers with PositionStore
        with Messaging {

    import Comparison.{same, sameCollection, sameElements}
    import org.scalatest.Tag

    /**
     * Initialise positions before next test. By default, the positions are reset.
     */
    def initialisePositions() {
        positions.reset()
    }

    /**
     * Set up before each test.
     */
    override def beforeEach() {
        initialisePositions()
    }

    /**
     * ScalaTest by default only shows the unqualified class name when
     * it displays the name of the suite. If a suite class name is
     * used in more than one package we can't tell them apart. Here
     * we override the name that is printed so that we get a project
     * relative source directory as well.
     */
    override def suiteName : String =
        s"${super.suiteName} in $suiteSourcePath"

    /**
     * The path from the main source directory to the directory that holds
     * this suite. By default it's the directory given by the suite class's
     * package name. E.g., package `foo.bar.ble` will be in `foo/bar/ble`.
     */
    def suitePackagePath : String = {
        val pkgName = Option(getClass.getPackage).map(_.getName).getOrElse("")
        pkgName.replaceAllLiterally(".", "/")
    }

    /**
     * Path to the source folder that contains this suite. By default,
     * `library/src` with `suitePackagePath` appended.
     */
    def suiteSourcePath : String =
        s"library/src/test/scala/$suitePackagePath"

    /**
     * Matcher for being the same collection, i.e., equal and containing
     * the same elements in the same order. Also works on pairs of values
     * and tuples.
     */
    def beSameCollectionAs(expected : Any) =
        new Matcher[Any] {
            def apply(value : Any) =
                MatchResult(
                    (value == expected) && sameCollection(value, expected),
                    s""""$value" is not same collection as "$expected"""",
                    s""""$value" is same collection as "$expected""""
                )
        }

    /**
     * Matcher for being a collection with the same elements.
     */
    def haveSameElementsAs[T](expected : Seq[T]) =
        new Matcher[Seq[T]] {
            def apply(value : Seq[T]) =
                MatchResult(
                    sameElements(value, expected),
                    s""""$value" does not have same elements as "$expected"""",
                    s""""$value" has same elements as "$expected""""
                )
        }

    /**
     * Matcher for same options.
     */
    def beSameOptionAs(expected : Option[Any]) =
        new Matcher[Option[Any]] {
            def apply(value : Option[Any]) =
                MatchResult(
                    (expected == None && value == None) || same(value.get, expected.get),
                    s""""$value" is not same Option as "$expected"""",
                    s""""$value" is same Option as "$expected""""
                )
        }

    /**
     * Matcher for being a `Some` containing an expected object. The difference
     * to just comparing with with `Some(x)` is that this matcher makes sure
     * the wrapped object is identical to the expected value, not just equal
     * to it.
     */
    def beSomeOf(expected : Any) =
        new Matcher[Option[Any]] {
            def apply(value : Option[Any]) =
                MatchResult(
                    !value.isEmpty && same(value.get, expected),
                    s""""$value" is not same Some as "$expected"""",
                    s""""$value" is same Some as "$expected""""
                )
        }

    /**
     * Matcher for use of options where `None` represents failure.
     */
    val beFailure =
        new Matcher[Option[Any]] {
            def apply(value : Option[Any]) =
                MatchResult(
                    value.isEmpty,
                    s""""$value" is not failure""",
                    s"""failure was seen"""
                )
        }

    /**
     * A ScalaTest tag that enables us to focus attention on particular tests
     * rather than running all of them each time. Add this as an argument to
     * the particular test methods that you want to focus on. Then you can
     * use an sbt command such as "test-only *RewriterTests -- -n FocusTest"
     * to run just the tests in that suite with this tag.
     */
    object FocusTest extends Tag("FocusTest")

}

/**
 * Infrastructure for writing parser tests.
 */
trait ParseTests extends Tests {

    import org.bitbucket.inkytonik.kiama.parsing.{
        Error,
        Failure,
        NoSuccess,
        ParseResult,
        ParsersBase,
        Success
    }

    /**
     * The suite of parsers that is used by these tests.
     */
    val parsers : ParsersBase

    /**
     * Matcher for parse success yielding an expected value.
     */
    def parseTo[T](expected : T) =
        new Matcher[ParseResult[T]] {
            def apply(result : ParseResult[T]) =
                result match {
                    case Success(value, in) =>
                        MatchResult(
                            (value == expected),
                            s"""Parse succeeded with "$value" instead of expected "$expected" at ${in.format}""",
                            s"""Parse succeeded with disallowed value "$value" at ${in.format}"""
                        )
                    case result : NoSuccess =>
                        MatchResult(false, result.toMessage, "NOT USED")
                }
        }

    /**
     * Matcher for parse success where the parsed value corresponds to
     * particluar input text.
     */
    def parseText[T](expected : String) =
        new Matcher[ParseResult[T]] {
            def apply(result : ParseResult[T]) =
                result match {
                    case Success(value, in) =>
                        val matched = positions.textOf(value)
                        MatchResult(
                            matched == Some(expected),
                            s"""Parse succeeded matching "$matched" instead of expected "Some($expected)" at ${in.format}""",
                            s"""Parse succeeded with disallowed value "$value" at ${in.format}"""
                        )
                    case result : NoSuccess =>
                        MatchResult(false, result.toMessage, "NOT USED")
                }
        }

    /**
     * Matcher for unsucessful parse at given location and with given message.
     * The predicate `ok` is used to detect the unsucecsful results that are
     * acceptable.
     */
    def noSuccessParseAt[T](line : Int, column : Int, expectedMsg : String, ok : ParseResult[T] => Boolean) =
        new Matcher[ParseResult[T]] {
            def apply(result : ParseResult[T]) = {
                result match {
                    case Success(value, in) =>
                        MatchResult(
                            false,
                            s"""Parse succeeded with "$value" at ${in.format}""",
                            "NOT USED"
                        )
                    case res @ NoSuccess(message, in) =>
                        if (ok(res))
                            MatchResult(
                                (in.position.line == line) && (in.position.column == column) && (message == expectedMsg),
                                {
                                    val buf = new scala.collection.mutable.ListBuffer[String]()
                                    if (in.position.line != line)
                                        buf += s"line is ${in.position.line} not $line"
                                    if (in.position.column != column)
                                        buf += s"column is ${in.position.column} not $column"
                                    if (message != expectedMsg)
                                        buf += s"""message is "$message" not "$expectedMsg""""
                                    buf.mkString(s"Wrong parse ${res.kind}: ", ", ", "")
                                },
                                {
                                    val buf = new scala.collection.mutable.ListBuffer[String]()
                                    if (in.position.line == line)
                                        buf += s"line is $line"
                                    if (in.position.column == column)
                                        buf += s"column is $column"
                                    if (message == expectedMsg)
                                        buf += s"""message is "$expectedMsg""""
                                    buf.mkString(s"Expected parse ${res.kind}: ", ", ", "")
                                }
                            )
                        else
                            MatchResult(false, s"Wrong parse: ${res.toMessage}", "")
                }
            }
        }

    /**
     * Matcher for parse error at given location and with given message.
     */
    def errorParseAt[T](line : Int, column : Int, expectedMsg : String) =
        noSuccessParseAt(line, column, expectedMsg, (r : ParseResult[T]) => r.isInstanceOf[Error])

    /**
     * Matcher for parse failure at given location and with given message.
     */
    def failParseAt[T](line : Int, column : Int, expectedMsg : String) =
        noSuccessParseAt(line, column, expectedMsg, (r : ParseResult[T]) => r.isInstanceOf[Failure])

}

/**
 * Useful test routines for transformers.
 */
trait TransformerTests extends ParseTests {

    import org.bitbucket.inkytonik.kiama.parsing.{NoSuccess, Success}

    /**
     * Matcher for parsing and then transformation with expected value. Try to
     * parse `term` as a `T` using the parser `parser`, which is expected to
     * succeed while consuming all of the input. Then pass the resulting `T` to the
     * `transform` transformation function. The result of the transformation is
     * then compared to `expected`.
     */
    def transformTo[T](parser : parsers.Parser[T], transform : T => T, expected : T) =
        new Matcher[String] {
            def apply(term : String) =
                parser(term) match {
                    case Success(ast, _) =>
                        val transformed = transform(ast)
                        MatchResult(
                            transformed == expected,
                            s""""$term" transformed to "$transformed" not expected "$expected"""",
                            s""""$term" evaluated to "$expected""""
                        )
                    case result : NoSuccess =>
                        MatchResult(false, s""""$term" ${result.toMessage}"""", "NOT USED")
                }
        }

}

/**
 * Useful test routines for pretty-printers.
 */

trait PrettyPrinterTests extends Tests {

    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.{Document, Links}

    /**
     * Matcher for pretty-printer links.
     */
    def produceLinks[T](links : Links) =
        new Matcher[Document] {
            def apply(doc : Document) =
                MatchResult(
                    doc.links == links,
                    s"Document has links ${doc.links} instead of expected $links",
                    s"Document has expected $links"
                )
        }

}

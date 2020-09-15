/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package util

import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.{Matcher, MatchResult}
import org.scalatestplus.scalacheck.Checkers

/**
 * General test support designed to be mixed in to compilers or drivers.
 */
trait Tests extends AnyFunSuiteLike with BeforeAndAfter with BeforeAndAfterAll
    with BeforeAndAfterEach with Checkers with Matchers {

    import Comparison.{same, sameCollection, sameElements}
    import org.scalatest.Tag

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
        pkgName.replace(".", "/")
    }

    /**
     * Path to the source folder that contains this suite. By default,
     * `src/test/scala` with `suitePackagePath` appended.
     */
    def suiteSourcePath : String =
        s"src/test/scala/$suitePackagePath"

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
 * Common base for all tests within the core Kiama project.
 */
trait KiamaTests extends Tests {

    /**
     * Path to the source folder that contains this suite. By default,
     * `extras/src/test/scala` with `suitePackagePath` appended.
     */
    override def suiteSourcePath : String =
        s"extras/src/test/scala/$suitePackagePath"

}

/**
 * Infrastructure for writing parser tests.
 */
trait ParseTests extends KiamaTests {

    import org.bitbucket.inkytonik.kiama.parsing.{
        Error,
        Failure,
        NoSuccess,
        ParseResult,
        Success
    }

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
    def parseText[T](positions : Positions, expected : String) =
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

    import org.bitbucket.inkytonik.kiama.parsing.{NoSuccess, ParseResult, Success}
    import org.bitbucket.inkytonik.kiama.util.StringSource

    /**
     * Matcher for parsing and then transformation with expected value. Try to
     * parse `term` as a `T` using the parser `parser`, which is expected to
     * succeed while consuming all of the input. Then pass the resulting `T` to the
     * `transform` transformation function. The result of the transformation is
     * then compared to `expected`.
     */
    def transformTo[T](parser : Source => ParseResult[T], transform : T => T, expected : T) =
        new Matcher[String] {
            def apply(term : String) =
                parser(StringSource(term)) match {
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

trait PrettyPrinterTests extends KiamaTests {

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

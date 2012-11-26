/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2012 Anthony M Sloane, Macquarie University.
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

import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.{NoPosition, Position}

/**
 * Class to keep track of line and column positions. We keep track of
 * both the `start` and `finish` position.
 */
trait Positioned {

    /**
     * `start` is the position just before the input for the positioned
     * value starts, ignoring any whitespace if the relevant parser is
     * skipping it.
     */
    var start : Position = NoPosition

    /**
     * Set the `start` position if it is `NoPosition`, otherwise leave
     * it unchanged. Return the new positioned value.
     */
    def setStart (p : Position) : this.type = {
        if (start eq NoPosition)
            start = p
        this
    }

    /**
     * `finish` is the position just after the last character of the
     * input for the positioned value.
     */
    var finish : Position = NoPosition

    /**
     * Set the `finish` position if it is `NoPosition`, otherwise leave
     * it unchanged. Return the new positioned value.
     */
    def setFinish (p : Position) : this.type = {
        if (finish eq NoPosition)
            finish = p
        this
    }

    /**
     * Set both the `start` and `finish` positions from the given
     * `Positioned` value, if they are, individually, `NoPosition`,
     * otherwise leave them unchanged.
     */
    def setPos (p : Positioned) : this.type =
        setStart (p.start).setFinish (p.finish)

}

/**
 * An extension of `ParserUtilities` that has support for automatically
 * tracking start and finish positions for tree nodes.
 */
trait PositionedParserUtilities extends ParserUtilities {

    /**
     * Run a parse function on some input and if the result is `Positioned`
     * set its positions.
     */
    def parseAndPosition[T] (f : Input => ParseResult[T], in : Input) : ParseResult[T] =
        f (in) match {
            case res @ Success (t, in1) =>
                t match {
                    case p : Positioned =>
                        val startoffset = handleWhiteSpace (in)
                        val newin = in.drop (startoffset - in.offset)
                        val np = p.setStart (newin.pos).setFinish (in1.pos)
                        Success (np.asInstanceOf[T], in1)
                    case _ =>
                        res
                }
            case res =>
                res
        }

    /**
     * Make a new parser that processes input according to `f`. If the
     * result produced by `f` is `Positioned`, set its start and finish
     * positions. If the parser is ignoring whitespace then the start
     * position will be the first non-white character that was accepted.
     */
    override def Parser[T] (f : Input => ParseResult[T]) : Parser[T] =
        new Parser[T] {
            def apply (in : Input) : ParseResult[T] =
                parseAndPosition (f, in)
        }

    /**
     * As for `Parser` but for the non-backtracking version `OnceParser`.
     */
    override def OnceParser[T](f : Input => ParseResult[T]) : Parser[T] with OnceParser[T] =
       new Parser[T] with OnceParser[T] {
            def apply (in : Input) : ParseResult[T] =
                parseAndPosition (f, in)
       }

}

/**
 * Combination of positioned parsing utilities and whitespace handling with a
 * parser.
 */
trait WhitespacePositionedParserUtilities extends WhitespaceParser with PositionedParserUtilities {

    /**
     * Version of `handleWhiteSpace` that accepts an `Input` value and
     * skips over whitespace defined by the parser. Used with
     * `PositionedParserUtilities` so that correct positions are used.
     */
    override def handleWhiteSpace (in : Input) : Int =
        parseWhitespace (in).next.offset

}

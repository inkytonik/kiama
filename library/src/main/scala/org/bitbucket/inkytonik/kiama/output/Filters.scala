/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package output

/**
 * A collection of useful string filters. They are particularly intended
 * to be filters for pretty-printer output so that the output can be
 * tailored for a restricted setting in which it will be put. E.g., a
 * program might be pretty-printed to show in a GUI window of a particular
 * size, or lines indented greater than a certain amount might be omitted
 * to show an overview.
 */
trait Filters {

    /**
     * A filter that limits the string `s` to at most `n` characters.
     */
    def keepMaxChars(n : Int)(s : String) : String =
        s.take(n)

    /**
     * A filter that limits the string `s` to at most `n` completed
     * lines. The final end of line is included.
     */
    def keepMaxLines(n : Int)(s : String) : String =
        s.linesWithSeparators.take(n).mkString

    /**
     * A filter that limits the string `s` to at most `n` words. A word
     * is one or more consecutive non-whitespace characters. The
     * whitespace after the last word (if any) is not included.
     */
    def keepMaxWords(n : Int)(s : String) : String = {
        val wordRE = """^(?:\s*[^\s]+){0,%d}""".format(n).r
        wordRE.findFirstIn(s).getOrElse("")
    }

    /**
     * A replacement function that when given an integer `n` returns the
     * string `"..."` preceded by `n` spaces. The string argument `s` is
     * ignored.
     */
    def indentedEllipsis(n : Int, s : String) : String =
        s"${" " * n}...\n"

    /**
     * Return the indentation of a line, i.e., the number of spaces that
     * appear before the first non-space character.
     */
    def indentOf(s : String) : Int =
        s.takeWhile(_.isSpaceChar).length

    /**
     * A filter that replaces runs of lines that have an indentation
     * level of at least `n` spaces. A run of replaced lines will be
     * replaced by the result of a call `mkrepl (n, l)` where `l` is
     * the first line of the run. By default, `mkrepl` is
     * `indentedEllipsis`.
     */
    def keepMaxIndent(n : Int, s : String,
        mkrepl : (Int, String) => String = indentedEllipsis) : String = {
        s.linesWithSeparators.foldLeft((Vector[String](), true)) {
            case ((result, first), l) =>
                if (indentOf(l) >= n)
                    if (first)
                        (result :+ mkrepl(n, l), false)
                    else
                        (result, false)
                else
                    (result :+ l, true)
        }._1.mkString
    }

}

/**
 * A collection of useful string filters.
 */
object Filters extends Filters

/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013 Anthony M Sloane, Macquarie University.
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

    import scala.collection.immutable.Seq

    /**
     * A filter that limits the string `s` to at most `n` characters.
     */
    def keepMaxChars (n : Int) (s : String) : String =
        s.take (n)

    /**
     * A filter that limits the string `s` to at most `n` completed
     * lines. The final end of line is included.
     */
    def keepMaxLines (n : Int) (s : String) : String =
        s.linesWithSeparators.take (n).mkString

    /**
     * A filter that limits the string `s` to at most `n` words. A word
     * is one or more consecutive non-whitespace characters. The
     * whitespace after the last word (if any) is not included.
     */
    def keepMaxWords (n : Int) (s : String) : String = {
        val wordRE = """^(?:\s*[^\s]+){0,%d}""".format (n).r
        wordRE.findFirstIn (s).getOrElse ("")
    }

    /**
     * A replacement function that when given an integer `n` returns the
     * string `"..."` preceded by `n` spaces. The string argument `s` is
     * ignored.
     */
    def indentedEllipsis (n : Int, s : String) : String =
        s"${" " * n}...\n"

    /**
     * Return the indentation of a line, i.e., the number of spaces that
     * appear before the first non-space character.
     */
    def indentOf (s : String) : Int =
        s.takeWhile (_.isSpaceChar).length

    /**
     * A filter that replaces runs of lines that have an indentation
     * level of at least `n` spaces. A run of replaced lines will be
     * replaced by the result of a call `mkrepl (n, l)` where `l` is
     * the first line of the run. By default, `mkrepl` is
     * `indentedEllipsis`.
     */
    def keepMaxIndent (n : Int, s : String,
                       mkrepl : (Int, String) => String = indentedEllipsis) : String = {
        s.linesWithSeparators.toSeq.foldLeft ((Seq[String] (), true)) {
            case ((result, first), l) =>
                if (indentOf (l) >= n)
                    if (first)
                        (result :+ mkrepl (n, l), false)
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

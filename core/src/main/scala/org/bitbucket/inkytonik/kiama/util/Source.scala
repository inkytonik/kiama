/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2015-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package util

import java.io.Reader

/**
 * A source of input to which a position might refer.
 */
trait Source {

    /**
     * The name of this source or `None` if it doesn't have a name.
     */
    def optName : Option[String]

    /**
     * The content of the source.
     */
    val content : String

    /**
     * A map of line offsets into the source character sequence and a
     * count of how many lines are present. The line offset map is
     * indexed starting at zero and contains at least one entry.
     */
    lazy val (lineStarts, lineCount) =
        (0 until content.length).foldLeft((Vector[Int](0), 1)) {
            case ((v, n), i) =>
                if (content.charAt(i) == '\n')
                    (v :+ (i + 1), n + 1)
                else
                    (v, n)
        }

    /**
     * Return the offset after the last character of a line.
     */
    def lineFinish(line : Int) =
        if (line == lineCount) content.length else lineStarts(line) - 1

    /**
     * If the given line number is within range for this source, return
     * the content of that line, otherwise return `None`. As a special
     * case, support a line beyond the end of the input which contains
     * nothing since parsers
     */
    def optLineContents(line : Int) : Option[String] = {
        if ((line >= 1) && (line <= lineCount))
            Some(content.substring(lineStarts(line - 1), lineFinish(line)))
        else if (line == lineCount + 1)
            Some("")
        else
            None
    }

    /**
     * Convert an offset into the content into a position.
     */
    def offsetToPosition(offset : Int) : Position =
        lineStarts.lastIndexWhere(offset >= _) match {
            case -1 =>
                Position(0, 0, this)
            case line =>
                Position(line + 1, offset - lineStarts(line) + 1, this)
        }

    /**
     * If the position is valid for this source, return the corresponding
     * offset into the content, otherwise return `None`.
     */
    def positionToOffset(position : Position) : Option[Int] = {
        val line = position.line
        if ((line >= 1) && (line <= lineCount)) {
            val lineStart = lineStarts(line - 1)
            val column = position.column
            if ((column >= 1) && (column <= lineFinish(line) - lineStart + 1))
                Some(lineStart + column - 1)
            else
                None
        } else
            None
    }

    /**
     * Return a reader on this source. Not normally used by Kiama but
     * useful if you want to use a source with other code that requires
     * a reader.
     */
    def reader : Reader

}

/**
 * Support code for various sources.
 */

object Source {

    import java.io.File.separatorChar
    import java.lang.System.getProperty

    /**
     * Return a simplified filename where a string has been dropped if it
     * occurs as a prefix of the given filename. The system separator
     * character is also dropped if it occurs immediately after a
     * non-empty prefix.
     */
    def dropPrefix(filename : String, prefix : String) : String = {

        def dropIgnoreSep(i : Int) : String =
            if ((i == 0) || ((i == 1) && (filename(0) == separatorChar)))
                filename
            else if (i < filename.length)
                filename.drop(if (filename(i) == separatorChar) i + 1 else i)
            else
                ""

        for (i <- 0 until prefix.length) {
            if ((i == filename.length) || (filename(i) != prefix(i)))
                return filename
        }
        dropIgnoreSep(prefix.length)

    }

    /**
     * Return a simplified filename where the current path has been dropped
     * if it occurs as a prefix of the given filename.
     */
    def dropCurrentPath(filename : String) : String =
        dropPrefix(filename, getProperty("user.dir"))

}

/**
 * A source that is a string.
 */
case class StringSource(
    content : String,
    optName : Option[String] = None
) extends Source {
    def reader : Reader = IO.stringreader(content)
}

/**
 * A source that is a named file.
 */
case class FileSource(filename : String, encoding : String = "UTF-8") extends Source {
    val optName = Some(Source.dropCurrentPath(filename))
    lazy val content = scala.io.Source.fromFile(filename, encoding).mkString
    def reader : Reader = IO.filereader(filename, encoding)
}

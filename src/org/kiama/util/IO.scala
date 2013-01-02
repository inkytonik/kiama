/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2013 Anthony M Sloane, Macquarie University.
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

/**
 * Utility wrapper for I/O to isolate Kiama code from some Java I/O details.
 * Ultimately this will be replaced by code using the new scala.io.
 */
object IO {

    import java.io.{BufferedReader, BufferedWriter, FileInputStream,
                    FileOutputStream, InputStreamReader, OutputStreamWriter,
                    Reader, StringReader, Writer}

    /**
     * Exception thrown when a requested file cannot be found.
     */
    case class FileNotFoundException (message : String) extends java.lang.Exception (message)

    /**
     * Return a new buffered reader on the file with the given name.
     * The `encoding` argument gives the character encoding of the
     * file (default: UTF-8). Throw `FileNotFoundException` if the
     * file cannot be found.
     */
    def filereader (name : String, encoding : String = "UTF-8") : BufferedReader =
        try {
            new BufferedReader (
                new InputStreamReader (
                    new FileInputStream (name),
                    encoding))
        } catch {
            case e : java.io.FileNotFoundException =>
                throw FileNotFoundException (e.getMessage)
        }

    /**
     * Return a new writer reader on the file with the given name.
     * The `encoding` argument gives the character encoding of the
     * file (default: UTF-8).
     */
    def filewriter (name : String, encoding : String = "UTF-8") : BufferedWriter =
        new BufferedWriter (
            new OutputStreamWriter (
                new FileOutputStream (name),
                encoding))

    /**
     * Return a new buffered reader on the given string.
     */
    def stringreader (string : String) : BufferedReader =
        new BufferedReader (new StringReader (string))

}

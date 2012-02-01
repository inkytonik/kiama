/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2011 Anthony M Sloane, Macquarie University.
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

    import java.io.{BufferedReader, FileInputStream, FileOutputStream,
                    InputStreamReader, OutputStreamWriter, Reader,
                    StringReader, Writer}

    /**
     * Exception thrown when a requested file cannot be found.
     */
    case class FileNotFoundException (message : String) extends java.lang.Exception (message)

    /**
     * Return a new buffered reader on the UTF-8 encoded file with the
     * given name.  Throw FileNotFoundException if that file cannot be
     * found.
     */
    def filereader (name : String) : BufferedReader =
        try {
            new BufferedReader (
                new InputStreamReader (
                    new FileInputStream (name), "UTF-8"))
        } catch {
            case e : java.io.FileNotFoundException =>
                throw FileNotFoundException (e.getMessage)
        }

    /**
     * Return a new writer reader on the UTF-8 encoded file with the
     * given name.
     */
    def filewriter (name : String) : Writer =
        new OutputStreamWriter (new FileOutputStream (name), "UTF-8")

    /**
     * Return a new buffered reader on the given string.
     */
    def stringreader (string : String) : BufferedReader =
        new BufferedReader (new StringReader (string))

}

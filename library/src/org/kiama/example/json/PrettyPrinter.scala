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
package example.json

/**
 * Abstract syntax tree pretty-printing for JSON.
 */
trait PrettyPrinter extends org.kiama.output.PrettyPrinter {

    import JSONTree._
    import org.kiama.output.PrettyPrinterTypes.Document

    override val defaultIndent = 1

    /**
     * Format a JSON node.
     */
    def format (t : JValue) : Document =
        pretty (toDoc (t))

    /**
     * Convert a JSON value node to a pretty-printing document.
     */
    def toDoc (t : JValue) : Doc =
        t match {
            case JNull ()     => "null"
            case JTrue ()     => "true"
            case JFalse ()    => "false"
            case JNumber (d)  => value (if (d.isWhole) d.toInt else d)
            case JString (s)  => dquotes (value (s))
            case JArray (vs)  =>
                brackets (group (nest (line <> ssep (vs map toDoc, comma <> line)) <> line))
            case JObject (ps) =>
                braces (group (nest (line <> ssep (ps map pairToDoc, comma <> line)) <> line))
        }

    /**
     * Return a pretty-printer document for an object pair.
     */
    def pairToDoc (p : (JName,JValue)) : Doc =
        dquotes (p._1.s) <+> colon <+> toDoc (p._2)

}

/**
 * Abstract syntax tree pretty-printing for JSON.
 */
object PrettyPrinter extends PrettyPrinter


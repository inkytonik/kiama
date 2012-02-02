/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011 Anthony M Sloane, Macquarie University.
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
 * AST pretty-printing.
 */
object PrettyPrinter extends org.kiama.util.PrettyPrinter {

    import JSONTree._

    override val defaultIndent = 1

    /**
     * Return a pretty-printed version of a node.
     */
    def pretty (t : JValue) : String =
        super.pretty (show (t))

    /**
     * Convert a JSON value node to a pretty-printing document.
     */
    private def show (t : JValue) : Doc =
        t match {
            case JNull ()     => "null"
            case JTrue ()     => "true"
            case JFalse ()    => "false"
            case JNumber (d)  => value (d)
            case JString (s)  => dquotes (value (s))
            case JArray (vs)  => 
                brackets (group (nest (line <> ssep (vs map show, comma <> line)) <> line))
            case JObject (ps) =>
                braces (group (nest (line <> ssep (ps map showPair, comma <> line)) <> line))
        }

    /**
     * Return a pretty-printer document for an object pair.
     */
    private def showPair (p : (JName,JValue)) : Doc =
        dquotes (p._1.s) <+> colon <+> show (p._2)

}

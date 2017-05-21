/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.json

/**
 * Abstract syntax tree pretty-printing for JSON.
 */
trait PrettyPrinter extends org.bitbucket.inkytonik.kiama.output.PrettyPrinter {

    import JSONTree._
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

    override val defaultIndent = 1

    /**
     * Format a JSON node.
     */
    def format(t : JValue) : Document =
        pretty(toDoc(t))

    /**
     * Convert a JSON value node to a pretty-printing document.
     */
    def toDoc(t : JValue) : Doc =
        t match {
            case JNull()    => "null"
            case JTrue()    => "true"
            case JFalse()   => "false"
            case JNumber(d) => value(if (d.isWhole) d.toInt else d)
            case JString(s) => dquotes(value(s))
            case JArray(vs) =>
                brackets(group(nest(line <> ssep(vs map toDoc, comma <> line)) <> line))
            case JObject(ps) =>
                braces(group(nest(line <> ssep(ps map pairToDoc, comma <> line)) <> line))
        }

    /**
     * Return a pretty-printer document for an object pair.
     */
    def pairToDoc(p : (JName, JValue)) : Doc =
        dquotes(p._1.s) <+> colon <+> toDoc(p._2)

}

/**
 * Abstract syntax tree pretty-printing for JSON.
 */
object PrettyPrinter extends PrettyPrinter


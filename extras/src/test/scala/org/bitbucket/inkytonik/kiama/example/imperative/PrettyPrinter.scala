/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.imperative

/**
 * Abstract syntax tree pretty-printing for the imperative language.
 */
class PrettyPrinter extends org.bitbucket.inkytonik.kiama.output.PrettyPrinter {

    import ImperativeTree._
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

    /**
     * Format an imperative node.
     */
    def format(t : ImperativeNode) : Document =
        pretty(toDoc(t))

    /**
     * Convert an imperative node to a pretty-printing document in
     * fully-parenthesised C style.
     */
    def toDoc(t : ImperativeNode) : Doc =
        t match {
            case Num(d)      => value(d)
            case Var(s)      => s
            case Neg(e)      => parens("-" <> toDoc(e))
            case Add(l, r)   => binToDoc(l, "+", r)
            case Sub(l, r)   => binToDoc(l, "-", r)
            case Mul(l, r)   => binToDoc(l, "*", r)
            case Div(l, r)   => binToDoc(l, "/", r)
            case Null()      => semi
            case Seqn(ss)    => group(braces(nest(line <> ssep(ss map toDoc, line)) <> line))
            case Asgn(v, e)  => toDoc(v) <+> "=" <+> toDoc(e) <> semi
            case While(e, b) => "while" <+> parens(toDoc(e)) <> group(nest(line <> toDoc(b)))
        }

    /**
     * Return a pretty-printing document for an instance of a binary expression.
     */
    def binToDoc(l : ImperativeNode, op : String, r : ImperativeNode) : Doc =
        parens(toDoc(l) <+> op <+> toDoc(r))

}

/**
 * Abstract syntax tree pretty-printing for the imperative language.
 */
object PrettyPrinter extends PrettyPrinter

/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2015 Anthony M Sloane, Macquarie University.
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
package example.imperative

/**
 * Abstract syntax tree pretty-printing for the imperative language.
 */
class PrettyPrinter extends org.kiama.output.PrettyPrinter {

    import ImperativeTree._
    import org.kiama.output.PrettyPrinterTypes.Document

    /**
     * Format an imperative node.
     */
    def format (t : ImperativeNode) : Document =
        pretty (toDoc (t))

    /**
     * Convert an imperative node to a pretty-printing document in
     * fully-parenthesised C style.
     */
    def toDoc (t : ImperativeNode) : Doc =
        t match {
            case Num (d)      => value (d)
            case Var (s)      => s
            case Neg (e)      => parens ("-" <> toDoc (e))
            case Add (l, r)   => binToDoc (l, "+", r)
            case Sub (l, r)   => binToDoc (l, "-", r)
            case Mul (l, r)   => binToDoc (l, "*", r)
            case Div (l, r)   => binToDoc (l, "/", r)
            case Null ()      => semi
            case Seqn (ss)    => group (braces (nest (line <> ssep (ss map toDoc, line)) <> line))
            case Asgn (v, e)  => toDoc (v) <+> "=" <+> toDoc (e) <> semi
            case While (e, b) => "while" <+> parens (toDoc (e)) <> group (nest (line <> toDoc (b)))
        }

    /**
     * Return a pretty-printing document for an instance of a binary expression.
     */
    def binToDoc (l : ImperativeNode, op : String, r : ImperativeNode) : Doc =
        parens (toDoc (l) <+> op <+> toDoc (r))

}

/**
 * Abstract syntax tree pretty-printing for the imperative language.
 */
object PrettyPrinter extends PrettyPrinter

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
package example.imperative

/**
 * AST pretty-printing.
 */
object PrettyPrinter extends org.kiama.util.PrettyPrinter {

    import AST._

    /**
     * Return a pretty-printed version of a node.
     */
    def pretty (t : ImperativeNode) : String =
        super.pretty (show (t))

    /**
     * Convert an imperative node to a pretty-printing document in
     * fully-parenthesised C style.
     */
    def show (t : ImperativeNode) : Doc =
        t match {
            case Num (d)      => value (d)
            case Var (s)      => s
            case Neg (e)      => parens ("-" <> show (e))
            case Add (l, r)   => showbin (l, "+", r)
            case Sub (l, r)   => showbin (l, "-", r)
            case Mul (l, r)   => showbin (l, "*", r)
            case Div (l, r)   => showbin (l, "/", r)
            case Null ()      => semi
            case Seqn (ss)    => braces (nest (line <> ssep (ss map show, line)) <> line)
            case Asgn (v, e)  => show (v) <+> "=" <+> show (e) <> semi
            case While (e, b) => "while" <+> parens (show (e)) <> nest (line <> show (b))
        }

    /**
     * Return a pretty-printing document for an instance of a binary expression.
     */
    def showbin (l : ImperativeNode, op : String, r : ImperativeNode) : Doc =
        parens (show (l) <+> op <+> show (r))

}

/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2014 Anthony M Sloane, Macquarie University.
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
package example.grammar

/**
 * Abstract syntax tree pretty-printing for grammars.
 */
trait PrettyPrinter extends org.kiama.output.PrettyPrinter {

    import GrammarTree._
    import scala.collection.immutable.Seq

    /**
     * Return a pretty-printed version of a node.
     */
    def pretty (t : GrammarNode) : String =
        super.pretty (show (t))

    /**
     * Convert a grammar node to a pretty-printing document.
     */
    def show (t : GrammarNode) : Doc =
        t match {
            case Grammar (r, rs) =>
                "Start rule:" <@> show (r) <@>
                "Other rules:" <@> cat (rs map show)
            case Rule (lhs, rhs) =>
                show (lhs) <+> "->" <+> show (rhs)
            case EmptyProdList () =>
                empty
            case NonEmptyProdList (h, EmptyProdList ()) =>
                show (h)
            case NonEmptyProdList (h, t) =>
                show (h) <+> "|" <+> show (t)
            case Prod (ss) =>
                show (ss)
            case EmptySymbolList () =>
                "epsilon"
            case NonEmptySymbolList (h, EmptySymbolList ()) =>
                show (h)
            case NonEmptySymbolList (h, t) =>
                show (h) <+> show (t)
            case nt : NonTerm =>
                nt.name
            case TermSym (s) =>
                s
            case NonTermSym (s) =>
                s.name
        }

}

/**
 * Abstract syntax tree pretty-printing for grammars.
 */
object PrettyPrinter extends PrettyPrinter


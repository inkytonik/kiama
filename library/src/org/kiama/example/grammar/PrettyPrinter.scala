/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2015 Anthony M Sloane, Macquarie University.
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
    import org.kiama.output.PrettyPrinterTypes.Document
    import scala.collection.immutable.Seq

    /**
     * Format a grammar node.
     */
    def format (t : GrammarNode) : Document =
        pretty (toDoc (t))

    /**
     * Convert a grammar node to a pretty-printing document.
     */
    def toDoc (t : GrammarNode) : Doc =
        t match {
            case Grammar (r, rs) =>
                "Start rule:" <@> toDoc (r) <@>
                "Other rules:" <@> cat (rs map toDoc)
            case Rule (lhs, rhs) =>
                toDoc (lhs) <+> "->" <+> toDoc (rhs)
            case EmptyProdList () =>
                empty
            case NonEmptyProdList (h, EmptyProdList ()) =>
                toDoc (h)
            case NonEmptyProdList (h, t) =>
                toDoc (h) <+> "|" <+> toDoc (t)
            case Prod (ss) =>
                toDoc (ss)
            case EmptySymbolList () =>
                "epsilon"
            case NonEmptySymbolList (h, EmptySymbolList ()) =>
                toDoc (h)
            case NonEmptySymbolList (h, t) =>
                toDoc (h) <+> toDoc (t)
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


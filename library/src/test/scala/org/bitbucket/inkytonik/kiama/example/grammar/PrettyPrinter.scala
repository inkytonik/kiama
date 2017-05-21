/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.grammar

/**
 * Abstract syntax tree pretty-printing for grammars.
 */
trait PrettyPrinter extends org.bitbucket.inkytonik.kiama.output.PrettyPrinter {

    import GrammarTree._
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

    /**
     * Format a grammar node.
     */
    def format(t : GrammarNode) : Document =
        pretty(toDoc(t))

    /**
     * Convert a grammar node to a pretty-printing document.
     */
    def toDoc(t : GrammarNode) : Doc =
        t match {
            case Grammar(r, rs) =>
                "Start rule:" <@> toDoc(r) <@>
                    "Other rules:" <@> cat(rs map toDoc)
            case Rule(lhs, rhs) =>
                toDoc(lhs) <+> "->" <+> toDoc(rhs)
            case EmptyProdList() =>
                emptyDoc
            case NonEmptyProdList(h, EmptyProdList()) =>
                toDoc(h)
            case NonEmptyProdList(h, t) =>
                toDoc(h) <+> "|" <+> toDoc(t)
            case Prod(ss) =>
                toDoc(ss)
            case EmptySymbolList() =>
                "epsilon"
            case NonEmptySymbolList(h, EmptySymbolList()) =>
                toDoc(h)
            case NonEmptySymbolList(h, t) =>
                toDoc(h) <+> toDoc(t)
            case nt : NonTerm =>
                nt.name
            case TermSym(s) =>
                s
            case NonTermSym(s) =>
                s.name
        }

}

/**
 * Abstract syntax tree pretty-printing for grammars.
 */
object PrettyPrinter extends PrettyPrinter

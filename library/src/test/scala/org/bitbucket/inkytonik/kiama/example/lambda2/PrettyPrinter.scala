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
package example.lambda2

/**
 * Lambda calculus pretty printing.
 */
class PrettyPrinter extends org.bitbucket.inkytonik.kiama.output.PrettyPrinter {

    import LambdaTree._
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

    /**
     * Format a lambda expression.
     */
    def format(t : Exp) : Document =
        pretty(toDoc(t))

    /**
     * The layout from formatting a lambda expression.
     */
    def formattedLayout(t : Exp) : String =
        format(t).layout

    /**
     * Format a type.
     */
    def format(t : Type) : Document =
        pretty(typeToDoc(t))

    /**
     * The layout from formatting a type.
     */
    def formattedLayout(t : Type) : String =
        format(t).layout

    /**
     * Convert an expression node to a pretty-printing document in
     * fully-parenthesised style.
     */
    def toDoc(t : Exp) : Doc =
        t match {
            case Num(d) => value(d)
            case Var(i) => i
            case Lam(i, t, e) => parens('\\' <> i <>
                typedeclToDoc(t) <+> '.' <+>
                group(nest(toDoc(e))))
            case App(e1, e2)        => parens(toDoc(e1) <+> toDoc(e2))

            case Opn(l, AddOp(), r) => binToDoc(l, "+", r)
            case Opn(l, SubOp(), r) => binToDoc(l, "-", r)

            case Let(i, t, e1, e2) =>
                parens("let" <+> i <> typedeclToDoc(t) <+> '=' <>
                    nest(line <> toDoc(e1)) <+> "in" <>
                    nest(line <> toDoc(e2)))
            case Letp(bs, e) =>
                parens("letp" <>
                    nest(line <> vsep(bs.map(b => b.i <+> '=' <+> toDoc(b.e)))) <+>
                    "in" <>
                    nest(line <> toDoc(e)))
        }

    /**
     * Return a pretty-printing document for an instance of a type declaration.
     */
    def typedeclToDoc(t : Type) : Doc =
        if (t == NoType())
            emptyDoc
        else
            space <> ':' <+> typeToDoc(t)

    /**
     * Return a pretty-printing document for an instance of a type.
     */
    def typeToDoc(t : Type) : Doc =
        t match {
            case IntType()       => "Int"
            case FunType(t1, t2) => typeToDoc(t1) <+> "->" <+> typeToDoc(t2)
            case NoType()        => "No" // Not used
            case UnknownType()   => "Unknown" // Not used
        }

    /**
     * Return a pretty-printing document for an instance of a binary expression.
     */
    def binToDoc(l : Exp, op : String, r : Exp) : Doc =
        parens(toDoc(l) <+> op <+> toDoc(r))

}

/**
 * Lambda calculus pretty printing.
 */
object PrettyPrinter extends PrettyPrinter

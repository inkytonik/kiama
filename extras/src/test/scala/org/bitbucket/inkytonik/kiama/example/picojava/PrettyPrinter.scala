/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.picojava

/**
 * Abstract syntax tree pretty-printing for PicoJava.
 */
class PrettyPrinter extends org.bitbucket.inkytonik.kiama.output.PrettyPrinter {

    import PicoJavaTree._
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

    /**
     * Format a PicoJava node.
     */
    def format(t : PicoJavaNode) : Document =
        pretty(toDoc(t))

    /**
     * Convert a PicoJava AST node to a pretty-printing document.
     */
    def toDoc(t : PicoJavaNode) : Doc =
        t match {
            case Program(b) =>
                toDoc(b)
            case Block(bs) =>
                braces(nest(line <> ssep(bs map toDoc, line)) <> line)
            case ClassDecl(n, sc, b) =>
                val scshow = sc.map {
                    case idn =>
                        " extends" <+> toDoc(idn)
                }.getOrElse(emptyDoc)
                "class" <+> n <> scshow <+> toDoc(b)
            case VarDecl(t, n) =>
                toDoc(t) <+> n <> semi
            case AssignStmt(v, e) =>
                toDoc(v) <+> equal <+> toDoc(e) <> semi
            case WhileStmt(c, b) =>
                "while" <+> parens(toDoc(c)) <> toDoc(b)
            case Use(n) =>
                n
            case Dot(or, i) =>
                toDoc(or) <> "." <> toDoc(i)
            case BooleanLiteral(v) =>
                v
            case PrimitiveDecl(n) =>
                n
            case UnknownDecl(n) =>
                n
        }

}

/**
 * Abstract syntax tree pretty-printing for PicoJava.
 */
object PrettyPrinter extends PrettyPrinter

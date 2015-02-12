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
package example.picojava

/**
 * Abstract syntax tree pretty-printing for PicoJava.
 */
class PrettyPrinter extends org.kiama.output.PrettyPrinter {

    import PicoJavaTree._
    import org.kiama.output.PrettyPrinterTypes.Document

    /**
     * Format a PicoJava node.
     */
    def format (t : PicoJavaNode) : Document =
        pretty (toDoc (t))

    /**
     * Convert a PicoJava AST node to a pretty-printing document.
     */
    def toDoc (t : PicoJavaNode) : Doc =
        t match {
            case Program (b) =>
                toDoc (b)
            case Block (bs) =>
                braces (nest (line <> ssep (bs map toDoc, line)) <> line)
            case ClassDecl (n, sc, b) =>
                val scshow = sc.map {
                                 case idn =>
                                     " extends" <+> toDoc (idn)
                             }.getOrElse (empty)
                "class" <+> n <> scshow <+> toDoc (b)
            case VarDecl (t, n) =>
                toDoc (t) <+> n <> semi
            case AssignStmt (v, e) =>
                toDoc (v) <+> equal <+> toDoc (e) <> semi
            case WhileStmt (c, b) =>
                "while" <+> parens (toDoc (c)) <> toDoc (b)
            case Use (n) =>
                n
            case Dot (or, i) =>
                toDoc (or) <> "." <> toDoc (i)
            case BooleanLiteral (v) =>
                v
            case PrimitiveDecl (n) =>
                n
            case UnknownDecl (n) =>
                n
        }

}

/**
 * Abstract syntax tree pretty-printing for PicoJava.
 */
object PrettyPrinter extends PrettyPrinter

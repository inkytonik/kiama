/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014 Anthony M Sloane, Macquarie University.
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
object PrettyPrinter extends org.kiama.output.PrettyPrinter {

    import PicoJavaTree._

    /**
     * Return a pretty-printed version of a node.
     */
    def pretty (t : PicoJavaTree) : String =
        super.pretty (show (t))

    /**
     * Convert a PicoJava AST node to a pretty-printing document.
     */
    def show (t : PicoJavaTree) : Doc =
        t match {
            case Program (b) =>
                show (b)
            case Block (bs) =>
                braces (nest (line <> ssep (bs map show, line)) <> line)
            case ClassDecl (n, sc, b) =>
                val scshow = sc.map { case idn => " extends" <+> show (idn)}.getOrElse (empty)
                "class" <+> n <> scshow <+> show (b)
            case VarDecl (t, n) =>
                show (t) <+> n <> semi
            case AssignStmt (v, e) =>
                show (v) <+> equal <+> show (e) <> semi
            case WhileStmt (c, b) =>
                "while" <+> parens (show (c)) <> show (b)
            case Use (n) =>
                n
            case Dot (or, i) =>
                show (or) <> "." <> show (i)
            case BooleanLiteral (v) =>
                v
            case PrimitiveDecl (n) =>
                n
            case UnknownDecl (n) =>
                n
        }

}

/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2014 Anthony M Sloane, Macquarie University.
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
package example.lambda2

object PrettyPrinter extends org.kiama.output.PrettyPrinter {

    import LambdaTree._

    /**
     * Format a lambda expression.
     */
    def format (t : Exp) : String =
        pretty (toDoc (t))

    /**
     * Format a type.
     */
    def format (t : Type) : String =
        pretty (typeToDoc (t))

    /**
     * Convert an expression node to a pretty-printing document in
     * fully-parenthesised style.
     */
    def toDoc (t : Exp) : Doc =
        t match {
            case Num (d)       => value (d)
            case Var (i)       => i
            case Lam (i, t, e) => parens ('\\' <> i <>
                                          typedeclToDoc (t) <+> '.' <+>
                                          group (nest (toDoc (e))))
            case App (e1, e2)  => parens (toDoc (e1) <+> toDoc (e2))

            case Opn (l, AddOp (), r) => binToDoc (l, "+", r)
            case Opn (l, SubOp (), r) => binToDoc (l, "-", r)

            case Let (i, t, e1, e2) =>
                parens ("let" <+> i <> typedeclToDoc (t) <+> '=' <>
                        nest (line <> toDoc (e1)) <+> "in" <>
                        nest (line <> toDoc (e2)))
            case Letp (bs, e) =>
                parens ("letp" <>
                        nest (line <> vsep (bs.map (b => b.i <+> '=' <+> toDoc (b.e)))) <+>
                        "in" <>
                        nest (line <> toDoc (e)))
        }

    /**
     * Return a pretty-printing document for an instance of a type declaration.
     */
    def typedeclToDoc (t : Type) : Doc =
        if (t == NoType ())
            empty
        else
            space <> ':' <+> typeToDoc (t)

    /**
     * Return a pretty-printing document for an instance of a type.
     */
    def typeToDoc (t : Type) : Doc =
        t match {
            case IntType ()       => "Int"
            case FunType (t1, t2) => typeToDoc (t1) <+> "->" <+> typeToDoc (t2)
            case NoType ()        => "No" // Not used
            case UnknownType ()   => "Unknown" // Not used
        }

    /**
     * Return a pretty-printing document for an instance of a binary expression.
     */
    def binToDoc (l : Exp, op : String, r : Exp) : Doc =
        parens (toDoc (l) <+> op <+> toDoc (r))

}

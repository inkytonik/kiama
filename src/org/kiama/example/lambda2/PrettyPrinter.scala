/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011 Anthony M Sloane, Macquarie University.
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

object PrettyPrinter extends org.kiama.util.PrettyPrinter
 {
    
    import AST._

    /**
     * Return a pretty-printed version of an expression.
     */
    def pretty (t : Exp) : String =
        super.pretty (show (t))

    /**
     * Return a pretty-printed version of a type.
     */
    def pretty (t : Type) : String =
        super.pretty (showtype (t))

    /**
     * Convert an expression node to a pretty-printing document in
     * fully-parenthesised style.
     */
    private def show (t : Exp) : Doc =
        t match {
            case Num (d)       => value (d)
            case Var (i)       => text (i)
            case Lam (i, t, e) => parens (char ('\\') <> text (i) <>
                                          showtypedecl (t) <+> char ('.') <+>
                                          group (nest (show (e))))
            case App (e1, e2)  => parens (show (e1) <+> show (e2))
            
            case Opn (AddOp, l, r) => showbin (l, "+", r)
            case Opn (SubOp, l, r) => showbin (l, "-", r)

            case Let (i, t, e1, e2) =>
                parens (text ("let") <+> text (i) <> showtypedecl (t) <+> char ('=') <>
                        nest (line <> show (e1)) <+> text ("in") <>
                        nest (line <> show (e2)))
            case Letp (bs, e) =>
                parens (text ("letp") <+>                         
                        nest (line <> vsep (bs.map (b => text (b.i) <+> char ('=') <+> show (b.e)))) <+> 
                        text ("in") <>
                        nest (line <> show (e)))
        }

    /**
     * Return a pretty-printing document for an instance of a type declaration.
     */
    private def showtypedecl (t : Type) : Doc =
        if (t == null)
            empty
        else
            space <> char (':') <+> showtype (t)
            
    /**
     * Return a pretty-printing document for an instance of a type.
     */
    private def showtype (t : Type) : Doc =
        t match {
            case IntType          => text ("Int")
            case FunType (t1, t2) => showtype (t1) <+> text ("->") <+> showtype (t2)
        }
        
    /**
     * Return a pretty-printing document for an instance of a binary expression.
     */
    private def showbin (l : Exp, op : String, r : Exp) : Doc =
        parens (show (l) <+> text (op) <+> show (r))
    
}

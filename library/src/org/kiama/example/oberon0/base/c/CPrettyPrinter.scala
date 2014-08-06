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
package example.oberon0
package base.c

import org.kiama.output.ParenPrettyPrinter

/**
 * Interface for all C pretty-printers.
 */
trait CPrettyPrinter extends ParenPrettyPrinter {

    import org.kiama.output.PrettyExpression

    def toDoc (n : CNode) : Doc =
        n match {
            case CProgram (is, ds) =>
                vsep (is map toDoc) <@>
                vsep (ds map toDoc, semi)

            case CInclude (s) =>
                s"#include $s"

            case CFunctionDecl (d, args, b) =>
                toDoc (d) <+>
                parens (hsep (args map toDoc, comma)) <+>
                toDoc (b)

            case CBlock (ds, ss) =>
                braces (nest (lterm (ds map toDoc, semi) <>
                              lsep (ss map toDoc, empty)) <>
                        line)

            case CVarDecl (i, t : CArrayType) =>
                basetypeToDoc (t) <> i <> arraydimensToDoc (t)

            case CVarDecl (i, t) =>
                basetypeToDoc (t) <> i

            case CEmptyStmt () =>
                semi

            case CReturn (e) =>
                "return" <+> toDoc (e) <> semi

            case e : CExpression =>
                toParenDoc (e)

            case _ =>
                empty
        }

    override def toParenDoc (e : PrettyExpression) : Doc =
        e match {
            case CIntExp (v) => value (v)
            case _           => super.toParenDoc (e)
        }

    def basetypeToDoc (t : CType) : Doc =
        t match {
            case CIntType ()        => "int" <> space
            case CStrType ()        => "char *"
            case CArrayType (_, et) => basetypeToDoc (et)
        }

    def arraydimensToDoc (t1 : CArrayType) : Doc = {
        s"[${t1.size}]" <>
        (t1.elemtype match {
            case t2 : CArrayType => arraydimensToDoc (t2)
            case _               => empty
         })
    }

}

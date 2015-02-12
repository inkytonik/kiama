/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2015 Anthony M Sloane, Macquarie University.
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
package L4.c

trait CPrettyPrinter extends L3.c.CPrettyPrinter {

    import base.c.{CExpression, CType}
    import L3.c.CDerefExp
    import org.kiama.output.PrettyExpression

    override def basetypeToDoc (t : CType) : Doc =
        t match {
            case CRecordType (fls) =>
                "struct" <+> "{" <> (nest (lterm (fls map toDoc, semi))) <>
                    line <> "}" <> space
            case _ =>
                super.basetypeToDoc (t)
        }

    override def toParenDoc (e : PrettyExpression) : Doc =
        e match {
            case CIndexExp (a, e) =>
                toDoc (a) <> brackets (toDoc (e))
            case CFieldExp (r : CDerefExp, f) =>
                parens (toDoc (r)) <> dot <> f
            case CFieldExp (r, f) =>
                toDoc (r) <> dot <> f
            case _ =>
                super.toParenDoc (e)
        }

}

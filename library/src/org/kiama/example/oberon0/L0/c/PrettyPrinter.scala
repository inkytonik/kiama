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
package L0.c

trait PrettyPrinter extends base.c.PrettyPrinter {

    this : org.kiama.output.PrettyPrinter =>

    import base.c.{CTree, CExpression, CType}
    import org.kiama.output.PrettyExpression

    override def toDoc (n : CTree) : Doc =
        n match {
            case CNamedType (s) =>
                s

            case CTypeDef (d) =>
                "typedef" <+> toDoc (d)

            case CInitDecl (d, e) =>
                toDoc (d) <+> "=" <+> toDoc (e)

            case CAssignment (d, e) =>
                toDoc (d) <+> "=" <+> toDoc (e) <> semi

            case e : CExpression =>
                toParenDoc (e)

            case _ =>
                super.toDoc (n)
        }

    override def basetypeToDoc (t : CType) : Doc =
        t match {
            case CNamedType (i) => i <> space
            case _              => super.basetypeToDoc (t)
        }

    /**
     * CNegExp (CNegExp) special case is to avoid output of --e which is interpreted
     * as a pre-decrement operator.
     */
    override def toParenDoc (e : PrettyExpression) : Doc =
        e match {
            case CIdnExp (i)           => i
            case CNegExp (e : CNegExp) => "-" <> parens (toParenDoc (e))
            case _                     => super.toParenDoc (e)
        }

}

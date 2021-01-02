/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package L0.c

trait CPrettyPrinter extends base.c.CPrettyPrinter {

    import base.c.{CExpression, CNode, CType}
    import org.bitbucket.inkytonik.kiama.output.PrettyExpression

    override def toDoc(n : CNode) : Doc =
        n match {
            case CNamedType(s) =>
                s

            case CTypeDef(d) =>
                "typedef" <+> toDoc(d)

            case CInitDecl(d, e) =>
                toDoc(d) <+> "=" <+> toDoc(e)

            case CAssignment(d, e) =>
                toDoc(d) <+> "=" <+> toDoc(e) <> semi

            case e : CExpression =>
                toParenDoc(e)

            case _ =>
                super.toDoc(n)
        }

    override def basetypeToDoc(t : CType) : Doc =
        t match {
            case CNamedType(i) => i <> space
            case _             => super.basetypeToDoc(t)
        }

    /**
     * CNegExp (CNegExp) special case is to avoid output of --e which is interpreted
     * as a pre-decrement operator.
     */
    override def toParenDoc(e : PrettyExpression) : Doc =
        e match {
            case CIdnExp(i)           => i
            case CNegExp(e : CNegExp) => "-" <> parens(toParenDoc(e))
            case _                    => super.toParenDoc(e)
        }

}

/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package L3.c

trait CPrettyPrinter extends L1.c.CPrettyPrinter {

    import base.c.{CNode, CType}
    import org.bitbucket.inkytonik.kiama.output.PrettyExpression

    override def toDoc(n : CNode) : Doc =
        n match {
            case CCall(s, ps) =>
                s <+> parens(hsep(ps map toParenDoc, comma)) <> semi

            case _ =>
                super.toDoc(n)
        }

    override def basetypeToDoc(t : CType) : Doc =
        t match {
            case CVoidType()   => "void" <> space
            case CAddrType(bt) => super.basetypeToDoc(bt) <> "*"
            case _             => super.basetypeToDoc(t)
        }

    override def toParenDoc(e : PrettyExpression) : Doc =
        e match {
            case CStrExp(s) => dquotes(s)
            case _          => super.toParenDoc(e)
        }

}

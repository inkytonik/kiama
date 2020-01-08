/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package L4.c

trait CPrettyPrinter extends L3.c.CPrettyPrinter {

    import base.c.CType
    import L3.c.CDerefExp
    import org.bitbucket.inkytonik.kiama.output.PrettyExpression

    override def basetypeToDoc(t : CType) : Doc =
        t match {
            case CRecordType(fls) =>
                "struct" <+> "{" <> (nest(lterm(fls map toDoc, semi))) <>
                    line <> "}" <> space
            case _ =>
                super.basetypeToDoc(t)
        }

    override def toParenDoc(e : PrettyExpression) : Doc =
        e match {
            case CIndexExp(a, e) =>
                toDoc(a) <> brackets(toDoc(e))
            case CFieldExp(r : CDerefExp, f) =>
                parens(toDoc(r)) <> dot <> f
            case CFieldExp(r, f) =>
                toDoc(r) <> dot <> f
            case _ =>
                super.toParenDoc(e)
        }

}

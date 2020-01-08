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
package L1.c

trait CPrettyPrinter extends L0.c.CPrettyPrinter {

    import base.c.CNode

    override def toDoc(n : CNode) : Doc =
        n match {
            case CIfStatement(c, ts) =>
                "if" <+> parens(toParenDoc(c)) <+> toDoc(ts)

            case CIfElseStatement(c, ts, es) =>
                "if" <+> parens(toParenDoc(c)) <+> toDoc(ts) <+> "else" <+> toDoc(es)

            case CWhileStatement(c, ss) =>
                "while" <+> parens(toParenDoc(c)) <+> toDoc(ss)

            case _ =>
                super.toDoc(n)
        }

}

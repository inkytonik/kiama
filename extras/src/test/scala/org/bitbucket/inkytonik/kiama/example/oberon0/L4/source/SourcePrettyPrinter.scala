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
package L4.source

trait SourcePrettyPrinter extends L3.source.SourcePrettyPrinter {

    import base.source.SourceNode
    import org.bitbucket.inkytonik.kiama.output.PrettyExpression

    override def toDoc(n : SourceNode) : Doc =
        n match {
            case ArrayTypeDef(s, t) =>
                "ARRAY" <+> toDoc(s) <+> "OF" <+> toDoc(t)

            case RecordTypeDef(Vector()) =>
                "RECORD" <+> "END"

            case RecordTypeDef(fs) =>
                "RECORD" <+> hsep(fs map toDoc, semi) <+> "END"

            case Fields(ids, t) =>
                (hsep(ids map text, comma)) <+> colon <+> toDoc(t)

            case _ =>
                super.toDoc(n)
        }

    override def toParenDoc(e : PrettyExpression) : Doc =
        e match {
            case IndexExp(b, e) =>
                toDoc(b) <> brackets(toDoc(e))
            case FieldExp(b, FieldIdn(f)) =>
                toDoc(b) <> "." <> f
            case _ =>
                super.toParenDoc(e)
        }

}

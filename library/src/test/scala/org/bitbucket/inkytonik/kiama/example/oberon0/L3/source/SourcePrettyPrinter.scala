/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package L3.source

trait SourcePrettyPrinter extends L2.source.SourcePrettyPrinter {

    import base.source.{IdnDef, IdnUse, SourceNode}

    override def toDoc(n : SourceNode) : Doc =
        n match {
            case ProcDecl(IdnDef(i1), as, b, IdnUse(i2)) =>
                "PROCEDURE" <+> i1 <> paramsToDoc(as map toDoc, semi) <> semi <@>
                    blockToDoc(b, true) <+> i2 <> semi

            case FPSection(m, ids, t) =>
                val mode : Doc = if (m == VarMode()) "VAR " else emptyDoc
                mode <> idlistToDoc(ids) <+> colon <+> toDoc(t)

            case Call(IdnUse(i), ps) =>
                i <> paramsToDoc(ps map toParenDoc, comma)

            case _ =>
                super.toDoc(n)
        }

    def paramsToDoc(ds : Vector[Doc], sep : Doc) : Doc =
        ds match {
            case Vector() => emptyDoc
            case _        => space <> parens(hsep(ds, sep))
        }

}

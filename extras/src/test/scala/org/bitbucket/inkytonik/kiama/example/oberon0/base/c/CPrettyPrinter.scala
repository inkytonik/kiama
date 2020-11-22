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
package base.c

import org.bitbucket.inkytonik.kiama.output.ParenPrettyPrinter

/**
 * Interface for all C pretty-printers.
 */
trait CPrettyPrinter extends ParenPrettyPrinter {

    import org.bitbucket.inkytonik.kiama.output.PrettyExpression

    def toDoc(n : CNode) : Doc =
        n match {
            case CProgram(is, ds) =>
                vsep(is map toDoc) <@>
                    vsep(ds map toDoc, semi)

            case CInclude(s) =>
                s"#include $s"

            case CFunctionDecl(d, args, b) =>
                toDoc(d) <+>
                    parens(hsep(args map toDoc, comma)) <+>
                    toDoc(b)

            case CBlock(ds, ss) =>
                braces(nest(lterm(ds map toDoc, semi) <>
                    lsep(ss map toDoc, emptyDoc)) <>
                    line)

            case CVarDecl(i, t : CArrayType) =>
                basetypeToDoc(t) <> i <> arraydimensToDoc(t)

            case CVarDecl(i, t) =>
                basetypeToDoc(t) <> i

            case CEmptyStmt() =>
                semi

            case CReturn(e) =>
                "return" <+> toDoc(e) <> semi

            case e : CExpression =>
                toParenDoc(e)

            case _ =>
                emptyDoc
        }

    override def toParenDoc(e : PrettyExpression) : Doc =
        e match {
            case CIntExp(v) => value(v)
            case _          => super.toParenDoc(e)
        }

    def basetypeToDoc(t : CType) : Doc =
        t match {
            case CIntType()        => "int" <> space
            case CStrType()        => "char *"
            case CArrayType(_, et) => basetypeToDoc(et)
            case _ =>
                sys.error(s"basetypeDoc: unexpected CType $t")
        }

    def arraydimensToDoc(t1 : CArrayType) : Doc = {
        s"[${t1.size}]" <>
            (t1.elemtype match {
                case t2 : CArrayType => arraydimensToDoc(t2)
                case _               => emptyDoc
            })
    }

}

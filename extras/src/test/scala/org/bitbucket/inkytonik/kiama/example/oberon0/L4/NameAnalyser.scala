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
package L4

trait NameAnalyser extends L3.NameAnalyser {

    import base.source.{Expression, SourceNode}
    import org.bitbucket.inkytonik.kiama.util.Messaging.{check, error, Messages}
    import source.{FieldExp, IndexExp, RecordTypeDef}

    /**
     * The error checking for this level.
     */
    override def errorsDef(n : SourceNode) : Messages =
        super.errorsDef(n) ++
            check(n) {
                case n @ RecordTypeDef(fls) =>
                    val flnames = fls.flatMap(_.idndefs)
                    error(n, "record contains duplicate field names", flnames.distinct != flnames)
            }

    override def isLvalue(l : Expression) : Boolean =
        l match {
            case _ : IndexExp | _ : FieldExp =>
                true
            case _ =>
                super.isLvalue(l)
        }

}

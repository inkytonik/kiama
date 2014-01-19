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
package L4

trait NameAnalyser extends L3.NameAnalyser {

    import base.source.{Expression, SourceTree}
    import org.kiama.util.Messaging.{check, message, Messages}
    import source.{FieldExp, IndexExp, RecordTypeDef}

    /**
     * The error checking for this level.
     */
    override def errorsDef (n : SourceTree) : Messages =
        super.errorsDef (n) ++
        check (n) {
            case n @ RecordTypeDef (fls) =>
                val flnames = fls.flatMap (_.idndefs)
                message (n, "record contains duplicate field names", flnames.distinct != flnames)
        }

    override def isLvalue (l : Expression) : Boolean =
        l match {
            case _ : IndexExp | _ : FieldExp =>
                true
            case _ =>
                super.isLvalue (l)
        }

}

/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2013 Anthony M Sloane, Macquarie University.
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

    import base.source.SourceASTNode
    import L0.source.Expression
    import org.kiama.util.Messaging.message
    import source.{FieldExp, IndexExp, RecordTypeDef}

    abstract override def check (n : SourceASTNode) {
        n match {
            case n @ RecordTypeDef (fls) =>
                val flnames = fls.flatMap (_.idndefs)
                if (flnames.distinct != flnames)
                    message (n, "record contains duplicate field names")

            case _ =>
                // Do nothing by default
        }

        super.check (n)
    }

    override def isLvalue (l : Expression) : Boolean =
        l match {
            case _ : IndexExp | _ : FieldExp =>
                true
            case _ =>
                super.isLvalue (l)
        }

}

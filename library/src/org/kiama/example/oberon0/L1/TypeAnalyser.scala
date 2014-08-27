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
package L1

trait TypeAnalyser extends L0.TypeAnalyser {

    import base.source.Expression
    import source.{IfStatement, WhileStatement}

    /**
     * The type expected of an expression as defined by its context.
     */
    override def exptypeDef : Expression => Type =
        {
            case tree.parent (_ : IfStatement | _ : WhileStatement) =>
                booleanType

            case n =>
                super.exptypeDef (n)
        }

}

/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2012 Anthony M Sloane, Macquarie University.
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
package L2

trait NameAnalyser extends L0.NameAnalyser {

    import L0.source.Expression
    import org.kiama.util.Messaging.message
    import org.kiama.util.Patterns.HasParent
    import source.{ForStatement, MinMaxCond, ValCond}

    override def rootconstexpDef : Expression => Boolean =
        {
            case HasParent (e1, ForStatement (_, _, _, Some (e2), _)) if e1 == e2 =>
                true
            case HasParent (_, _ : ValCond | _ : MinMaxCond) =>
                true
            case e =>
                super.rootconstexpDef (e)
        }

}

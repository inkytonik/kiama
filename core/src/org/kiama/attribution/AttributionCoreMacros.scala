/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2014 Anthony M Sloane, Macquarie University.
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
package attribution

import org.kiama.util.Compat210._

object AttributionMacros {

    import org.bitbucket.inkytonik.dsinfo.DSInfo.makeCallWithName

    import scala.reflect.macros._

    // Macros for the builder methods

    def attrMacro[T,U,A] (c : blackbox.Context) (f : c.Expr[T => U]): c.Expr[A] =
        makeCallWithName (c)

    def circularMacro[T,U,A] (c : blackbox.Context) (init : c.Expr[U]) (f : c.Expr[T => U]): c.Expr[A] =
        makeCallWithName (c)

    def dynAttrMacro[T,U,A] (c : blackbox.Context) (f : c.Expr[T => U]): c.Expr[A] =
        makeCallWithName (c)

    def paramAttrMacro[V,T,U,P] (c : blackbox.Context) (f : c.Expr[V => T => U]): c.Expr[P] =
        makeCallWithName (c)

}

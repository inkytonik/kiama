/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2013 Anthony M Sloane, Macquarie University.
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

/**
 * Common support for attribution of syntax trees in a functional style.
 * Includes circular and constant attributes but needs to be augmented with
 * basic attributes and parameterised attributes.
 */
trait AttributionCommon {

    import scala.language.experimental.macros

    /**
     * Define an optionally named circular attribute of `T` nodes of type `U`
     * by the function `f`. `f` is allowed to depend on the value of this
     * attribute, which will be given by `init` initially and will be evaluated
     * iteratively until a fixed point is reached (in conjunction with other
     * circular attributes on which it depends).  The final value is cached.
     * If `optNameDef` is not `None`, then `optNameDef.get` is used in
     * debugging output to identify this attribute.
     */
    def circular[T <: AnyRef,U] (init : U) (f : T => U) : T => U =
        macro AttributionCommonMacros.circularMacro[T,U]

    /**
     * As for the other `circular` with the first argument specifying a name for
     * the constructed attribute.
     */
    def circular[T <: AnyRef,U] (name : String, init : U) (f : T => U) : T => U =
        new CircularAttribute (name, init, f)

    /**
     * Define an optionally named constanat attribute of `T` nodes of type `U`
     * given by the value `u`. `u` is evaluated at most once. If `optNameDef`
     * is not `None`, then `optNameDef.get` is used in debugging output to
     * identify this attribute.
     */
    def constant[T <: AnyRef,U] (u : => U) : Attribute[T,U] =
        macro AttributionCommonMacros.constantMacro[T,U]

    /**
     * As for the other `constant` with the first argument specifying a name for
     * the constructed attribute.
     */
    def constant[T <: AnyRef,U] (name : String, u : => U) : Attribute[T,U] =
        new ConstantAttribute[T,U] (name, u)

    /**
     * Initialise the `Attributable` tree rooted at `t` so that it is ready for
     * attribution. At present, the only initialisation performed is to set node
     * attributes such as parent and children so that nodes can generically refer
     * to their neighbours. If you wish to use any of these properties, you must
     * call this method before doing so.  Otherwise, the node properties should
     * not be used and there is no need to call this method.
     */
    def initTree[T <: Attributable] (t : T) {
        t.initTreeProperties
    }

}

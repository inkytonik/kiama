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
 * Reusable implementation of attribution of syntax trees in a functional style
 * with attribute values computed each time they are accessed.
 */
trait UncachedAttributionCore extends AttributionCommon {

    import scala.language.experimental.macros

    /**
     * Define an uncached attribute of `T` nodes of type `U` by the function `f`,
     * which should not depend on the value of this attribute.  The computed
     * attribute value is cached so it will be computed at most once. If
     * `optNameDef` is not `None`, then `optNameDef.get` is used in debugging
     * output to identify this attribute.
     */
    def attr[T <: AnyRef,U] (f : T => U) : UncachedAttribute[T,U] =
        macro AttributionMacros.attrMacro[T,U,UncachedAttribute[T,U]]

    /**
     * As for the other `attr` with the first argument specifying a name for
     * the constructed attribute.
     */
    def attr[T <: AnyRef,U] (name : String, f : T => U) : UncachedAttribute[T,U] =
        new UncachedAttribute (name, f)

    /**
     * Define a parameterised uncached attribute of `T` nodes of type `U` by the
     * function `f`, which takes an argument of type `A`.  The computed attribute
     * value for a given `T` and `A` pair is cached so it will be computed at most
     * once.  If `optNameDef` is not `None`, then `optNameDef.get` and the `A`
     * value are used in debugging output to identify this attribute.
     */
    def paramAttr[V,T <: AnyRef,U] (f : V => T => U) : UncachedParamAttribute[V,T,U] =
        macro AttributionMacros.paramAttrMacro[V,T,U,UncachedParamAttribute[V,T,U]]

    /**
     * As for the other `paramAttr` with the first argument specifying a name for
     * the constructed attribute.
     */
    def paramAttr[V,T <: AnyRef,U] (name : String, f : V => T => U) : UncachedParamAttribute[V,T,U] =
        new UncachedParamAttribute (name, f)

    /**
     * Define an uncached attribute of `T` nodes of type `U` by the function `f`,
     * which takes the current node and its parent as its arguments. `T` must be
     * a sub-type of `Attributable` so that parents can be accessed generically.
     * If `optNameDef` is not `None`, then `optNameDef.get` is used in debugging
     * output to identify this attribute.
     */
    def childAttr[T <: Attributable,U] (f : T => Attributable => U) : UncachedAttribute[T,U] =
        macro AttributionMacros.childAttrMacro[T,U,UncachedAttribute[T,U]]

    /**
     * As for the other `childAttr` with the first argument specifying a name for
     * the constructed attribute.
     */
    def childAttr[T <: Attributable,U] (name : String, f : T => Attributable => U) : UncachedAttribute[T,U] =
        attr (name, (t : T) => f (t) (t.parent))

}

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
     * An attribute of a node type `T` with value of type `U`, supported by a circularity
     * test.  The value of the attribute is computed by the function `f`.  `f` will be
     * called each time the value of the attribute is accessed.  `f` should not itself
     * require the value of this attribute. If it does, a circularity error is reported
     * by throwing an `IllegalStateException`.
     */
    class UncachedAttribute[T <: AnyRef,U] (name : String, f : T => U) extends Attribute[T,U] (name) {

        import java.util.IdentityHashMap
        import org.bitbucket.inkytonik.dsprofile.Events.{finish, start}

        /**
         * Are we currently evaluating this attribute for a given tree?
         */
        private val visited = new IdentityHashMap[T,Unit]

        /**
         * Return the value of this attribute for node `t`, raising an error if
         * it depends on itself.
         */
        def apply (t : T) : U = {
            val i = start ("event" -> "AttrEval", "subject" -> t,
                           "attribute" -> this, "parameter" -> None,
                           "circular" -> false)
            if (visited containsKey t)
                reportCycle (t)
            else {
                visited.put (t, ())
                val u = f (t)
                visited.remove (t)
                finish (i, "value" -> u, "cached" -> false)
                u
            }
        }

    }

    /**
     * A variation of the `UncachedAttribute` class for parameterised attributes.
     */
    class UncachedParamAttribute[A,T <: AnyRef,U] (name : String, f : A => T => U) extends (A => Attribute[T,U]) {

        attr =>

        import java.util.IdentityHashMap
        import org.bitbucket.inkytonik.dsprofile.Events.{finish, start}

        /**
         * Are we currently evaluating this attribute for a given argument and tree?
         */
        private val visited = new IdentityHashMap[ParamAttributeKey,Unit]

        /**
         * Return the value of this attribute for node `t`, raising an error if
         * it depends on itself.
         */
        def apply (arg : A) : Attribute[T,U] =
            new Attribute[T,U] (name) {

                def apply (t : T) : U = {
                    val i = start ("event" -> "AttrEval", "subject" -> t,
                                   "attribute" -> this, "parameter" -> Some (arg),
                                   "circular" -> false)
                    val key = new ParamAttributeKey (arg, t)
                    if (visited containsKey key)
                        throw new IllegalStateException ("Cycle detected in attribute evaluation")
                    else {
                        visited.put (key, ())
                        val u = f (arg) (t)
                        visited.remove (key)
                        finish (i, "value" -> u, "cached" -> false)
                        u
                    }
                }

            }

    }

    /**
     * Define an uncached attribute of `T` nodes of type `U` by the function `f`,
     * which should not depend on the value of this attribute.  The computed
     * attribute value is cached so it will be computed at most once.
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
     * once.
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

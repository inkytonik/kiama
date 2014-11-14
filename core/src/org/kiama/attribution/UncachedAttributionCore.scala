/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2014 Anthony M Sloane, Macquarie University.
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

import org.kiama.util.Memoiser

/**
 * Reusable implementation of attribution of syntax trees in a functional style
 * with attribute values computed each time they are accessed.
 */
trait UncachedAttributionCore extends AttributionCommon with Memoiser {

    import scala.collection.immutable.Seq
    import scala.language.experimental.macros

    /**
     * An attribute of a node type `T` with value of type `U`, supported by a circularity
     * test.  The value of the attribute is computed by the function `f`.  `f` will be
     * called each time the value of the attribute is accessed.  `f` should not itself
     * require the value of this attribute. If it does, a circularity error is reported
     * by throwing an `IllegalStateException`.
     */
    class UncachedAttribute[T,U] (name : String, f : T => U) extends
            Attribute[T,U] (name) with IdMemoised[T,Unit] {

        import org.bitbucket.inkytonik.dsprofile.Events.{finish, start}

        /**
         * Return the value of this attribute for node `t`, raising an error if
         * it depends on itself.
         */
        def apply (t : T) : U = {
            val i = start (Seq ("event" -> "AttrEval", "subject" -> t,
                                "attribute" -> this, "parameter" -> None,
                                "circular" -> false))
            get (t) match {
                case Some (()) =>
                    reportCycle (t)
                case None =>
                    put (t, ())
                    val u = f (t)
                    resetAt (t)
                    finish (i, Seq ("value" -> u, "cached" -> false))
                    u
            }

        }

    }

    /**
     * A variation of the `UncachedAttribute` class for parameterised attributes.
     */
    class UncachedParamAttribute[A,T,U] (name : String, f : A => T => U) extends
            (A => Attribute[T,U]) with Memoised[ParamAttributeKey,Unit] {

        attr =>

        import org.bitbucket.inkytonik.dsprofile.Events.{finish, start}

        /**
         * Return the value of this attribute for node `t`, raising an error if
         * it depends on itself.
         */
        def apply (arg : A) : Attribute[T,U] =
            new Attribute[T,U] (name) {

                def apply (t : T) : U = {
                    val i = start (Seq ("event" -> "AttrEval", "subject" -> t,
                                        "attribute" -> this, "parameter" -> Some (arg),
                                        "circular" -> false))
                    val key = new ParamAttributeKey (arg, t)
                    get (key) match {
                        case Some (()) =>
                            reportCycle (t)
                        case None =>
                            put (key, ())
                            val u = f (arg) (t)
                            resetAt (key)
                            finish (i, Seq ("value" -> u, "cached" -> false))
                            u
                    }
                }

                override def reportCycle (t : T) : U =
                    throw new IllegalStateException (s"Cycle detected in attribute evaluation '$name' ($arg) at $t")

            }

    }

    /**
     * Define an uncached attribute of `T` nodes of type `U` by the function `f`,
     * which should not depend on the value of this attribute.  The computed
     * attribute value is cached so it will be computed at most once.
     */
    def attr[T,U] (f : T => U) : UncachedAttribute[T,U] =
        macro AttributionMacros.attrMacro[T,U,UncachedAttribute[T,U]]

    /**
     * As for the other `attr` with the first argument specifying a name for
     * the constructed attribute.
     */
    def attr[T,U] (name : String, f : T => U) : UncachedAttribute[T,U] =
        new UncachedAttribute (name, f)

    /**
     * Define a parameterised uncached attribute of `T` nodes of type `U` by the
     * function `f`, which takes an argument of type `A`.  The computed attribute
     * value for a given `T` and `A` pair is cached so it will be computed at most
     * once.
     */
    def paramAttr[V,T,U] (f : V => T => U) : UncachedParamAttribute[V,T,U] =
        macro AttributionMacros.paramAttrMacro[V,T,U,UncachedParamAttribute[V,T,U]]

    /**
     * As for the other `paramAttr` with the first argument specifying a name for
     * the constructed attribute.
     */
    def paramAttr[V,T,U] (name : String, f : V => T => U) : UncachedParamAttribute[V,T,U] =
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

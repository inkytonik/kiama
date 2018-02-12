/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package attribution

import org.bitbucket.inkytonik.kiama.util.Memoiser.{makeIdMemoiser, makeMemoiser}

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
    class UncachedAttribute[T, U](name : String, f : T => U) extends Attribute[T, U](name) {

        import org.bitbucket.inkytonik.dsprofile.Events.{finish, start}

        /**
         * Backing memo table.
         */
        val memo = makeIdMemoiser[T, Unit]()

        /**
         * Return the value of this attribute for node `t`, raising an error if
         * it depends on itself.
         */
        def apply(t : T) : U = {
            val i = start(List("event" -> "AttrEval", "subject" -> t,
                "attribute" -> this, "parameter" -> None,
                "circular" -> false))
            memo.get(t) match {
                case Some(()) =>
                    reportCycle(t)
                case None =>
                    memo.put(t, ())
                    val u = f(t)
                    memo.resetAt(t)
                    finish(i, List("value" -> u, "cached" -> false))
                    u
            }

        }

    }

    /**
     * A variation of the `UncachedAttribute` class for parameterised attributes.
     */
    class UncachedParamAttribute[A, T, U](name : String, f : A => T => U) extends (A => Attribute[T, U]) {

        attr =>

        import org.bitbucket.inkytonik.dsprofile.Events.{finish, start}

        /**
         * Backing memo table.
         */
        val memo = makeMemoiser[ParamAttributeKey, Unit]()

        /**
         * Return the value of this attribute for node `t`, raising an error if
         * it depends on itself.
         */
        def apply(arg : A) : Attribute[T, U] =
            new Attribute[T, U](name) {

                def apply(t : T) : U = {
                    val i = start(List("event" -> "AttrEval", "subject" -> t,
                        "attribute" -> this, "parameter" -> Some(arg),
                        "circular" -> false))
                    val key = new ParamAttributeKey(arg, t)
                    memo.get(key) match {
                        case Some(()) =>
                            reportCycle(t)
                        case None =>
                            memo.put(key, ())
                            val u = f(arg)(t)
                            memo.resetAt(key)
                            finish(i, List("value" -> u, "cached" -> false))
                            u
                    }
                }

                override def reportCycle(t : T) : U =
                    throw new IllegalStateException(s"Cycle detected in attribute evaluation '$name' ($arg) at $t")

            }

    }

    /**
     * Define an uncached attribute of `T` nodes of type `U` by the function `f`,
     * which should not depend on the value of this attribute.  The computed
     * attribute value is cached so it will be computed at most once.
     */
    def attr[T, U](f : T => U) : UncachedAttribute[T, U] = macro UncachedAttributionCoreMacros.attrMacro[T, U, UncachedAttribute[T, U]]

    /**
     * As for the other `attr` with the first argument specifying a name for
     * the constructed attribute.
     */
    def attrWithName[T, U](name : String, f : T => U) : UncachedAttribute[T, U] =
        new UncachedAttribute(name, f)

    /**
     * Define a parameterised uncached attribute of `T` nodes of type `U` by the
     * function `f`, which takes an argument of type `A`.  The computed attribute
     * value for a given `T` and `A` pair is cached so it will be computed at most
     * once.
     */
    def paramAttr[V, T, U](f : V => T => U) : UncachedParamAttribute[V, T, U] = macro UncachedAttributionCoreMacros.paramAttrMacro[V, T, U, UncachedParamAttribute[V, T, U]]

    /**
     * As for the other `paramAttr` with the first argument specifying a name for
     * the constructed attribute.
     */
    def paramAttrWithName[V, T, U](name : String, f : V => T => U) : UncachedParamAttribute[V, T, U] =
        new UncachedParamAttribute(name, f)

}

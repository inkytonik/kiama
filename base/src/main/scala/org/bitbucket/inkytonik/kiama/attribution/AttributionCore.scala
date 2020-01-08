/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2020 Anthony M Sloane, Macquarie University.
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
 * with attribute values cached so that each value is computed at most once.
 */
trait AttributionCore extends AttributionCommon {

    import org.bitbucket.inkytonik.dsprofile.Events.{finish, start}
    import scala.language.experimental.macros
    import scala.language.implicitConversions

    /**
     * An attribute of a node type `T` with value of type `U`, supported by a memo
     * table and circularity test.  The value of the attribute is computed by
     * the function `f`.  The result is memoised so that it is only evaluated once.
     * `f` should not itself require the value of this attribute. If it does, a
     * circularity error is reported by throwing an `IllegalStateException`.
     */
    class CachedAttribute[T <: AnyRef, U](name : String, f : T => U) extends Attribute[T, U](name) {

        /**
         * Backing memo table.
         */
        val memo = makeIdMemoiser[T, Option[U]]

        /**
         * Return the value of this attribute for node `t`, raising an error if
         * it depends on itself.
         */
        def apply(t : T) : U =
            t.synchronized {
                val i = start(List("event" -> "AttrEval", "subject" -> t,
                    "attribute" -> this, "parameter" -> None,
                    "circular" -> false))
                memo.get(t) match {
                    case Some(Some(u)) =>
                        finish(i, List("value" -> u, "cached" -> true))
                        u
                    case Some(None) =>
                        reportCycle(t)
                    case None =>
                        try {
                            memo.put(t, None)
                            val u = f(t)
                            memo.put(t, Some(u))
                            finish(i, List("value" -> u, "cached" -> false))
                            u
                        } catch {
                            case e : Exception =>
                                memo.resetAt(t)
                                throw e
                        }
                }
            }

        /**
         * Has the value of this attribute at `t` already been computed or not?
         * If the table contains `Some (u)` then we've computed it and the value
         * was `u`. If the memo table contains `None` we are in the middle of
         * computing it. Otherwise the memo table contains no entry for `t`.
         */
        def hasBeenComputedAt(t : T) : Boolean =
            memo.get(t) match {
                case Some(Some(_)) => true
                case _             => false
            }

        /**
         * Reset the cache for this attribute.
         */
        def reset() : Unit = {
            memo.reset()
        }

    }

    /**
     * A variation of the `CachedAttribute` class for parameterised attributes.
     */
    class CachedParamAttribute[A, T <: AnyRef, U](name : String, f : A => T => U) extends (A => Attribute[T, U]) {

        attr =>

        /**
         * Backing memo table.
         */
        val memo = makeMemoiser[ParamAttributeKey, Option[U]]()

        /**
         * Return the value of this attribute for node `t`, raising an error if
         * it depends on itself.
         */
        def apply(arg : A) : Attribute[T, U] =
            new Attribute[T, U](name) {

                def apply(t : T) : U =
                    t.synchronized {
                        val i = start(List("event" -> "AttrEval", "subject" -> t,
                            "attribute" -> this, "parameter" -> Some(arg),
                            "circular" -> false))
                        val key = new ParamAttributeKey(arg, t)
                        memo.get(key) match {
                            case Some(Some(u)) =>
                                finish(i, List("value" -> u, "cached" -> true))
                                u
                            case Some(None) =>
                                reportCycle(t)
                            case None =>
                                try {
                                    memo.put(key, None)
                                    val u = f(arg)(t)
                                    memo.put(key, Some(u))
                                    finish(i, List("value" -> u, "cached" -> false))
                                    u
                                } catch {
                                    case e : Exception =>
                                        memo.resetAt(key)
                                        throw e
                                }
                        }
                    }

                override def reportCycle(t : T) : U =
                    throw new IllegalStateException(s"Cycle detected in attribute evaluation '$name' ($arg) at $t")

            }

        /**
         * Has the value of this attribute at `t` already been computed for `arg`
         * or not?
         */
        def hasBeenComputedAt(arg : A, t : T) : Boolean =
            memo.hasBeenComputedAt(new ParamAttributeKey(arg, t))

        /**
         * Reset the cache for this attribute.
         */
        def reset() : Unit = {
            memo.reset()
        }

        /**
         * Reset this attribute's cache at `t` for just parameter value `arg`.
         */
        def resetAt(arg : A, t : T) : Unit = {
            memo.resetAt(new ParamAttributeKey(arg, t))
        }

    }

    /**
     * A cached dynamic attribute of a node type `T` with value of type `U`, supported
     * by a memo table and circularity test.  The value of the attribute is initially
     * computed by the function `f`, but the definition can be augmented dynamically.
     * The result is memoised so that it is only evaluated once for a given definition.
     * The attribute definition should not itself require the value of this attribute.
     * If it does, a circularity error is reported by throwing an `IllegalStateException`.
     * This kind of attribute encapsulates state to keep track of the current definition,
     * so an instance should only be used from one thread at a time.
     */
    class CachedDynamicAttribute[T <: AnyRef, U](name : String, f : T => U) extends CachedAttribute[T, U](name, f) {

        /**
         * List of functions that currently dynamically define this attribute.
         */
        val functions = scala.collection.mutable.ListBuffer[T ==> U]()

        /**
         * Return the value of this attribute for node `t`, raising an error if
         * it depends on itself.  If the value has been memoised, return it.
         * Otherwise, the functions list is tried in order. The first partial
         * function on that list that is defined will be used. If no partial
         * function on this list is defined, then `f` will be used.
         */
        override def apply(t : T) : U =
            t.synchronized {
                memo.get(t) match {
                    case Some(Some(u)) =>
                        u
                    case Some(None) =>
                        reportCycle(t)
                    case None =>
                        try {
                            memo.put(t, None)
                            val pf = functions.find(_.isDefinedAt(t))
                            val func = pf.getOrElse(f)
                            val u = func(t)
                            memo.put(t, Some(u))
                            u
                        } catch {
                            case e : Exception =>
                                memo.resetAt(t)
                                throw e
                        }
                }
            }

        /**
         * Add a new partial function to the definition of this attribute.
         * Resets the memo table for this attribute.
         */
        def +=(g : T ==> U) : Unit = {
            g +=: functions
            memo.reset()
        }

        /**
         * Remove a partial function from the definition of this attribute, if it's
         * there, otherwise have no effect. If `g` appears more than once in the
         * definition, just remove the first one. Resets the memo table for this
         * attribute.
         */
        def -=(g : T ==> U) : Unit = {
            functions -= g
            memo.reset()
        }

        /**
         * Execute a block and restore the state of this dynamic attribute after
         * the block has executed. The idea is that the block can add to the
         * definition of the attribute for use within the block. Any definitions
         * that are added will be automatically removed at the end of the block.
         */
        def block(b : => Any) : Unit = {
            val savedFunctions = functions.toList
            b
            functions.clear()
            functions.appendAll(savedFunctions)
        }

    }

    /**
     * Global state for the circular attribute evaluation algorithm.
     */
    private object CircularAttribute {

        /**
         * Are we currently evaluation a circle of attributes?
         */
        var IN_CIRCLE = false

        /**
         * Has an attribute on the current circle changed value since the
         * last time it was computed?
         */
        var CHANGE = false

        /**
         * Are we in the final clean-up pass around the circle?
         */
        var READY = false

        /**
         * Reset the circular attribute evaluation state.
         */
        def resetState() : Unit = {
            IN_CIRCLE = false
            CHANGE = false
            READY = false
        }

    }

    /**
     * An attribute of a node type `T` with value of type `U` which has a circular
     * definition.  The value of the attribute is computed by the function f
     * which may itself use the value of the attribute.  init specifies an
     * initial value for the attribute.  The attribute (and any circular attributes
     * on which it depends) are evaluated until no value changes (i.e., a fixed
     * point is reached).  The final result is memoised so that subsequent evaluations
     * return the same value.
     *
     * This code implements the basic circular evaluation algorithm from "Circular
     * Reference Attributed Grammars - their Evaluation and Applications", by Magnusson
     * and Hedin from LDTA 2003.
     */
    class CircularAttribute[T <: AnyRef, U](name : String, init : U, f : T => U) extends Attribute[T, U](name) {

        import CircularAttribute._
        import org.bitbucket.inkytonik.dsprofile.Events.{finish, start}
        import org.bitbucket.inkytonik.kiama.util.WeakIdentityHashSet

        /**
         * Backing memo table.
         */
        val memo = makeIdMemoiser[T, U]()

        /**
         * Has the value of this attribute for a given tree already been computed?
         */
        private val computed = new WeakIdentityHashSet[T]

        /**
         * Has the attribute for given tree been computed on this iteration of the
         * circular evaluation?
         */
        private val visited = new WeakIdentityHashSet[T]

        /**
         * Return the value of the attribute for tree `t`, or the initial value if
         * no value for `t` has been computed.
         */
        private def value(t : T) : U =
            memo.getOrDefault(t, init)

        /**
         * Run the semantic function `f` in a safe manner. We need to guard against
         * `f` throwing an exception which aborts the computation, since we could
         * then leave the global circular attribute state in a funny state.
         */
        def safef(t : T) : U =
            try {
                f(t)
            } catch {
                case e : Exception =>
                    resetState()
                    throw e
            }

        /**
         * Return the value of this attribute for node `t`.  Essentially Figure 6
         * from the CRAG paper, plus the READY optimisation (section 3.3).
         */
        def apply(t : T) : U =
            t.synchronized {
                if (hasBeenComputedAt(t)) {

                    // We have previously computed this attribute occurrence so fetch it from the cache.

                    val i = start(List("event" -> "AttrEval", "subject" -> t, "attribute" -> this,
                        "parameter" -> None, "circular" -> true, "phase" -> "computed"))
                    val u = value(t)
                    finish(i, List("value" -> u, "cached" -> true, "phase" -> "computed"))
                    u

                } else if (!IN_CIRCLE) {

                    // This is the first evaluation of a circular attribute occurrence, so enter
                    // a fixed-point computation that computes it and all dependent attribute
                    // occurrences until they stabilise.

                    IN_CIRCLE = true
                    visited.add(t)
                    do {

                        // Evaluate the attribute occurrence once. Compare the value that is
                        // computed (newu) with the previous value (u). If they are the same,
                        // we are done, since it and all dependent occurrences have stabilised.
                        // If the values are different, cache the new one and repeat.

                        val i = start(List("event" -> "AttrEval", "subject" -> t,
                            "attribute" -> this, "parameter" -> None,
                            "circular" -> true, "phase" -> "iterate"))
                        CHANGE = false
                        val u = value(t)
                        val newu = safef(t)
                        if (u == newu) {
                            finish(i, List("value" -> u, "cached" -> false, "phase" -> "iteratenochange"))
                        } else {
                            finish(i, List("value" -> newu, "cached" -> false, "phase" -> "iteratechange"))
                            CHANGE = true
                            memo.put(t, newu)
                        }

                    } while (CHANGE)

                    // The value of this attribute at t has been computed and cached.
                    computed.add(t)

                    // All of the values of dependent attribute occurences are also final, but have
                    // not yet been cached. Enter READY mode to go around the circle one more time
                    // to cache them.

                    READY = true
                    val u = safef(t)
                    READY = false

                    // Now we have computed and cached all of the attribute occurrences on the circle
                    // so we are done with this one. Return the final value of the initial attribute
                    // occurrence.

                    visited.remove(t)
                    IN_CIRCLE = false
                    u

                } else if (!(visited contains t)) {

                    if (READY) {

                        // We get to this point if a fixed-point iteration has ended with no changes.
                        // The value of the initial attribute occurrence of the circle has stabilised,
                        // been cached and marked as computed. Since a fixed-point has been reached,
                        // it must be that all dependent attribute occurrences have also stabilised
                        // and been cached, so in the READY phase we do one more iteration to mark
                        // them as computed as well. This code handles an occurrence that hasn't yet
                        // been visited on this last iteration.

                        computed.add(t)
                        visited.add(t)
                        val u = safef(t)
                        visited.remove(t)
                        u

                    } else {

                        // We are in a circle but not at the beginning of it. In other words, we are
                        // evaluating a circular attribute occurrence on which the initial circular
                        // attribute occurrence depends. We reach here if we have not previously
                        // visited this occurrence on this iteration of the fixed-point computation.
                        // Evaluate this attribute occurrence. As for the initial attribute occurrence
                        // above, if the value changes, note that something has changed on the cycle,
                        // and cache the new value.

                        val i = start(List("event" -> "AttrEval", "subject" -> t,
                            "attribute" -> this, "parameter" -> None,
                            "circular" -> true, "phase" -> "notvisited"))
                        visited.add(t)
                        val u = value(t)
                        val newu = safef(t)
                        visited.remove(t)
                        if (u == newu) {
                            finish(i, List("value" -> u, "cached" -> false, "phase" -> "notvisitednochange"))
                            u
                        } else {
                            finish(i, List("value" -> newu, "cached" -> false, "phase" -> "notvisitedchange"))
                            CHANGE = true
                            memo.put(t, newu)
                            newu
                        }

                    }

                } else {

                    // We reach this point if we ask for the value of a circular attribute occurrence
                    // and we have already visited it in the current fixed-point iteration. We just
                    // return the cached value since that is our view of the value of this attribute
                    // so far.

                    val i = start(List("event" -> "AttrEval", "subject" -> t,
                        "attribute" -> this, "parameter" -> None,
                        "circular" -> true, "phase" -> "default"))
                    val u = value(t)
                    finish(i, List("value" -> u, "cached" -> false, "phase" -> "default"))
                    u

                }
            }

        /**
         * Immediately reset this attribute's memoisation cache.
         */
        def reset() : Unit = {
            memo.reset()
            computed.clear()
            visited.clear()
        }

        /**
         * Has the value of this attribute at `t` already been computed or not?
         */
        def hasBeenComputedAt(t : T) : Boolean =
            computed contains t

    }

    /**
     * Define a cached attribute of `T` nodes of type `U` by the function `f`,
     * which should not depend on the value of this attribute.  The computed
     * attribute value is cached so it will be computed at most once.
     */
    def attr[T <: AnyRef, U](f : T => U) : CachedAttribute[T, U] = macro AttributionCoreMacros.attrMacro[T, U, CachedAttribute[T, U]]

    /**
     * As for the other `attr` with the first argument specifying a name for
     * the constructed attribute.
     */
    def attrWithName[T <: AnyRef, U](name : String, f : T => U) : CachedAttribute[T, U] =
        new CachedAttribute(name, f)

    /**
     * Define a cached dynamic attribute of `T` nodes of type `U` by the partial
     * function `f`, which should not depend on the value of this attribute.
     * The computed attribute value is cached so it will be computed at most once.
     */
    def dynAttr[T <: AnyRef, U](f : T => U) : CachedDynamicAttribute[T, U] = macro AttributionCoreMacros.dynAttrMacro[T, U, CachedDynamicAttribute[T, U]]

    /**
     * As for the other `dynAttr` with the first argument specifying a name for
     * the constructed attribute.
     */
    def dynAttrWithName[T <: AnyRef, U](name : String, f : T => U) : CachedDynamicAttribute[T, U] =
        new CachedDynamicAttribute(name, f)

    /**
     * Define a parameterised attribute of `T` nodes of type `U` by the function
     * `f`, which takes an argument of type `V`.  The computed attribute value
     * for a given `V` and `T` pair is cached so it will be computed at most
     * once.
     */
    def paramAttr[V, T <: AnyRef, U](f : V => T => U) : CachedParamAttribute[V, T, U] = macro AttributionCoreMacros.paramAttrMacro[V, T, U, CachedParamAttribute[V, T, U]]

    /**
     * As for the other `paramAttr` with the first argument specifying a name for
     * the constructed attribute.
     */
    def paramAttrWithName[V, T <: AnyRef, U](name : String, f : V => T => U) : CachedParamAttribute[V, T, U] =
        new CachedParamAttribute(name, f)

    /**
     * Implicitly converts functions to dynamic attributes. This conversion allows us
     * to use simpler types for dynamic attributes, but still extend them later.
     */
    implicit def internalToDynamicAttribute[T <: AnyRef, U](f : T => U) : CachedDynamicAttribute[T, U] =
        f match {
            case f : CachedDynamicAttribute[_, _] =>
                f.asInstanceOf[CachedDynamicAttribute[T, U]]
            case f =>
                throw new UnsupportedOperationException("Can only extend the definition of dynamic attributes")
        }

    /**
     * Define an optionally named circular attribute of `T` nodes of type `U`
     * by the function `f`. `f` is allowed to depend on the value of this
     * attribute, which will be given by `init` initially and will be evaluated
     * iteratively until a fixed point is reached (in conjunction with other
     * circular attributes on which it depends).  The final value is cached.
     */
    def circular[T <: AnyRef, U](init : U)(f : T => U) : CircularAttribute[T, U] = macro AttributionCoreMacros.circularMacro[T, U, CircularAttribute[T, U]]

    /**
     * As for the other `circular` with the first argument specifying a name for
     * the constructed attribute.
     */
    def circularWithName[T <: AnyRef, U](name : String, init : U)(f : T => U) : CircularAttribute[T, U] =
        new CircularAttribute(name, init, f)

}

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

import org.kiama.util.Memoiser

/**
 * Reusable implementation of attribution of syntax trees in a functional style
 * with attribute values cached so that each value is computed at most once.
 */
trait AttributionCore extends AttributionCommon with Memoiser {

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
    class CachedAttribute[T <: AnyRef,U] (name : String, f : T => U) extends
            Attribute[T,U] (name) with IdMemoised[T,Option[U]] {

        /**
         * Return the value of this attribute for node `t`, raising an error if
         * it depends on itself.
         */
        def apply (t : T) : U = {
            val i = start ("event" -> "AttrEval", "subject" -> t,
                           "attribute" -> this, "parameter" -> None,
                           "circular" -> false)
            resetIfRequested ()
            memo.get (t) match {
                case None     => reportCycle (t)
                case Some (u) => finish (i, "value" -> u, "cached" -> true)
                                 u
                case _        => // null
                                 memo.put (t, None)
                                 val u = f (t)
                                 memo.put (t, Some (u))
                                 finish (i, "value" -> u, "cached" -> false)
                                 u
            }
        }

        /**
         * Has the value of this attribute at `t` already been computed or not?
         * If the table contains `Some (u)` then we've compute it and the value
         * was `u`. If the memo table contains `None` we are in the middle of
         * computing it. Otherwise the memo table contains no entry for `t`.
         */
        override def hasBeenComputedAt (t : T) : Boolean = {
            resetIfRequested ()
            memo.get (t) match {
                case Some (_) => true
                case _        => // null, None
                                 false
            }
        }

    }

    /**
     * A variation of the `CachedAttribute` class for parameterised attributes.
     */
    class CachedParamAttribute[A,T <: AnyRef,U] (name : String, f : A => T => U) extends
            (A => Attribute[T,U]) with Memoised[ParamAttributeKey,Option[U]] {

        attr =>

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
                    resetIfRequested ()
                    val key = new ParamAttributeKey (arg, t)
                    memo.get (key) match {
                        case Some (None)     => reportCycle (t)
                        case Some (Some (u)) => finish (i, "value" -> u, "cached" -> true)
                                                u
                        case None            => memo.put (key, None)
                                                val u = f (arg) (t)
                                                memo.put (key, Some (u))
                                                finish (i, "value" -> u, "cached" -> false)
                                                u
                    }
                }

                override def reportCycle (t : T) : U =
                    throw new IllegalStateException (s"Cycle detected in attribute evaluation '$name' ($arg) at $t")

            }

        /**
         * Has the value of this attribute at `t` already been computed for `arg`
         * or not?
         */
        def hasBeenComputedAt (arg : A, t : T) : Boolean = {
            val key = new ParamAttributeKey (arg, t)
            memo.get (key) match {
                case Some (Some (_)) => true
                case _               => // None, Some (None)
                                        false
            }
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
    class CachedDynamicAttribute[T <: AnyRef,U] (name : String, f : T => U) extends CachedAttribute[T,U] (name, f) {

        /**
         * List of functions that currently dynamically define this attribute.
         */
        val functions = scala.collection.mutable.ListBuffer[T ==> U] ()

        /**
         * Return the value of this attribute for node `t`, raising an error if
         * it depends on itself.  IF the value has been memoised, return it.
         * Otherwise, the functions list is tried in order. The first partial
         * function on that list that is defined will be used. If no partial
         * function on this list is defined, then `f` will be used.
         */
        override def apply (t : T) : U = {
            resetIfRequested ()
            memo.get (t) match {
                case None     => reportCycle (t)
                case Some (u) => u
                case _        => // null
                                 memo.put (t, None)
                                 val pf = functions.find (_.isDefinedAt (t))
                                 val func = pf.getOrElse (f)
                                 val u = func (t)
                                 memo.put (t, Some (u))
                                 u
            }
        }

        /**
         * Add a new partial function to the definition of this attribute.
         * Resets the memo table for this attribute.
         */
        def += (g : T ==> U) {
            g +=: functions
            reset ()
        }

        /**
         * Remove a partial function from the definition of this attribute, if it's
         * there, otherwise have no effect. If `g` appears more than once in the
         * definition, just remove the first one. Resets the memo table for this
         * attribute.
         */
        def -= (g : T ==> U) {
            functions -= g
            reset ()
        }

        /**
         * Execute a block and restore the state of this dynamic attribute after
         * the block has executed. The idea is that the block can add to the
         * definition of the attribute for use within the block. Any definitions
         * that are added will be automatically removed at the end of the block.
         */
        def block (b : => Any) {
            val savedFunctions = functions.toList
            b
            functions.clear ()
            functions.appendAll (savedFunctions)
        }

    }

    /**
     * Global state for the circular attribute evaluation algorithm
     * and the memoisation tables.
     */
    private object CircularAttribute {
        var IN_CIRCLE = false
        var CHANGE = false
        var READY = false
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
    class CircularAttribute[T <: AnyRef,U] (name : String, init : U, f : T => U) extends
            Attribute[T,U] (name) with IdMemoised[T,U] {

        import CircularAttribute._
        import org.bitbucket.inkytonik.dsprofile.Events.{finish, start}

        /**
         * Has the value of this attribute for a given tree already been computed?
         */
        private val computed = new java.util.IdentityHashMap[T,Unit]

        /**
         * Has the attribute for given tree been computed on this iteration of the
         * circular evaluation?
         */
        private val visited = new java.util.IdentityHashMap[T,Unit]

        /**
         * Return the value of the attribute for tree `t`, or the initial value if
         * no value for `t` has been computed.
         */
        private def value (t : T) : U = {
            val v = memo.get (t)
            if (v == null)
                init
            else
                v
        }

        /**
         * Return the value of this attribute for node `t`.  Essentially Figure 6
         * from the CRAG paper, plus the READY optimisation (section 3.3).
         */
        def apply (t : T) : U = {
            resetIfRequested ()

            if (computed containsKey t) {

                // We have previously computed this attribute occurrence so fetch it from the cache.

                val i = start ("event" -> "AttrEval", "subject" -> t, "attribute" -> this,
                               "parameter" -> None, "circular" -> true, "phase" -> "computed")
                val u = value (t)
                finish (i, "value" -> u, "cached" -> true, "phase" -> "computed")
                u

            } else if (!IN_CIRCLE) {

                // This is the first evaluation of a circular attribute occurrence, so enter
                // a fixed-point computation that cmoputes it and all dependent attribute
                // occurrences until they stabilise.

                IN_CIRCLE = true
                visited.put (t, ())
                do {

                    // Evaluate the attribute occurrence once. Compare the value that is
                    // computed (newu) with the previous value (u). If they are the same,
                    // we are done, since it and all dependent occurrences have stabilised.
                    // If the values are different, cache the new one and repeat.

                    val i = start ("event" -> "AttrEval", "subject" -> t,
                                   "attribute" -> this, "parameter" -> None,
                                   "circular" -> true, "phase" -> "iterate")
                    CHANGE = false
                    val u = value (t)
                    val newu = f (t)
                    if (u == newu) {
                        finish (i, "value" -> u, "cached" -> false, "phase" -> "iteratenochange")
                    } else {
                        finish (i, "value" -> newu, "cached" -> false, "phase" -> "iteratechange")
                        CHANGE = true
                        memo.put (t, newu)
                    }

                } while (CHANGE)

                // The value of this attribute at t has been computed and cached.
                computed.put (t, ())

                // All of the values of dependent attribute occurences are also final, but have
                // not yet been cached. Enter READY mode to go around the circle one more time
                // to cache them.

                READY = true
                val u = f (t)
                READY = false

                // Now we have computed and cached all of the attribute occurrences on the circle
                // so we are done with this one. Return the final value of the initial attribute
                // occurrence.

                visited.remove (t)
                IN_CIRCLE = false
                u

            } else if (! (visited containsKey t)) {

                if (READY) {

                    // We get to this point if a fixed-point iteration has ended with no changes.
                    // The value of the initial attribute occurrence of the circle has stabilised,
                    // been cached and marked as computed. Since a fixed-point has been reached,
                    // it must be that all dependent attribute occurrences have also stabilised
                    // and been cached, so in the READY phase we do one more iteration to mark
                    // them as computed as well. This code handles an occurrence that hasn't yet
                    // been visited on this last iteration.

                    computed.put (t, ())
                    visited.put (t, ())
                    val u = f (t)
                    visited.remove (t)
                    u

                } else {

                    // We are in a circle but not at the beginning of it. In other words, we are
                    // evaluating a circular attribute occurrence on which the initial circular
                    // attribute occurrence depends. We reach here if we have not previously
                    // visited this occurrence on this iteration of the fixed-point computation.
                    // Evaluate this attribute occurrence. As for the initial attribute occurrence
                    // above, if the value changes, note that something has changed on the cycle,
                    // and cache the new value.

                    val i = start ("event" -> "AttrEval", "subject" -> t,
                                   "attribute" -> this, "parameter" -> None,
                                   "circular" -> true, "phase" -> "notvisited")
                    visited.put (t, ())
                    val u = value (t)
                    val newu = f (t)
                    visited.remove (t)
                    if (u == newu) {
                        finish (i, "value" -> u, "cached" -> false, "phase" -> "notvisitednochange")
                        u
                    } else {
                        finish (i, "value" -> newu, "cached" -> false, "phase" -> "notvisitedchange")
                        CHANGE = true
                        memo.put (t, newu)
                        newu
                    }

                }

            } else {

                // We reach this point if we ask for the value of a circular attribute occurrence
                // and we have already visited it in the current fixed-point iteration. We just
                // return the cached value since that is our view of the value of this attribute
                // so far.

                val i = start ("event" -> "AttrEval", "subject" -> t,
                               "attribute" -> this, "parameter" -> None,
                               "circular" -> true, "phase" -> "default")
                val u = value (t)
                finish (i, "value" -> u, "cached" -> false, "phase" -> "default")
                u

            }
        }

        /**
         * Immediately reset this attribute's memoisation cache.
         */
        override def reset () {
            super.reset ()
            computed.clear ()
            visited.clear ()
        }

    }

    /**
     * Define a cached attribute of `T` nodes of type `U` by the function `f`,
     * which should not depend on the value of this attribute.  The computed
     * attribute value is cached so it will be computed at most once.
     */
    def attr[T <: AnyRef,U] (f : T => U) : CachedAttribute[T,U] =
        macro AttributionMacros.attrMacro[T,U,CachedAttribute[T,U]]

    /**
     * As for the other `attr` with the first argument specifying a name for
     * the constructed attribute.
     */
    def attr[T <: AnyRef,U] (name : String, f : T => U) : CachedAttribute[T,U] =
        new CachedAttribute (name, f)

    /**
     * Define a cached dynamic attribute of `T` nodes of type `U` by the partial
     * function `f`, which should not depend on the value of this attribute.
     * The computed attribute value is cached so it will be computed at most once.
     */
    def dynAttr[T <: AnyRef,U] (f : T => U) : CachedDynamicAttribute[T,U] =
        macro AttributionMacros.dynAttrMacro[T,U,CachedDynamicAttribute[T,U]]

    /**
     * As for the other `dynAttr` with the first argument specifying a name for
     * the constructed attribute.
     */
    def dynAttr[T <: AnyRef,U] (name : String, f : T => U) : CachedDynamicAttribute[T,U] =
        new CachedDynamicAttribute (name, f)

    /**
     * Define a parameterised attribute of `T` nodes of type `U` by the function
     * `f`, which takes an argument of type `A`.  The computed attribute value
     * for a given `T` and `A` pair is cached so it will be computed at most
     * once.
     */
    def paramAttr[V,T <: AnyRef,U] (f : V => T => U) : CachedParamAttribute[V,T,U] =
        macro AttributionMacros.paramAttrMacro[V,T,U,CachedParamAttribute[V,T,U]]

    /**
     * As for the other `paramAttr` with the first argument specifying a name for
     * the constructed attribute.
     */
    def paramAttr[V,T <: AnyRef,U] (name : String, f : V => T => U) : CachedParamAttribute[V,T,U] =
        new CachedParamAttribute (name, f)

    /**
     * Define an attribute of `T` nodes of type `U` by the function `f`, which
     * takes the current node and its parent as its arguments. `T` must be
     * a sub-type of `Attributable` so that parents can be accessed generically.
     */
    def childAttr[T <: Attributable,U] (f : T => Attributable => U) : CachedAttribute[T,U] =
        macro AttributionMacros.childAttrMacro[T,U,CachedAttribute[T,U]]

    /**
     * As for the other `childAttr` with the first argument specifying a name for
     * the constructed attribute.
     */
    def childAttr[T <: Attributable,U] (name : String, f : T => Attributable => U) : CachedAttribute[T,U] =
        attr (name, (t : T) => f (t) (t.parent))

    /**
     * Define an optionally named attribute as per `attr`, except that the
     * attribute must have a tree value and will be spliced into the tree to
     * have the same parent as the node on which it is defined.  This kind of
     * attribute is used to generate new trees that must share context
     * with the node on which they are defined.
     */
    def tree[T <: Attributable,U <: Attributable] (f : T => U) : CachedAttribute[T,U] =
        macro AttributionMacros.treeMacro[T,U,CachedAttribute[T,U]]

    /**
     * As for the other `tree` with the first argument specifying a name for
     * the constructed attribute.
     */
    def tree[T <: Attributable,U <: Attributable] (name : String, f : T => U) : CachedAttribute[T,U] =
        attr (name, (t : T) => {
                        val u = f (t)
                        u.parent = t.parent
                        u
                    })

    /**
     * Implicitly converts functions to dynamic attributes. This conversion allows us
     * to use simpler types for dynamic attributes, but still extend them later.
     */
    implicit def internalToDynamicAttribute[T <: AnyRef,U] (f : T => U) : CachedDynamicAttribute[T,U] =
        f match {
            case f : CachedDynamicAttribute[_,_] =>
                f.asInstanceOf[CachedDynamicAttribute[T,U]]
            case f =>
                throw new UnsupportedOperationException ("Can only extend the definition of dynamic attributes")
        }

    /**
     * Define an optionally named circular attribute of `T` nodes of type `U`
     * by the function `f`. `f` is allowed to depend on the value of this
     * attribute, which will be given by `init` initially and will be evaluated
     * iteratively until a fixed point is reached (in conjunction with other
     * circular attributes on which it depends).  The final value is cached.
     */
    def circular[T <: AnyRef,U] (init : U) (f : T => U) : T => U =
        macro AttributionCommonMacros.circularMacro[T,U]

    /**
     * As for the other `circular` with the first argument specifying a name for
     * the constructed attribute.
     */
    def circular[T <: AnyRef,U] (name : String, init : U) (f : T => U) : T => U =
        new CircularAttribute (name, init, f)

}

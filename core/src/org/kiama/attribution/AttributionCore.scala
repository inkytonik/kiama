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
 * with attribute values cached so that each value is computed at most once.
 */
trait AttributionCore extends AttributionCommon {

    import org.bitbucket.inkytonik.dsprofile.Events.{finish, start}
    import scala.language.experimental.macros
    import scala.language.implicitConversions

    /**
     * Global state for the memoisation tables.
     */
    private object MemoState {
        var MEMO_VERSION = 0
    }

    /**
     * Lazily reset all memoisation tables.
     */
    def resetMemo () : Unit =
        MemoState.MEMO_VERSION += 1

    /**
     * An attribute of a node type `T` with value of type `U`, supported by a memo
     * table and circularity test.  The value of the attribute is computed by
     * the function `f`.  The result is memoised so that it is only evaluated once.
     * `f` should not itself require the value of this attribute. If it does, a
     * circularity error is reported by throwing an `IllegalStateException`.
     */
    class CachedAttribute[T <: AnyRef,U] (name : String, f : T => U) extends Attribute[T,U] (name) {

        import java.util.IdentityHashMap

        /**
         * The memo table for this attribute, with `memo(t) == Some(v)` represents
         * the node `t` having the value `v` for this attribute.  `memo(t) = None`
         * means that the attribute for `t` is currently being evaluated.  Note that
         * the memo table needs to be some form of identity map so that value equal
         * trees are not treated as equal unless they are actually the same reference.
         */
        protected val memo = new IdentityHashMap[T,Option[U]]

        /**
         * The current version number of the memo table.
         */
        protected var memoVersion = MemoState.MEMO_VERSION

        /**
         * Check to see if a reset has been requested, and if so, do it.
         */
        private def resetIfRequested () {
            if (memoVersion != MemoState.MEMO_VERSION) {
                memoVersion = MemoState.MEMO_VERSION
                reset ()
            }
        }

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
         * Immediately reset this attribute's memoisation cache.
         */
        def reset () {
            memo.clear ()
        }

        /**
         * Has the value of this attribute at `t` already been computed or not?
         */
        def hasBeenComputedAt (t : T) : Boolean = {
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
    class CachedParamAttribute[A,T <: AnyRef,U] (name : String, f : A => T => U) extends (A => Attribute[T,U]) {

        attr =>

        import scala.collection.mutable.HashMap

        /**
         * Memoisation table for pairs of parameters and nodes to attribute
         * values.
         */
        private val memo = new HashMap[ParamAttributeKey,Option[U]]

        /**
         * The current version of the memoised data.
         */
        private var memoVersion = MemoState.MEMO_VERSION

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
                    if (memoVersion != MemoState.MEMO_VERSION) {
                        memoVersion = MemoState.MEMO_VERSION
                        memo.clear
                    }
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
         * Immediately reset this attribute's memoisation cache.
         */
        def reset () {
            memo.clear ()
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
     */
    class CachedDynamicAttribute[T <: AnyRef,U] (name : String, f : T => U) extends CachedAttribute[T,U] (name, f) {

        import scala.collection.mutable.ListBuffer

        /**
         * List of functions that currently dynamically define this attribute.
         */
        val functions = new ListBuffer[T ==> U] ()

        /**
         * Return the value of this attribute for node `t`, raising an error if
         * it depends on itself.  IF the value has been memoised, return it.
         * Otherwise, the functions list is tried in order. The first partial
         * function on that list that is defined will be used. If no partial
         * function on this list is defined, then `f` will be used.
         */
        override def apply (t : T) : U = {
            if (memoVersion != MemoState.MEMO_VERSION) {
                memoVersion = MemoState.MEMO_VERSION
                memo.clear
            }
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
            // resetMemo
        }

        /**
         * Remove a partial function from the definition of this attribute, if it's
         * there, otherwise have no effect. If `g` appears more than once in the
         * definition, just remove the first one. Resets the memo table for this
         * attribute.
         */
        def -= (g : T ==> U) {
            functions -= g
            // resetMemo
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
     * Define a cached attribute of `T` nodes of type `U` by the function `f`,
     * which should not depend on the value of this attribute.  The computed
     * attribute value is cached so it will be computed at most once. If
     * `optNameDef` is not `None`, then `optNameDef.get` is used in debugging
     * output to identify this attribute.
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
     * If `optNameDef` is not `None`, then `optNameDef.get` is used in debugging
     * output to identify this attribute.
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
     * once.  If `optNameDef` is not `None`, then `optNameDef.get` and the `A`
     * value are used in debugging output to identify this attribute.
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
     * If `optNameDef` is not `None`, then `optNameDef.get` is used in debugging
     * output to identify this attribute.
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
     * with the node on which they are defined. If `optNameDef` is not `None`,
     * then `optNameDef.get` is used in debugging output to identify this
     * attribute.
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

}

/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2012 Anthony M Sloane, Macquarie University.
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

import java.util.IdentityHashMap

/**
 * Base support for attribution of syntax trees in a functional style.
 * Includes circular attributes but needs to be augmented with basic attributes
 * and parameterised attributes.
 */
trait AttributionBase {

    /**
     * Common functionality for all attributes.
     */
    abstract class Attribute[T <: AnyRef,U] extends (T => U) {

        /**
         * An optional name, used in debugging output if present.
         */
        def optName : Option[String]

        /**
         * Report a cycle in the calculation of this attribute discovered when
         * evaluating the attribute on value `t`. Throws an `IllegalStateException`.
         */
        def reportCycle (t : T) : U = {
            val error = "Cycle detected in attribute evaluation"
            val identity = optName.map (" '" + _ + "'").getOrElse ("")
            val message = "%s%s at %s".format (error, identity, t)
            throw new IllegalStateException (message)
        }

    }

    /**
     * Global state for the circular attribute evaluation algorithm
     * and the memoisation tables.
     */
    private object CircularState {
        var IN_CIRCLE = false
        var CHANGE = false
    }

    /**
     * An attribute of a node type `T` with value of type `U` which has a circular
     * definition.  The value of the attribute is computed by the function f
     * which may itself use the value of the attribute.  init specifies an
     * initial value for the attribute.  The attribute (and any circular attributes
     * on which it depends) are evaluated until no value changes (i.e., a fixed
     * point is reached).  The final result is memoised so that subsequent evaluations
     * return the same value.  If `optName` is not `None`, then `optName.get` is
     * used in debugging output to identify this attribute.

     *
     * This code implements the basic circular evaluation algorithm from "Circular
     * Reference Attributed Grammars - their Evaluation and Applications", by Magnusson
     * and Hedin from LDTA 2003.
     */
    class CircularAttribute[T <: AnyRef,U] (val optName : Option[String],
                                            init : U, f : T => U) extends Attribute[T,U] {

        /**
         * Has the value of this attribute for a given tree already been computed?
         */
        private val computed = new IdentityHashMap[T,Unit]

        /**
         * Has the attribute for given tree been computed on this iteration of the
         * circular evaluation?
         */
        private val visited = new IdentityHashMap[T,Unit]

        /**
         * The memo table for this attribute.
         */
        private val memo = new IdentityHashMap[T,U]

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
         * from the CRAG paper.
         */
        def apply (t : T) : U = {
            if (computed containsKey t) {
                value (t)
            } else if (!CircularState.IN_CIRCLE) {
                CircularState.IN_CIRCLE = true
                visited.put (t, ())
                var u = init
                do {
                    CircularState.CHANGE = false
                    val newu = f (t)
                    if (u != newu) {
                        CircularState.CHANGE = true
                        u = newu
                    }
                } while (CircularState.CHANGE)
                visited.remove (t)
                computed.put (t, ())
                memo.put (t, u)
                CircularState.IN_CIRCLE = false
                u
            } else if (! (visited containsKey t)) {
                visited.put (t, ())
                var u = value (t)
                val newu = f (t)
                if (u != newu) {
                    CircularState.CHANGE = true
                    u = newu
                    memo.put (t, u)
                }
                visited.remove (t)
                u
            } else
                value (t)
        }

    }

    /**
     * Support for parameterised attributes: argument, node pair comparison.
     */
    class ParamAttributeKey (val arg : Any, val node : AnyRef) {
        override def equals(o : Any) : Boolean =
            o match {
                case o : ParamAttributeKey =>
                  arg == o.arg &&                                        // object equality
                  (if (node eq null) o.node eq null else node eq o.node) // reference equality
                case _ => false
            }

        override def hashCode : Int =
            System.identityHashCode(node) ^ arg.hashCode
    }

    /**
     * A constant attribute of a node type `T` with value of type `U`. The
     * value is given by the computation `u` which is evaluated at most once.
     * If `optName` is not `None`, then `optName.get` is used in debugging output
     * to identify this attribute.
     */
    class ConstantAttribute[T <: AnyRef,U] (val optName : Option[String],
                                            u : => U) extends Attribute[T,U] {

        /**
         * Lazily computed result of evaluating the attribute's computation.
         */
        private lazy val result = u

        /**
         * Return the value of this attribute for node `t`, always returning
         * `u` but only evaluating it once.
         */
        def apply (t : T) : U =
            result

    }

    /**
     * Define an optionally named circular attribute of `T` nodes of type `U`
     * by the function `f`. `f` is allowed to depend on the value of this
     * attribute, which will be given by `init` initially and will be evaluated
     * iteratively until a fixed point is reached (in conjunction with other
     * circular attributes on which it depends).  The final value is cached.
     * If `optName` is not `None`, then `optName.get` is used in debugging
     * output to identify this attribute.
     */
    def circular[T <: AnyRef,U] (optName : Option[String]) (init : U) (f : T => U) : T => U =
        new CircularAttribute (optName, init, f)

    /**
     * Define an anonymous circular attribute of `T` nodes of type `U` by the
     * function `f`. `f` is allowed to depend on the value of this attribute,
     * which will be given by `init` initially and will be evaluated iteratively
     * until a fixed point is reached (in conjunction with other circular
     * attributes on which it depends).  The final value is cached.
     */
    def circular[T <: AnyRef,U] (init : U) (f : T => U) : T => U =
        circular (None) (init) (f)

    /**
     * Define a named circular attribute of `T` nodes of type `U` by the function
     * `f`. `f` is allowed to depend on the value of this attribute, which will
     * be given by `init` initially and will be evaluated iteratively until a
     * fixed point is reached (in conjunction with other circular attributes
     * on which it depends).  The final value is cached.  `name` is used in
     * debugging output to identify this attribute.
     */
    def circular[T <: AnyRef,U] (name : String) (init : U) (f : T => U) : T => U =
        circular (Some (name)) (init) (f)

    /**
     * Define an optionally named constanat attribute of `T` nodes of type `U`
     * given by the value `u`. `u` is evaluated at most once. If `optName` is
     * not `None`, then `optName.get` is used in debugging output to identify
     * this attribute.
     */
    def constant[T <: AnyRef,U] (optName : Option[String]) (u : => U) : Attribute[T,U] =
        new ConstantAttribute (optName, u)

    /**
     * Define an anonymous constanat attribute of `T` nodes of type `U`
     * given by the value `u`. `u` is evaluated at most once.
     */
    def constant[T <: AnyRef,U] (u : => U) : Attribute[T,U] =
        constant (None) (u)

    /**
     * Define an optionally named constanat attribute of `T` nodes of type `U`
     * given by the value `u`. `u` is evaluated at most once.  `name` is used
     * in debugging output to identify this attribute.
     */
    def constant[T <: AnyRef,U] (name : String) (u : => U) : Attribute[T,U] =
        constant (Some (name)) (u)

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

/**
 * Reusable implementation of attribution of syntax trees in a functional style
 * with attribute values cached so that each value is computed at most once.
 */
trait Attribution extends AttributionBase {

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
     * If `optName` is not `None`, then `optName.get` is used in debugging output
     * to identify this attribute.
     */
    class CachedAttribute[T <: AnyRef,U] (val optName : Option[String],
                                          f : T => U) extends Attribute[T,U] {

        /**
         * The memo table for this attribute, with `memo(t) == Some(v)` represents
         * the node `t` having the value `v` for this attribute.  `memo(t) = None`
         * means that the attribute for `t` is currently being evaluated.  Note that
         * the memo table needs to be some form of identity map so that value equal
         * trees are not treated as equal unless they are actually the same reference.
         */
        private val memo = new IdentityHashMap[T,Option[U]]

        /**
         * The current version number of the memo table.
         */
        private var memoVersion = MemoState.MEMO_VERSION

        /**
         * Return the value of this attribute for node `t`, raising an error if
         * it depends on itself.
         */
        def apply (t : T) : U = {
            if (memoVersion != MemoState.MEMO_VERSION) {
                memoVersion = MemoState.MEMO_VERSION
                memo.clear
            }
            memo.get (t) match {
                case None     => reportCycle (t)
                case Some (u) => u
                case _        => // null
                                 memo.put (t, None)
                                 val u = f (t)
                                 memo.put (t, Some (u))
                                 u
            }
        }

        /**
         * Immediately reset this attribute's memoisation cache.
         */
        def reset () {
            memo.clear ()
        }

    }

    /**
     * A variation of the `CachedAttribute` class for parameterised attributes.
     */
    class CachedParamAttribute[A,T <: AnyRef,U] (optName : Option[String],
                                                 f : A => T => U)
            extends (A => Attribute[T,U]) {

        attr =>

        import scala.collection.mutable.HashMap

        private val memo = new HashMap[ParamAttributeKey,Option[U]]
        private var memoVersion = MemoState.MEMO_VERSION

        /**
         * Return the value of this attribute for node `t`, raising an error if
         * it depends on itself.
         */
        def apply (arg : A) : Attribute[T,U] =
            new Attribute[T,U] {

                override val optName =
                    attr.optName.map (_ + " (" + arg + ")")

                def apply (t : T) : U = {
                    if (memoVersion != MemoState.MEMO_VERSION) {
                        memoVersion = MemoState.MEMO_VERSION
                        memo.clear
                    }
                    val key = new ParamAttributeKey (arg, t)
                    memo.get (key) match {
                        case Some (None)     => reportCycle (t)
                        case Some (Some (u)) => u
                        case None            => // null
                                                memo.put (key, None)
                                                val u = f (arg) (t)
                                                memo.put (key, Some (u))
                                                u
                    }
                }

            }

        /**
         * Immediately reset this attribute's memoisation cache.
         */
        def reset () {
            memo.clear ()
        }

    }

    /**
     * Define a cached attribute of `T` nodes of type `U` by the function `f`,
     * which should not depend on the value of this attribute.  The computed
     * attribute value is cached so it will be computed at most once. If
     * `optName` is not `None`, then `optName.get` is used in debugging output
     * to identify this attribute.
     */
    def attr[T <: AnyRef,U] (optName : Option[String]) (f : T => U) : CachedAttribute[T,U] =
        new CachedAttribute (optName, f)

    /**
     * Define an anonymous cached attribute of `T` nodes of type `U` by the
     * function `f`, which should not depend on the value of this attribute.
     * The computed attribute value is cached so it will be computed at most
     * once.
     */
    def attr[T <: AnyRef,U] (f : T => U) : CachedAttribute[T,U] =
        attr (None) (f)

    /**
     * Define a named cached attribute of `T` nodes of type `U` by the function
     * `f`, which should not depend on the value of this attribute.  The computed
     * attribute value is cached so it will be computed at most once. `name`
     * is used in debugging output to identify this attribute.
     */
    def attr[T <: AnyRef,U] (name : String) (f : T => U) : CachedAttribute[T,U] =
        attr (Some (name)) (f)

    /**
     * Define a parameterised attribute of `T` nodes of type `U` by the function
     * `f`, which takes an argument of type `A`.  The computed attribute value
     * for a given `T` and `A` pair is cached so it will be computed at most
     * once.  If `optName` is not `None`, then `optName.get` and the `A` value
     * are used in debugging output to identify this attribute.
     */
    def paramAttr[A,T <: AnyRef,U] (optName : Option[String]) (f : A => T => U) : CachedParamAttribute[A,T,U] =
        new CachedParamAttribute (optName, f)

    /**
     * Define an anonymous parameterised attribute of `T` nodes of type `U` by the
     * function `f`, which takes an argument of type `A`.  The computed attribute
     * value for a given `T` and `A` pair is cached so it will be computed at most
     * once.
     */
    def paramAttr[A,T <: AnyRef,U] (f : A => T => U) : CachedParamAttribute[A,T,U] =
        paramAttr (None) (f)

    /**
     * Define a named parameterised attribute of `T` nodes of type `U` by the
     * function `f`, which takes an argument of type `A`.  The computed attribute
     * value for a given `T` and `A` pair is cached so it will be computed at most
     * once.  `name` and the `A` value are used in debugging output to identify
     * this attribute and its parameter.
     */
    def paramAttr[A,T <: AnyRef,U] (name : String) (f : A => T => U) : CachedParamAttribute[A,T,U] =
        paramAttr (Some (name)) (f)

    /**
     * Define an attribute of `T` nodes of type `U` by the function `f`, which
     * takes the current node and its parent as its arguments. `T` must be
     * a sub-type of `Attributable` so that parents can be accessed generically.
     * If `optName` is not `None`, then `optName.get` is used in debugging output
     * to identify this attribute.
     */
    def childAttr[T <: Attributable,U] (optName : Option[String]) (f : T => Attributable => U) : CachedAttribute[T,U] =
        attr (optName) ((t : T) => f (t) (t.parent))

    /**
     * Define an anonymous attribute of `T` nodes of type `U` by the function
     * `f`, which takes the current node and its parent as its arguments. `T`
     * must be a sub-type of `Attributable` so that parents can be accessed
     * generically.
     */
    def childAttr[T <: Attributable,U] (f : T => Attributable => U) : CachedAttribute[T,U] =
        childAttr (None) (f)

    /**
     * Define a named attribute of `T` nodes of type `U` by the function `f`,
     * which takes the current node and its parent as its arguments. `T` must be
     * a sub-type of `Attributable` so that parents can be accessed generically.
     * `name` is used in debugging output to identify this attribute and its parameter.
     */
    def childAttr[T <: Attributable,U] (name : String) (f : T => Attributable => U) : CachedAttribute[T,U] =
        childAttr (Some (name)) (f)

    /**
     * Define an optionally named attribute as per `attr`, except that the
     * attribute must have a tree value and will be spliced into the tree to
     * have the same parent as the node on which it is defined.  This kind of
     * attribute is used to generate new trees that must share context
     * with the node on which they are defined. If `optName` is not `None`,
     * then `optName.get` is used in debugging output to identify this attribute.
     */
    def tree[T <: Attributable,U <: Attributable] (optName : Option[String]) (f : T => U) : CachedAttribute[T,U] =
        attr (optName) ((t : T) => {
                            val u = f (t)
                            u.parent = t.parent
                            u
                        })

    /**
     * Define an anonymous attribute as per `attr`, except that the attribute
     * must have a tree value and will be spliced into the tree to have the
     * same parent as the node on which it is defined.  This kind of attribute
     * is used to generate new trees that must share context with the node on
     * which they are defined.
     */
    def tree[T <: Attributable,U <: Attributable] (f : T => U) : CachedAttribute[T,U] =
        tree (None) (f)

    /**
     * Define a named attribute as per `attr`, except that the attribute
     * must have a tree value and will be spliced into the tree to have the
     * same parent as the node on which it is defined.  This kind of attribute
     * is used to generate new trees that must share context with the node on
     * which they are defined. `name` is used in debugging output to identify
     * this attribute and its parameter.
     */
    def tree[T <: Attributable,U <: Attributable] (name : String) (f : T => U) : CachedAttribute[T,U] =
        tree (Some (name)) (f)

}

/**
 * Module for cached attributes.
 */
object Attribution extends Attribution

/**
 * Reusable implementation of attribution of syntax trees in a functional style
 * with attribute values computed each time they are accessed.
 */
trait UncachedAttribution extends AttributionBase {

    /**
     * An attribute of a node type `T` with value of type `U`, supported by a circularity
     * test.  The value of the attribute is computed by the function `f`.  `f` will be
     * called each time the value of the attribute is accessed.  `f` should not itself
     * require the value of this attribute. If it does, a circularity error is reported
     * by throwing an `IllegalStateException`. If `optName` is not `None`, then
     * `optName.get` is used in debugging output to identify this attribute.
     */
    class UncachedAttribute[T <: AnyRef,U] (val optName : Option[String],
                                            f : T => U) extends Attribute[T,U] {

        /**
         * Are we currently evaluating this attribute for a given tree?
         */
        private val visited = new IdentityHashMap[T,Unit]

        /**
         * Return the value of this attribute for node `t`, raising an error if
         * it depends on itself.
         */
        def apply (t : T) : U = {
            if (visited containsKey t)
                reportCycle (t)
            else {
                visited.put (t, ())
                val u = f (t)
                visited.remove (t)
                u
            }
        }

    }

    /**
     * A variation of the `UncachedAttribute` class for parameterised attributes.
     */
    class UncachedParamAttribute[A,T <: AnyRef,U] (optName : Option[String],
                                                   f : A => T => U) extends (A => Attribute[T,U]) {

        attr =>

        /**
         * Are we currently evaluating this attribute for a given argument and tree?
         */
        private val visited = new IdentityHashMap[ParamAttributeKey,Unit]

        /**
         * Return the value of this attribute for node `t`, raising an error if
         * it depends on itself.
         */
        def apply (arg : A) : Attribute[T,U] =
            new Attribute[T,U] {

                override val optName =
                    attr.optName.map (_ + " (" + arg + ")")

                def apply (t : T) : U = {
                    val key = new ParamAttributeKey (arg, t)
                    if (visited containsKey key) {

                        throw new IllegalStateException ("Cycle detected in attribute evaluation")
                    } else {
                        visited.put (key, ())
                        val u = f (arg) (t)
                        visited.remove (key)
                        u
                    }
                }

            }
    }

    /**
     * Define an uncached attribute of `T` nodes of type `U` by the function `f`,
     * which should not depend on the value of this attribute.  The computed
     * attribute value is cached so it will be computed at most once. If
     * `optName` is not `None`, then `optName.get` is used in debugging output
     * to identify this attribute.
     */
    def attr[T <: AnyRef,U] (optName : Option[String]) (f : T => U) : UncachedAttribute[T,U] =
        new UncachedAttribute (optName, f)

    /**
     * Define an anonymous uncached attribute of `T` nodes of type `U` by the
     * function `f`, which should not depend on the value of this attribute.
     * The computed attribute value is cached so it will be computed at most
     * once.
     */
    def attr[T <: AnyRef,U] (f : T => U) : UncachedAttribute[T,U] =
        attr (None) (f)

    /**
     * Define a named uncached attribute of `T` nodes of type `U` by the function
     * `f`, which should not depend on the value of this attribute.  The computed
     * attribute value is cached so it will be computed at most once. `name`
     * is used in debugging output to identify this attribute.
     */
    def attr[T <: AnyRef,U] (name : String) (f : T => U) : UncachedAttribute[T,U] =
        attr (Some (name)) (f)

    /**
     * Define a parameterised uncached attribute of `T` nodes of type `U` by the function
     * `f`, which takes an argument of type `A`.  The computed attribute value
     * for a given `T` and `A` pair is cached so it will be computed at most
     * once.  If `optName` is not `None`, then `optName.get` and the `A` value
     * are used in debugging output to identify this attribute.
     */
    def paramAttr[A,T <: AnyRef,U] (optName : Option[String]) (f : A => T => U) : UncachedParamAttribute[A,T,U] =
        new UncachedParamAttribute (optName, f)

    /**
     * Define an anonymous parameterised uncached attribute of `T` nodes of type `U` by the
     * function `f`, which takes an argument of type `A`.  The computed attribute
     * value for a given `T` and `A` pair is cached so it will be computed at most
     * once.
     */
    def paramAttr[A,T <: AnyRef,U] (f : A => T => U) : UncachedParamAttribute[A,T,U] =
        paramAttr (None) (f)

    /**
     * Define a named parameterised uncached attribute of `T` nodes of type `U` by the
     * function `f`, which takes an argument of type `A`.  The computed attribute
     * value for a given `T` and `A` pair is cached so it will be computed at most
     * once.  `name` and the `A` value are used in debugging output to identify
     * this attribute and its parameter.
     */
    def paramAttr[A,T <: AnyRef,U] (name : String) (f : A => T => U) : UncachedParamAttribute[A,T,U] =
        paramAttr (Some (name)) (f)

    /**
     * Define an uncached attribute of `T` nodes of type `U` by the function `f`, which
     * takes the current node and its parent as its arguments. `T` must be
     * a sub-type of `Attributable` so that parents can be accessed generically.
     * If `optName` is not `None`, then `optName.get` is used in debugging output
     * to identify this attribute.
     */
    def childAttr[T <: Attributable,U] (optName : Option[String]) (f : T => Attributable => U) : UncachedAttribute[T,U] =
        attr (optName) ((t : T) => f (t) (t.parent))

    /**
     * Define an anonymous uncached attribute of `T` nodes of type `U` by the function
     * `f`, which takes the current node and its parent as its arguments. `T`
     * must be a sub-type of `Attributable` so that parents can be accessed
     * generically.
     */
    def childAttr[T <: Attributable,U] (f : T => Attributable => U) : UncachedAttribute[T,U] =
        childAttr (None) (f)

    /**
     * Define a named uncached attribute of `T` nodes of type `U` by the function `f`,
     * which takes the current node and its parent as its arguments. `T` must be
     * a sub-type of `Attributable` so that parents can be accessed generically.
     * `name` is used in debugging output to identify this attribute and its parameter.
     */
    def childAttr[T <: Attributable,U] (name : String) (f : T => Attributable => U) : UncachedAttribute[T,U] =
        childAttr (Some (name)) (f)

}

/**
 * Module for uncached attributes.
 */
object UncachedAttribution extends UncachedAttribution

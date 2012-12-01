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

/**
 * Reusable implementation of dynamic attributes where definitions can be
 * extended at runtime by the addition of new equations, which can later
 * be removed.
 *
 * @author Lennart Kats <lennart add lclnet.nl>
 * @author Tony Sloane <Anthony.Sloane add mq.edu.au>
 */
trait DynamicAttribution extends AttributionBase {

    import java.util.IdentityHashMap
    import scala.collection.mutable.{ArrayBuffer, ListBuffer}

    type ChangeBuffer = ArrayBuffer[(DynamicAttribute[_, _], _ ==> _)]

    private var currentRecordedChanges : ChangeBuffer = null
    private val allRecordedChanges = new IdentityHashMap[AnyRef, ChangeBuffer]
    private var equationsVersion = 0

    /**
     * Lazily resets all memoisation tables.
     */
    def resetMemo () : Unit =
        equationsVersion += 1

    /**
     * Define a dynamic attribute of `T` nodes of type `U` by the function `f`.
     */
    def attr[T <: AnyRef,U] (f : T ==> U) : T ==> U =
        new DynamicAttribute (f)

    /**
     * Define an attribute of `T` nodes of type `U` by the function `f`,
     * which takes the current node and its parent as its arguments.
     * `T` must be `Attributable` so that parents can be accessed.
     */
    def childAttr[T <: Attributable,U] (f : T => Attributable ==> U) : T ==> U = {
        val childF = new (T ==> U) {
            def apply (t : T) : U = f (t) (t.parent)
            def isDefinedAt (t : T) : Boolean = f (t) isDefinedAt t.parent
        }
        attr (childF)
    }

    /**
     * Implicitly converts partial functions to support the `+` operator.
     **/
    implicit def internalToDynamicAttribute[T <: AnyRef,U] (f : Function[T,U]) : DynamicAttribute[T,U] =
        f match {
            case f : DynamicAttribute[_, _] => f.asInstanceOf[DynamicAttribute[T,U]]
            case f => throw new UnsupportedOperationException("Can only add partial functions to existing attributes")
        }

    /**
     * Defines a new scope in which a dynamic attribution module is active.
     * At the end of the scope, the module is unloaded again.
     *
     * @param attributeInitializer  A module defining dynamic attributes.
     * @param block                 A block to evaluate.
     */
    def using[T] (attributeInitializer : => AnyRef) (block : => T) {
        try {
            use (attributeInitializer)
            block
        } finally {
            endUse (attributeInitializer)
        }
    }

    /**
     * Activates a module that defines dynamic attributes,
     * allowing it to be deactivated again using `endUse`.
     */
    def use[T] (attributeInitializer : => AnyRef) {
        val prevRecordedChanges = currentRecordedChanges
        try {
            currentRecordedChanges = new ArrayBuffer
            val initialized = attributeInitializer // import initializer
            allRecordedChanges.put (initialized, currentRecordedChanges)
            currentRecordedChanges = null
        } finally {
           currentRecordedChanges = prevRecordedChanges
        }
    }

    // private def reuse (attributeInitializer : AnyRef) {
    //     val changes = allRecordedChanges.get (attributeInitializer)
    //     if (changes != null)
    //         for ((attr, function) <- changes)
    //             attr += function
    // }

    /**
     * Deactivates a module that defines dynamic attributes,
     * activated using `use`.
     */
    def endUse (attributeInitializer : AnyRef) {
        val changes = allRecordedChanges.get (attributeInitializer)
        if (changes != null)
            for ((attr, function) <- changes)
                attr -= function
    }

    /**
     * A dynamic atribute defined initially by the function `f`.
     */
    class DynamicAttribute[T,U] (private var f : T ==> U) extends (T ==> U) {
        private val memo = new IdentityHashMap[T, Option[U]]
        private var memoVersion = equationsVersion

        /**
         * Obtain the attribute value.
         */
        def apply (t : T) : U = {
            if (memoVersion != equationsVersion) {
                memoVersion = equationsVersion
                memo.clear
            }

            memo.get (t) match {
                 case None =>
                     throw new IllegalStateException ("Cycle detected in attribute evaluation at " + t)
                 case Some (u) => u
                 case _ => // null
                    memo.put (t, None)
                    val result = f (t)
                    memo.put (t, Some (result))
                    result
            }
        }

        /**
         * Is the attribute defined at the node `t`?
         */
        def isDefinedAt (t : T) : Boolean =
            composedF isDefinedAt t

        /**
         * Return the function defining this attribute as `ComposedPartialFunction`.
         */
        protected def composedF : ComposedPartialFunction[T,U] =
            f match {
                case _ : ComposedPartialFunction[_,_] => f.asInstanceOf[ComposedPartialFunction[T,U]]
                case _ : ==>[_,_]                     => val g = new ComposedPartialFunction(f); f = g; g
            }

        /**
         * Add the equations defined by the function `g` to the definition of this
         * attribute after the existing definitions.
         */
        def += (g : T ==> U) {
            val uncached : T ==> U =
                g match {
                    case g : DynamicAttribute[_, _] => g.f
                    case _                          => g
                }

            if (currentRecordedChanges != null) currentRecordedChanges += ((this, uncached))
            composedF += uncached
            resetMemo
        }

        /**
         * Remove the function `g` from the definitions of this attribute.  If
         * `g` was not previously added to the definitions, do nothing.
         */
        def -= (g : T ==> U) {
            val uncached : T ==> U =
                g match {
                    case g : DynamicAttribute[_, _] => g.f
                    case _                          => g
                }

            composedF -= uncached
            resetMemo
        }

        /**
         * Immediately reset this attribute's memoisation cache.
         */
        def reset () {
            memo.clear ()
        }

    }

    /**
     * A partial function composed of an ordered, mutable buffer of
     * PartialFunction instances. `f` is the iniital single occupant
     * of the buffer. When the function is applied its component
     * functions are checked in the reverse order of insertion.
     */
    class ComposedPartialFunction[T,U] (f : T ==> U) extends (T ==> U) {

        // Buffer to hold functions
        val functions = new ListBuffer[T ==> U]

        // Initially, just `f` is in the buffer
        f +=: functions

        /**
         * Is there a function in the buffer that is defined at `t`?
         */
        def isDefinedAt (t : T) : Boolean =
            functions.exists (_ isDefinedAt t)

        /**
         * Apply the buffered functions to the value `t` in the order
         * that they appear in the buffer. Return the value of the first
         * such function that is defined at `t`. If no function is
         * defined at `t`, throw a `MatchError` exception.
         */
        def apply (t : T) : U =
            functions.collectFirst {
                case function if function.isDefinedAt (t) =>
                    function (t)
            }.getOrElse (
                throw new MatchError(t)
            )

        /**
         * Add `g` to the buffer.
         */
        def += (g : T ==> U) {
            g +=: functions
        }

        /**
         * Remove the first occurrence of `g` from the buffer if it is there.
         * Otherwise, do nothing.
         */
        def -= (g : T ==> U) {
            val removed = functions.indexOf (g)
            if (removed != -1)
                functions.remove (removed)
        }

    }

}

/**
 * Module for dynamic attributes.
 */
object DynamicAttribution extends DynamicAttribution

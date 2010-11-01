/**
 * This file is part of Kiama.
 *
 * Copyright (C) 2010 Dominic R B Verity, Macquarie University.
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
package example.iswim.secd

/**
 * Add mutable, heap allocated values to a SECD machine
 */

import org.kiama.util.PrettyPrinter

object HeapOps {

	import SECDBase._

	/**
	 * Extra bytecode instructions for this extension
     * Allocate, access and update values in the heap
     */
    case class Alloc() extends Instruction
    case class Get() extends Instruction
    case class Put() extends Instruction

    /**
     * New type values for this extension
     */
    case object RefTypeValue extends TypeValue

}

/**
 * Trait implementing this SECD machine extension
 */
trait HeapOps extends SECDBase {

    import HeapOps._
    import SECDBase._

 	/**
 	 * Extra value types which come with this extension.
 	 *
     * Reference values
     */
    case class RefValue() extends Value {
        override def hashCode () = super.hashCode
        override def equals(that : Any) = super.equals(that)
        override def toString () = "RefValue@" ++ hashCode.toHexString
    	lazy val content = new State[Value]("heap chunk id @" + hashCode.toHexString) {
    	    override def pretty(p : PrettyPrinter, v : Value) = v.pretty(p)
    	}
    	def getType : TypeValue = RefTypeValue
    }

    /**
     * Extend the partial function to evaluate a single instruction
     * to handle our new instructions.
     */
	override def evalInst : Code ==> Unit = super.evalInst orElse {
		// Instructions for manipulating heap allocated mutable values.
        case Alloc() :: next =>
            val r = RefValue()
            r.content := EmptyValue
            stack := r :: stack
            control := next
        case Get() :: next => (stack : Stack) match {
            case (r @ RefValue()) :: tail =>
                stack := r.content :: tail
                control := next
            case _ :: _ => raiseException(TypeError)
            case _ => raiseException(StackUnderflow)
        }
        case Put() :: next => (stack : Stack) match {
            case v :: (r @ RefValue()) :: tail =>
                r.content := v
                stack := tail
                control := next
            case _ :: _ :: _ => raiseException(TypeError)
            case _ => raiseException(StackUnderflow)
        }
	}
}

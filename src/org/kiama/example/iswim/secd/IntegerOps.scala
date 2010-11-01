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
 * Add integer values and associated operations to a SECD machine
 */

object IntegerOps {

	import SECDBase._

	/**
	 * Extra bytecode instructions for this extension
     * Integer arithmetic operations.
     */
    case class PushInt(n : Int) extends Instruction
    case class Add() extends Instruction
    case class Sub() extends Instruction
    case class Mult() extends Instruction
    case class Div() extends Instruction
    case class Rem() extends Instruction

    /**
     * New type values for this extension
     */
    case object IntTypeValue extends TypeValue

    /**
     * New machine error types
     */
    case object DivisionByZero extends MachineExceptionValue {
         def message : String = "division by zero"
     }
}

/**
 * Trait implementing this SECD machine extension
 */
trait IntegerOps extends SECDBase {

    import IntegerOps._
    import SECDBase._

    /**
     * Extra value types which come with this extension.
     *
     * Integer values
     */
    case class IntValue(n : Int) extends Value {
        override def toString : String = n.toString
        def getType : TypeValue = IntTypeValue
    }

    /**
     * Extend the partial function to evaluate a single instruction
     * to handle our new instructions.
     */
	override def evalInst : Code ==> Unit = super.evalInst orElse {
		// Arithmetic operations on integers.
        case PushInt(n) :: next =>
            stack := IntValue(n) :: stack
            control := next
        case Add() :: next => (stack : Stack) match {
            case IntValue(n) :: IntValue(m) :: tail =>
                stack := IntValue(m + n) :: tail
                control := next
            case _ :: _ :: _ => raiseException(TypeError)
            case _ => raiseException(StackUnderflow)
        }
        case Sub() :: next => (stack : Stack) match {
            case IntValue(n) :: IntValue(m) :: tail =>
                stack := IntValue(m - n) :: tail
                control := next
            case _ :: _ :: _ => raiseException(TypeError)
            case _ => raiseException(StackUnderflow)
        }
        case Mult() :: next => (stack : Stack) match {
            case IntValue(n) :: IntValue(m) :: tail =>
                stack := IntValue(m * n) :: tail
                control := next
            case _ :: _ :: _ => raiseException(TypeError)
            case _ => raiseException(StackUnderflow)
        }
        case Div() :: next => (stack : Stack) match {
            case IntValue(n) :: IntValue(m) :: tail =>
                if (n == 0)
                    raiseException(DivisionByZero)
                else {
                    stack := IntValue(m / n) :: tail
                    control := next
                }
            case _ :: _ :: _ => raiseException(TypeError)
            case _ => raiseException(StackUnderflow)
        }
        case Rem() :: next => (stack : Stack) match {
            case IntValue(n) :: IntValue(m) :: tail =>
                if (n == 0)
                    raiseException(DivisionByZero)
                else {
                    stack := IntValue(m % n) :: tail
                    control := next
                }
            case _ :: _ :: _ => raiseException(TypeError)
            case _ => raiseException(StackUnderflow)
        }
	}
}

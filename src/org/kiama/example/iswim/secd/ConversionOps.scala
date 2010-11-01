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
 * Operations to convert values from one type to another
 */

import scala.util.matching.Regex

object ConversionOps {

	import SECDBase._

	/**
	 * Extra bytecode instructions for this extension
     * Simple conversion operations between types.
     */
    case class ToString() extends Instruction
    case class ToInt() extends Instruction
    case class ToBoolean() extends Instruction

    /**
     * Regular expressions for preprocessing string before conversion
     */
    val processInteger : Regex = "^\\s*(-?[0-9]+)\\s*$".r
    val processBoolean : Regex = "^\\s*(true|false)\\s*$".r

    /**
     * New machine error types
     */
    case object ConversionError extends MachineExceptionValue {
        def message : String = "type conversion error"
    }

}

/**
 * Trait implementing this SECD machine extension
 */
trait ConversionOps extends SECDBase with StringOps
                    with IntegerOps with BooleanOps {

    import ConversionOps._
    import SECDBase._

    /**
     * Extend the partial function to evaluate a single instruction
     * to handle our new instructions.
     */
	override def evalInst : Code ==> Unit = super.evalInst orElse {
        // Convert any value to a string
        case ToString() :: next => (stack : Stack) match {
            case v :: tail =>
                stack := StringValue(v.toString) :: tail
                control := next
            case _ => raiseException(StackUnderflow)
        }
        // Convert strings, booleans and types to integers
        case ToInt() :: next => (stack : Stack) match {
            case IntValue(n) :: tail =>
                control := next
            case TrueValue :: tail =>
                stack := IntValue(1) :: tail
                control := next
            case FalseValue :: tail =>
                stack := IntValue(0) :: tail
                control := next
            case StringValue(s) :: tail =>
                processInteger.findFirstMatchIn(s) match {
                    case Some(m) => try {
                        stack := IntValue(m.group(1).toInt) :: tail
                        control := next
                    } catch {
                        case ex : NumberFormatException =>
                            raiseException(ConversionError)
                    }
                    case None => raiseException(ConversionError)
                }
            case _ :: _ => raiseException(TypeError)
            case _ => raiseException(StackUnderflow)
        }
        // Convert strings and integers to booleans
        case ToBoolean() :: next => (stack : Stack) match {
            case IntValue(n) :: tail =>
                stack := (if (n == 0) FalseValue else TrueValue) :: tail
                control := next
            case (_ : BooleanValue) :: tail =>
                control := next
            case StringValue(s) :: tail =>
                processBoolean.findFirstMatchIn(s) match {
                    case Some(m) =>
                        stack := (if (m.group(1).toBoolean)
                                    TrueValue
                                  else
                                    FalseValue) :: tail
                        control := next
                    case None => raiseException(ConversionError)
                }
            case _ :: _ => raiseException(TypeError)
            case _ => raiseException(StackUnderflow)
        }
	}
}

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
 * Add string values and associated operations to a SECD machine
 */

object RecordOps {

	import SECDBase._

	/**
	 * Extra bytecode instructions for this extension
     * Basic operations on records (tuples).
     */
    case class MkRecord(n : Int) extends Instruction
    case class Fields() extends Instruction
    case class GetField() extends Instruction
    case class UnpackRecord() extends Instruction

    /**
     * New type values for this extension
     */
    case object RecordTypeValue extends TypeValue

    /**
     * New machine error types
     */
    case object FieldOutOfBounds extends MachineExceptionValue {
         def message : String = "reference to field beyond the bounds of the current record"
     }

}

/**
 * Trait implementing this SECD machine extension
 */
trait RecordOps extends SECDBase with IntegerOps {

    import SECDBase._
    import RecordOps._
    import IntegerOps._

    /**
     * Extra value types which come with this extension.
     *
     * Record values
     */
    case class RecordValue(flds : List[Value]) extends Value {
        override def toString : String = flds.reverse.mkString("(",",",")")
        def getType : TypeValue = RecordTypeValue
    }

    /**
     * Extend the partial function to evaluate a single instruction
     * to handle our new instructions.
     */
	override def evalInst : Code ==> Unit = super.evalInst orElse {
		// Make a new record from entries on the stack
        case MkRecord(n) :: next =>
            if (n < 0)
                raiseException(MalformedInstruction)
            else if (stack.length < n)
                raiseException(StackUnderflow)
            else
                stack.splitAt(n) match {
                    case (flds, tail) =>
                        stack := RecordValue(flds) :: tail
                        control := next
                }
        // Return the number of fields in a record.
        case Fields() :: next => (stack : Stack) match {
            case RecordValue(flds) :: tail =>
                stack := IntValue(flds.length) :: tail
                control := next
            case _ :: _ => raiseException(TypeError)
            case _ => raiseException(StackUnderflow)
        }
        // Field access
        case GetField() :: next => (stack : Stack) match {
            case IntValue(n) :: RecordValue(flds) :: tail =>
                if (n < 0 || n >= flds.length)
                    raiseException(FieldOutOfBounds)
                else {
                    stack := flds(flds.length - n) :: tail
                    control := next
                }
            case _ :: _ :: _ => raiseException(TypeError)
            case _ => raiseException(StackUnderflow)
        }
        // Unpack a record onto the stack
        case UnpackRecord() :: next => (stack : Stack) match {
            case RecordValue(flds) :: tail =>
                stack := flds ++ tail
                control := next
            case _ :: _ => raiseException(TypeError)
            case _ => raiseException(StackUnderflow)
        }
	}
}

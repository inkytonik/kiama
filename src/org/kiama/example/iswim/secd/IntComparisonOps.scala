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
 * Add stack manipulation operations to a SECD machine
 */

object IntComparisonOps {

	import SECDBase._

	/**
	 * Extra bytecode instructions for this extension
     * Compare two integers.
     */
    case class LessThan() extends Instruction
    case class LessThanOrEqual() extends Instruction
}

/**
 * Trait implementing this SECD machine extension
 */
trait IntComparisonOps extends SECDBase with IntegerOps with BooleanOps {

    import IntComparisonOps._
    import SECDBase._

    /**
     * Extend the partial function to evaluate a single instruction
     * to handle our new instructions.
     */
	override def evalInst : Code ==> Unit = super.evalInst orElse {
        case LessThan() :: next => (stack : Stack) match {
            case IntValue(n) :: IntValue(m) :: tail =>
                stack := (if (m < n) TrueValue else FalseValue) :: tail
                control := next
            case _ :: _ :: _ => raiseException(TypeError)
            case _ => raiseException(StackUnderflow)
        }
        case LessThanOrEqual() :: next => (stack : Stack) match {
            case IntValue(n) :: IntValue(m) :: tail =>
                stack := (if (m <= n) TrueValue else FalseValue) :: tail
                control := next
            case _ :: _ :: _ => raiseException(TypeError)
            case _ => raiseException(StackUnderflow)
        }
	}
}

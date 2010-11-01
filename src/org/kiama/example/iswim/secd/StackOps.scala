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

object StackOps {

	import SECDBase._

	/**
	 * Extra bytecode instructions for this extension
     * Stack manipulation operations like pop and rotate
     */
    case class Pop(n : Int) extends Instruction
    case class Dup(n : Int) extends Instruction
    case class Swap(n : Int, m : Int) extends Instruction
}

/**
 * Trait implementing this SECD machine extension
 */
trait StackOps extends SECDBase {

    import StackOps._
    import SECDBase._

    /**
     * Extend the partial function to evaluate a single instruction
     * to handle our new instructions.
     */
	override def evalInst : Code ==> Unit = super.evalInst orElse {
		// Instructions for manipulating the stack.
		// Pop pop a block of values from the top of the stack and discard.
        case Pop(n) :: next =>
            if (n < 1)
                raiseException(MalformedInstruction)
            else if (stack.length < n)
                raiseException(StackUnderflow)
            else {
                stack := stack.drop(n)
                control := next
            }
        // Duplicate a block of values on the top of the stack.
        case Dup(n) :: next =>
            if (n < 1)
                raiseException(MalformedInstruction)
            else if (stack.length < n)
                raiseException(StackUnderflow)
            else {
                stack := stack.take(n) ++ stack
                control := next
            }
        // swap two blocks of values at the top of the stack
        case Swap(n,m) :: next =>
            if (n < 1 || m < 1)
                raiseException(MalformedInstruction)
            else if (stack.length < n + m)
                raiseException(StackUnderflow)
            else
                stack.splitAt(n) match {
                    case (block1, rest) =>
                        rest.splitAt(m) match {
                            case (block2, tail) =>
                                stack := block2 ++ block1 ++ tail
                                control := next
                        }
                }
	}
}

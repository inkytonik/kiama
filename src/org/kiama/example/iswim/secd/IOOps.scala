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
 * Add terminal input/output operations to a SECD machine
 */

object IOOps {

	import SECDBase._

	/**
	 * Extra bytecode instructions for this extension
     * Simple terminal IO operations
     */
    case class Write() extends Instruction
    case class Read() extends Instruction
}

/**
 * Trait implementing this SECD machine extension
 */
trait IOOps extends SECDBase with StringOps {

    import IOOps._
    import SECDBase._

    /**
     * Extend the partial function to evaluate a single instruction
     * to handle our new instructions.
     */
	override def evalInst : Code ==> Unit = super.evalInst orElse {
        // Write a value to the terminal
        case Write() :: next => (stack : Stack) match {
            case v :: tail =>
                print(v.toString)
                stack := tail
                control := next
            case _ => raiseException(StackUnderflow)
        }
        // Read a string value from the terminal
        case Read() :: next =>
            var line = readLine
            stack := StringValue(line) :: stack
            control := next
	}
}

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
 * Add boolean values and associated operations to a SECD machine
 */

import org.kiama.util.PrettyPrinter

object BooleanOps {

	import SECDBase._

	/**
	 * Extra bytecode instructions for this extension
     * Boolean tests and the equality relation.
     */
    case class PushTrue() extends Instruction
    case class PushFalse() extends Instruction
    case class Test(ct : CodeSegment, ce : CodeSegment) extends Instruction {
        override def pretty(p : PrettyPrinter) = {
            p.text("Test(")
            p.indent {
                p.newline
                ct.pretty(p)
                p.text(",")
                p.newline
                ce.pretty(p)
            }
            p.newline
            p.text(")")
        }
    }
    case class Equals() extends Instruction

    /**
     * New type values for this extension
     */
    case object BooleanTypeValue extends TypeValue

}

/**
 * Trait implementing this SECD machine extension
 */
trait BooleanOps extends SECDBase {

    import BooleanOps._
    import SECDBase._

    /**
     * Extra value types which come with this extension.
     *
     * Boolean values
     */
    abstract class BooleanValue extends Value {
        def getType : TypeValue = BooleanTypeValue
    }
    case object TrueValue extends BooleanValue {
        override def toString : String = "true"
    }
    case object FalseValue extends BooleanValue {
        override def toString : String = "false"
    }

    /**
     * Extend the partial function to evaluate a single instruction
     * to handle our new instructions.
     */
	override def evalInst : Code ==> Unit = super.evalInst orElse {
		// Push constant boolean values on the stack.
        case PushTrue() :: next =>
            stack := TrueValue :: stack
            control := next
        case PushFalse() :: next =>
            stack := FalseValue :: stack
            control := next
        // Conditional branch.
        case Test(CodeSegment(ct), CodeSegment(ce)) :: next =>
            (stack : Stack) match {
                case TrueValue :: tail =>
                    stack := tail
                    control := ct ++ next
                case FalseValue :: tail =>
                    stack := tail
                    control := ce ++ next
                case _ :: _ => raiseException(TypeError)
                case _ => raiseException(StackUnderflow)
            }
        // Equality test on values.
        case Equals() :: next => (stack : Stack) match {
            case val1 :: val2 :: tail =>
                if (val1 == val2)
                    stack := TrueValue :: tail
                else
                    stack := FalseValue :: tail
                control := next
            case _ => raiseException(StackUnderflow)
        }
	}
}

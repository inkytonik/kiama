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

//import org.apache.commons.lang.StringEscapeUtils._

object StringOps {

	import SECDBase._

	/**
	 * Extra bytecode instructions for this extension
     * Basic string operations.
     */
    case class PushString(s : String) extends Instruction

    /**
     * New type values for this extension
     */
    case object StringTypeValue extends TypeValue

}

/**
 * Trait implementing this SECD machine extension
 */
trait StringOps extends SECDBase {

    import SECDBase._
    import StringOps._

    /**
     * Extra value types which come with this extension.
     *
     * String and user exception values
     */
    case class StringValue(s : String) extends Value {
        override def toString : String = s
        def getType : TypeValue = StringTypeValue
    }
    case class UserExceptionValue(m : String) extends ExceptionValue {
        override def toString : String =
            "userExceptionValue@" ++ hashCode.toHexString ++ ": " ++
            m ++ " at " ++ this.pos.toString
    }

    /**
     * Unescape the most obvious escape sequences.  Replaces only \t,
     * \n, \" and \\.  \c where c is some other character is turned into c.
     * A backslash at the end is silently ignored.
     */
    private def unescape(s : String) : String = {
        val b = new StringBuilder
        var escape = false
        for (c <- s) {
            if (escape) {
                c match {
                    case 'n'  => b += '\n'
                    case 't'  => b += '\t'
                    case '"'  => b += '"'
                    case '\\' => b += '\\'
                    case _    => b += c
                }
                escape = false
            } else if (c == '\\') {
                escape = true
            } else {
                b += c
            }
        }
        b.toString
    }

    /**
     * Extend the partial function to evaluate a single instruction
     * to handle our new instructions.
     */
	override def evalInst : Code ==> Unit = super.evalInst orElse {
		// Push a string onto the stack
        case PushString(s) :: next =>
            stack := StringValue(unescape(s)) :: stack
            control := next
    	// Make a new user exception value from string on the top of the stack
    	// and push it on the stack.
		case MkUserException() :: next => (stack : Stack) match {
		    case StringValue(s) :: tail =>
		        stack := UserExceptionValue(s) :: tail
		        control := next
		    case _ :: _ => raiseException(TypeError)
		    case _ => raiseException(StackUnderflow)
		}
	}
}

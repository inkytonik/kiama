/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008 Anthony M Sloane, Macquarie University.
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

/**
 * This file is derived from specifications in "Java and the Java Virtual
 * Machine: Definition, Verification and Validation" by Robert Stärk, Joachim
 * Schmid and Egon Börger, Springer, 2001.
 */
 
package kiama.example.javaasm.jvm

object Primitives {
    
    import util.JList
    
    /**
     * Word representation.
     */
    type Word = Int
    
    /**
     * The type of data to move in a load or store instruction.  size is
     * the number of words used to store a value of this type.
     */
    abstract class MoveType (val size : Int)
    
    /**
     * 32-bit integers.
     */
    case object IntType extends MoveType (1)
    
    /**
     * 64-bit integers.
     */
    case object LongType extends MoveType (2)
    
    /**
     * 32-bit IEEE 754 values.
     */
    case object FloatType extends MoveType (1)
    
    /**
     * 64-bit IEEE 754 values.
     */
    case object DoubleType extends MoveType (2)
    
    /**
     * Return the size in words of a value of the given type.  Provided
     * so that the functional style of Stärk et al can be used.
     */
    def size (t : MoveType) : Int =
        t.size
     
    /**
     * A primitive JVM operation.  argSize is the number of words on the
     * stack taken by this operation's arguments. The apply function 
     * runs the operation on the given arguments (which is assumed to have
     * the correct size) to return some number of word results.
     */
    abstract class PrimOp (val argSize : Int) {
        def apply (ws : JList[Word]) : JList[Word]
    }
    
    /**
     * Return the number of words that comprise the arguments of a primitive
     * operation.  Provided so that the functional style of Stärk et al can
     * be used.
     */
    def argSize (p : PrimOp) : Int =
        p.argSize

    /**
     * Integer addition.
     */ 
    case object iadd extends PrimOp (2) {
        def apply (ws : JList[Word]) = JList (ws (0) + ws (1))
    }
    
    /**
     * Implicit conversion for primitive integers.  i is a primitive
     * that returns the value i.
     */
    implicit def intToPrimOp (i : Int) : PrimOp =
        new PrimOp (0) {
            def apply (ws : JList[Word]) = JList (i)
        }

    /**
     * True if the primitive is a division or remainder operation, false
     * otherwise.
     */
    def isDivMod (p : PrimOp) : Boolean =
        false  // FIXME

    /**
     * Apply a primitive operation to its arguments.
     */
    def JVMS (p : PrimOp, ws : JList[Word]) : JList[Word] =
        p (ws)

    /**
     * A primitive JVM operation that returns a boolean value.  argSize
     * is the number of words on the stack taken by this operation's
     * arguments. The apply function runs the operation on the given
     * arguments (which is assumed to have the correct size) to return
     * a Boolean result.
     */
    abstract class BoolPrimOp (val argSize : Int) {
        def apply (ws : JList[Word]) : Boolean
    }
     
    /**
     * Condition check: not equal to zero.
     */
    case object ifne extends BoolPrimOp (1) {
        def apply (ws : JList[Word]) = ws (0) != 0
    }
    
    /**
     * Return the number of words that comprise the arguments of a Boolean
     * primitive operation.  Provided so that the functional style of Stärk
     * et al can be used.
     */
    def argSize (p : BoolPrimOp) : Int =
        p.argSize
                
    /**
     * Apply a Boolean-value primitive to its arguments, return the Boolean result.
     */
    def JVMSBool (p : BoolPrimOp, ws : JList[Word]) : Boolean =
        p (ws)
    
}


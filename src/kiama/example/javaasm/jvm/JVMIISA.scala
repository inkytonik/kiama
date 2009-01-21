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
 
/**
 * Instruction set archiecture for the JVMI.
 */
object JVMIISA {
    
    import Primitives._
    
    /**
     * Code offset for jumps and branches.
     */
    type Offset = Int
    
    /**
     * Data size for memory operations.
     */
    type Size = Int
    
    /**
     * Register numbers.
     */
    type RegNo = Int
    
    /**
     * A code sequence.
     */
    type Code = Seq[Instr]
    
    /**
     * Machine instructions.
     */
    abstract class Instr
    
    /**
     * Execute a JVM primitive.
     */
    case class Prim (op : PrimOp) extends Instr
    
    /**
     * Load a value of type ty from register reg onto the operand stack.
     */
    case class Load (ty : MoveType, reg : RegNo) extends Instr
    
    /**
     * Store a value of type ty into register reg from the operand stack.
     */
    case class Store (ty : MoveType, reg : RegNo) extends Instr
    
    /**
     * Duplicate the top size2 elements of the operand stack under the 
     * next top size1 elements.  E.g., Dupx(2,1) applied to the stack
     * 1,2,3 yields 1,2,3,1,2 (where the top of stack is to the left).
     */
    case class Dupx (size1 : Size, size2 : Size) extends Instr
    
    /**
     * Remove the top size elements from the operand stack.
     */
    case class Pop (size : Size) extends Instr
    
    /**
     * Set the program counter to the given offset.
     */
    case class Goto (offset : Offset) extends Instr
    
    /**
     * Evaluate op and if it returns true then set the program counter to
     * the given offset, otherwise increment the program counter.
     */
    case class Cond (op : BoolPrimOp, offset : Offset) extends Instr
    
    /**
     * Halt the machine.
     */
    case object Halt extends Instr
    
}

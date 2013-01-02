/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2013 Anthony M Sloane, Macquarie University.
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
package example.RISC

/**
 * Instruction set architecture for a simple RISC machine.
 */
object RISCISA {

    import org.kiama.util.Emitter

    /**
     * A machine code sequence.
     */
    type Code = Seq[Instr]

    /**
     * An assembler code sequence.
     * Includes comments and labels.
     */
    type AssemCode = Seq[Assembler]

    /**
     * Operand types
     */
    type Imm = Short
    type Disp = Int

    /**
     * Register numbers (0-31).  Program counter is R28.
     */
    type RegNo = Int

    def sayReg (reg : RegNo) : String = "$" + reg

    /**
     * Machine instructions.
     */
    sealed abstract class Assembler
    sealed abstract class Instr extends Assembler

    /**
     * Shift the value in register c by b and store the result in register a.
     */
    case class MOV (a : RegNo, b : RegNo, c : RegNo) extends Instr {
        override def toString : String =
                "mov " + sayReg (a) + ", " + sayReg (b) + ", " + sayReg (c)
    }

    /**
     * Shift the value im by b and store the result in register a.
     */
    case class MOVI (a : RegNo, b : RegNo, im : Imm) extends Instr  {
        override def toString : String =
            "movi " + sayReg (a) + ", " + sayReg (b) + ", " + im
    }

    /**
     * Shift the value in register c by b and store the negation of the result
     * in register a.
     */
    case class MVN (a : RegNo, b : RegNo, c : RegNo) extends Instr {
        override def toString : String =
            "mvn " + sayReg (a) + ", " + sayReg (b) + ", " + sayReg (c)
    }

    /**
     * Shift the value im by b and store the negation of the result in
     * register a.
     */
    case class MVNI (a : RegNo, b : RegNo, im : Imm) extends Instr {
        override def toString : String =
            "mvni " + sayReg (a) + ", " + sayReg (b) + ", " + im
    }

    /**
     * Add the values in registers b and c, store the result in register a.
     */
    case class ADD (a : RegNo, b : RegNo, c : RegNo) extends Instr {
        override def toString : String =
            "add " + sayReg (a) + ", " + sayReg (b) + ", " + sayReg (c)
    }

    /**
     * Add the value in register b and the value im, store the result
     * in register a.
     */
    case class ADDI (a : RegNo, b : RegNo, im : Imm) extends Instr {
        override def toString : String =
            "addi " + sayReg (a) + ", " + sayReg (b) + ", " + im
    }

    /**
     * Subtract the values in registers b and c, store the result in
     * register a.
     */
    case class SUB (a : RegNo, b : RegNo, c : RegNo) extends Instr  {
        override def toString : String =
            "sub " + sayReg (a) + ", " + sayReg (b) + ", " + sayReg (c)
    }

    /**
     * Subtract the value in register b and the value im, store the result
     * in register a.
     */
    case class SUBI (a : RegNo, b : RegNo, im : Imm) extends Instr  {
        override def toString : String =
            "subi " + sayReg (a) + ", " + sayReg (b) + ", " + im
    }

    /**
     * Muliply the values in registers b and c, store the result in register a.
     */
    case class MUL (a : RegNo, b : RegNo, c : RegNo) extends Instr {
        override def toString : String =
            "mul " + sayReg (a) + ", " + sayReg (b) + ", " + sayReg (c)
    }

    /**
     * Multiply the value in register b and the value im, store the result
     * in register a.
     */
    case class MULI (a : RegNo, b : RegNo, im : Imm) extends Instr  {
        override def toString : String =
            "muli " + sayReg (a) + ", " + sayReg (b) + ", " + im
    }

    /**
     * Divide the values in registers b and c, store the (integer) result in
     * register a.
     */
    case class DIV (a : RegNo, b : RegNo, c : RegNo) extends Instr {
        override def toString : String =
            "div " + sayReg (a) + ", " + sayReg (b) + ", " + sayReg (c)
    }

    /**
     * Divide the value in register b and the value im, store the (integer)
     * result in register a.
     */
    case class DIVI (a : RegNo, b : RegNo, im : Imm) extends Instr  {
        override def toString : String =
            "divi " + sayReg (a) + ", " + sayReg (b) + ", " + im
    }

    /**
     * Divide the values in registers b and c, store the (integer) remainder
     * in register a.
     */
    case class MOD (a : RegNo, b : RegNo, c : RegNo) extends Instr {
        override def toString : String =
            "mod " + sayReg (a) + ", " + sayReg (b) + ", " + sayReg (c)
    }

    /**
     * Divide the value in registers b and the value im, store the (integer)
     * remainder in register a.
     */
    case class MODI (a : RegNo, b : RegNo, im : Imm) extends Instr  {
        override def toString : String =
            "modi " + sayReg (a) + ", " + sayReg (b) + ", " + im
    }

    /**
     * Set the Z condition code if the contents of registers b and c
     * are equal.  Set the N condition code if the content of register
     * b is less than the content of register c.
     */
    case class CMP (b : RegNo, c : RegNo) extends Instr {
        override def toString : String =
            "cmp " + sayReg (b) + ", " + sayReg (c)
     }

    /**
     * Set the Z condition code if the content of register b and the
     * value im are equal, otherwise clear Z.  Set the N condition code
     * if the content of register b is less than the value im, otherwise
     * clear N.
     */
    case class CMPI (b : RegNo, im : Imm) extends Instr {
        override def toString : String =
            "cmpi " + sayReg (b) + ", " + im
    }

    /**
     * If register a contains a value that is negative or greater than
     * or equal to the value im, set register a to zero.
     */
    case class CHKI (b : RegNo, im : Imm) extends Instr {
        override def toString : String =
            "chki " + sayReg (b) + ", " + im
    }

    /**
     * Bitwise AND the values in registers b and c, store the (integer) remainder
     * in register a.
     */
    case class AND (a : RegNo, b : RegNo, c : RegNo) extends Instr {
        override def toString : String =
            "and " + sayReg (a) + ", " + sayReg (b) + ", " + sayReg (c)
    }

    /**
     * Bitwise AND the value in registers b and the value im, store the (integer)
     * remainder in register a.
     */
    case class ANDI (a : RegNo, b : RegNo, im : Imm) extends Instr  {
        override def toString : String =
            "andi " + sayReg (a) + ", " + sayReg (b) + ", " + im
    }

    /**
     * Bitwise OR the values in registers b and c, store the (integer) remainder
     * in register a.
     */
    case class OR (a : RegNo, b : RegNo, c : RegNo) extends Instr {
        override def toString : String =
            "or " + sayReg (a) + ", " + sayReg (b) + ", " + sayReg (c)
    }

    /**
     * Bitwise OR the value in registers b and the value im, store the (integer)
     * remainder in register a.
     */
    case class ORI (a : RegNo, b : RegNo, im : Imm) extends Instr  {
        override def toString : String =
            "ori " + sayReg (a) + ", " + sayReg (b) + ", " + im
    }

    /**
     * Bitwise XOR the values in registers b and c, store the (integer) remainder
     * in register a.
     */
    case class XOR (a : RegNo, b : RegNo, c : RegNo) extends Instr {
        override def toString : String =
            "xor " + sayReg (a) + ", " + sayReg (b) + ", " + sayReg (c)
    }

    /**
     * Bitwise XOR the value in registers b and the value im, store the (integer)
     * remainder in register a.
     */
    case class XORI (a : RegNo, b : RegNo, im : Imm) extends Instr  {
        override def toString : String =
            "xori " + sayReg (a) + ", " + sayReg (b) + ", " + im
    }

    /**
     * Load register a with the word value stored in memory at the
     * address given by the contents of register b plus the value im.
     * The lowest two bits of the address are ignored.
     */
    case class LDW (a : RegNo, b : RegNo, im : Imm) extends Instr {
        override def toString : String =
            "ldw " + sayReg (a) + ", " + sayReg (b) + ", " + im
    }

    /**
     * Load register a with the byte value stored in memory at the
     * address given by the contents of register b plus the value im.
     */
    case class LDB (a : RegNo, b : RegNo, im : Imm) extends Instr {
        override def toString : String =
            "ldb " + sayReg (a) + ", " + sayReg (b) + ", " + im
    }

    /**
     * Load register a with the word value stored in register b.  The
     * lowest two bits of the address are ignored. Subtract (???) im from
     * the contents of register b and store the result in register b.
     */
    case class POP (a : RegNo, b : RegNo, im : Imm) extends Instr {
        override def toString : String =
            "pop " + sayReg (a) + ", " + sayReg (b) + ", " + im
    }

    /**
     * Store the contents of register a to memory at the address given
     * by the contents of register b plus the value im.  The lowest two
     * bits of the address are ignored.
     */
    case class STW (a : RegNo, b : RegNo, im : Imm) extends Instr {
        override def toString : String =
            "stw " + sayReg (a) + ", " + sayReg (b) + ", " + im
    }

    /**
     * Store the least-significant byte of the contents of register a to
     * memory at the address given by the contents of register b plus the
     * value im.
     */
    case class STB (a : RegNo, b : RegNo, im : Imm) extends Instr {
        override def toString : String =
            "stb " + sayReg (a) + ", " + sayReg (b) + ", " + im
    }

    /**
     * Add im to the contents of register b and store the
     * result in register b.  Store the value in register a into
     * memory at the address given by the contents of register b.
     * The lowest two bits of the address are ignored.
     */
    case class PSH (a : RegNo, b : RegNo, im : Imm) extends Instr {
        override def toString : String =
            "psh " + sayReg (a) + ", " + sayReg (b) + ", " + im
    }

    /**
     * Read an integer variable from standard input and store the value
     * in register a.
     */
    case class RD (a : RegNo) extends Instr {
        override def toString : String = "rd " + sayReg (a)
    }

    /**
     * Write a decimal representation of the value in register c to
     * standard output.
     */
    case class WRD (c : RegNo) extends Instr {
        override def toString : String = "wrd " + sayReg (c)
    }

    /**
     * Write a hexadecimal representation of the value in register c to
     * standard output.
     */
    case class WRH (c : RegNo) extends Instr {
        override def toString : String = "wrh " + sayReg (c)
    }

    /**
     * Write a newline to standard output.
     */
    case class WRL () extends Instr {
        override def toString : String = "wrl"
    }

    /**
     * Abstract interface for all branch instructions.  Branches are
     * created using symbolic labels. The assembler sets the disp
     * field once the symbolic label has been resolved to an offset.
     */
    sealed abstract class Branch extends Instr {
        def label : Int
        var disp : Disp = -1
        override def toString : String =
            if (disp != -1)
                "(" + disp + ")"
            else
                "label" + label
        def copy(label : Int = label) : Branch
    }

    /**
     * If the Z condition code is set, set the program counter to its
     * value plus four times disp.
     */
    case class BEQ (val label : Int) extends Branch {
        override def toString : String = "beq " + super.toString
        def copy(label : Int) : Branch = BEQ (label)
    }

    /**
     * If the Z condition code is clear, set the program counter to its
     * value plus four times disp.
     */
    case class BNE (val label : Int) extends Branch {
        override def toString : String = "bne " + super.toString
        def copy(label : Int) : Branch = BNE (label)
    }

    /**
     * If the N condition code is set, set the program counter to its
     * value plus four times disp.
     */
    case class BLT (val label : Int) extends Branch {
        override def toString : String = "blt " + super.toString
        def copy(label : Int) : Branch = BLT (label)
    }

    /**
     * If the N condition code is clear, set the program counter to its
     * value plus four times disp.
     */
    case class BGE (val label : Int) extends Branch {
        override def toString : String = "bge " + super.toString
        def copy(label : Int) : Branch = BGE (label)
    }

    /**
     * If either of the Z or N condition codes is set, set the program
     * counter to its value plus four times disp.
     */
    case class BLE (val label : Int) extends Branch {
        override def toString : String = "ble " + super.toString
        def copy(label : Int) : Branch = BLE (label)
    }

    /**
     * If both of the Z and N condition codes are clear, set the program
     * counter to its value plus four times disp.
     */
    case class BGT (val label : Int) extends Branch {
        override def toString : String = "bgt " + super.toString
        def copy(label : Int) : Branch = BGT (label)
    }

    /**
     * Set the program counter to its value plus disp.
     */
    case class BR (val label : Int) extends Branch {
        override def toString : String = "br " + super.toString
        def copy(label : Int) : Branch = BR (label)
    }

    /**
     * Set R31 to the value of the program counter plus one. Set the
     * program counter to its value plus disp.
     */
    case class BSR (val label : Int) extends Branch {
        override def toString : String = "bsr " + super.toString
        def copy(label : Int) : Branch = BSR (label)
    }

    /**
     * Set the program counter to the value in register c.  If that
     * value is zero, halt the machine.
     */
    case class RET (c : RegNo) extends Instr {
        override def toString : String = "ret " + sayReg (c)
    }

    /**
     * Pseudo instructions. These aren't really RISC machine instructions but
     * are used to insert comments and labels into a RISC assembly code program
     */
    sealed abstract class Pseudo extends Assembler

    /**
     * Branch target label
     * We call this Target rather than Label to avoid a name
     * clash with the Label class in RISCTree
     */
    case class Target (label : Int) extends Pseudo {
        override def toString : String = "label" + label + ":"
    }

    /**
     * Comment
     */
    case class Comment (text : String) extends Pseudo {
        override def toString : String = "! " + text
    }

    /**
     * Pretty-print a list of assembly code instructions
     * to an emitter
     */
     def prettyprint (emitter : Emitter, code : AssemCode) {
         for (line <- code) {
             line match {
                 case _ : Target => emitter.emitln (line)
                 case _          => emitter.emitln ("    " + line)
             }
         }
     }
}

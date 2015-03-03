/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2015 Anthony M Sloane, Macquarie University.
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
    type Code = Vector[Instr]

    /**
     * An assembler code sequence.
     * Includes comments and labels.
     */
    type AssemCode = Vector[Assembler]

    /**
     * Operand types
     */
    type Imm = Short
    type Disp = Int

    /**
     * Register numbers (0-31).  Program counter is R28.
     */
    type RegNo = Int

    def sayReg (reg : RegNo) : String = s"$$$reg"

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
            s"mov ${sayReg (a)}, ${sayReg (b)}, ${sayReg (c)}"
    }

    /**
     * Shift the value im by b and store the result in register a.
     */
    case class MOVI (a : RegNo, b : RegNo, im : Imm) extends Instr  {
        override def toString : String =
            s"movi ${sayReg (a)}, ${sayReg (b)}, $im"
    }

    /**
     * Shift the value in register c by b and store the negation of the result
     * in register a.
     */
    case class MVN (a : RegNo, b : RegNo, c : RegNo) extends Instr {
        override def toString : String =
            s"mvn ${sayReg (a)}, ${sayReg (b)}, ${sayReg (c)}"
    }

    /**
     * Shift the value im by b and store the negation of the result in
     * register a.
     */
    case class MVNI (a : RegNo, b : RegNo, im : Imm) extends Instr {
        override def toString : String =
            s"mvni ${sayReg (a)}, ${sayReg (b)}, $im"
    }

    /**
     * Add the values in registers b and c, store the result in register a.
     */
    case class ADD (a : RegNo, b : RegNo, c : RegNo) extends Instr {
        override def toString : String =
            s"add ${sayReg (a)}, ${sayReg (b)}, ${sayReg (c)}"
    }

    /**
     * Add the value in register b and the value im, store the result
     * in register a.
     */
    case class ADDI (a : RegNo, b : RegNo, im : Imm) extends Instr {
        override def toString : String =
            s"addi ${sayReg (a)}, ${sayReg (b)}, $im"
    }

    /**
     * Subtract the values in registers b and c, store the result in
     * register a.
     */
    case class SUB (a : RegNo, b : RegNo, c : RegNo) extends Instr  {
        override def toString : String =
            s"sub ${sayReg (a)}, ${sayReg (b)}, ${sayReg (c)}"
    }

    /**
     * Subtract the value in register b and the value im, store the result
     * in register a.
     */
    case class SUBI (a : RegNo, b : RegNo, im : Imm) extends Instr  {
        override def toString : String =
            s"subi ${sayReg (a)}, ${sayReg (b)}, $im"
    }

    /**
     * Muliply the values in registers b and c, store the result in register a.
     */
    case class MUL (a : RegNo, b : RegNo, c : RegNo) extends Instr {
        override def toString : String =
            s"mul ${sayReg (a)}, ${sayReg (b)}, ${sayReg (c)}"
    }

    /**
     * Multiply the value in register b and the value im, store the result
     * in register a.
     */
    case class MULI (a : RegNo, b : RegNo, im : Imm) extends Instr  {
        override def toString : String =
            s"muli ${sayReg (a)}, ${sayReg (b)}, $im"
    }

    /**
     * Divide the values in registers b and c, store the (integer) result in
     * register a.
     */
    case class DIV (a : RegNo, b : RegNo, c : RegNo) extends Instr {
        override def toString : String =
            s"div ${sayReg (a)}, ${sayReg (b)}, ${sayReg (c)}"
    }

    /**
     * Divide the value in register b and the value im, store the (integer)
     * result in register a.
     */
    case class DIVI (a : RegNo, b : RegNo, im : Imm) extends Instr  {
        override def toString : String =
            s"divi ${sayReg (a)}, ${sayReg (b)}, $im"
    }

    /**
     * Divide the values in registers b and c, store the (integer) remainder
     * in register a.
     */
    case class MOD (a : RegNo, b : RegNo, c : RegNo) extends Instr {
        override def toString : String =
            s"mod ${sayReg (a)}, ${sayReg (b)}, ${sayReg (c)}"
    }

    /**
     * Divide the value in registers b and the value im, store the (integer)
     * remainder in register a.
     */
    case class MODI (a : RegNo, b : RegNo, im : Imm) extends Instr  {
        override def toString : String =
            s"modi ${sayReg (a)}, ${sayReg (b)}, $im"
    }

    /**
     * Set the Z condition code if the contents of registers b and c
     * are equal.  Set the N condition code if the content of register
     * b is less than the content of register c.
     */
    case class CMP (b : RegNo, c : RegNo) extends Instr {
        override def toString : String =
            s"cmp ${sayReg (b)}, ${sayReg (c)}"
     }

    /**
     * Set the Z condition code if the content of register b and the
     * value im are equal, otherwise clear Z.  Set the N condition code
     * if the content of register b is less than the value im, otherwise
     * clear N.
     */
    case class CMPI (b : RegNo, im : Imm) extends Instr {
        override def toString : String =
            s"cmpi ${sayReg (b)}, $im"
    }

    /**
     * If register a contains a value that is negative or greater than
     * or equal to the value im, set register a to zero.
     */
    case class CHKI (b : RegNo, im : Imm) extends Instr {
        override def toString : String =
            s"chki ${sayReg (b)}, $im"
    }

    /**
     * Bitwise AND the values in registers b and c, store the (integer) remainder
     * in register a.
     */
    case class AND (a : RegNo, b : RegNo, c : RegNo) extends Instr {
        override def toString : String =
            s"and ${sayReg (a)}, ${sayReg (b)}, ${sayReg (c)}"
    }

    /**
     * Bitwise AND the value in registers b and the value im, store the (integer)
     * remainder in register a.
     */
    case class ANDI (a : RegNo, b : RegNo, im : Imm) extends Instr  {
        override def toString : String =
            s"andi ${sayReg (a)}, ${sayReg (b)}, $im"
    }

    /**
     * Bitwise OR the values in registers b and c, store the (integer) remainder
     * in register a.
     */
    case class OR (a : RegNo, b : RegNo, c : RegNo) extends Instr {
        override def toString : String =
            s"or ${sayReg (a)}, ${sayReg (b)}, ${sayReg (c)}"
    }

    /**
     * Bitwise OR the value in registers b and the value im, store the (integer)
     * remainder in register a.
     */
    case class ORI (a : RegNo, b : RegNo, im : Imm) extends Instr  {
        override def toString : String =
            s"ori ${sayReg (a)}, ${sayReg (b)}, $im"
    }

    /**
     * Bitwise XOR the values in registers b and c, store the (integer) remainder
     * in register a.
     */
    case class XOR (a : RegNo, b : RegNo, c : RegNo) extends Instr {
        override def toString : String =
            s"xor ${sayReg (a)}, ${sayReg (b)}, ${sayReg (c)}"
    }

    /**
     * Bitwise XOR the value in registers b and the value im, store the (integer)
     * remainder in register a.
     */
    case class XORI (a : RegNo, b : RegNo, im : Imm) extends Instr  {
        override def toString : String =
            s"xori ${sayReg (a)}, ${sayReg (b)}, $im"
    }

    /**
     * Load register a with the word value stored in memory at the
     * address given by the contents of register b plus the value im.
     * The lowest two bits of the address are ignored.
     */
    case class LDW (a : RegNo, b : RegNo, im : Imm) extends Instr {
        override def toString : String =
            s"ldw ${sayReg (a)}, ${sayReg (b)}, $im"
    }

    /**
     * Load register a with the byte value stored in memory at the
     * address given by the contents of register b plus the value im.
     */
    case class LDB (a : RegNo, b : RegNo, im : Imm) extends Instr {
        override def toString : String =
            s"ldb ${sayReg (a)}, ${sayReg (b)}, $im"
    }

    /**
     * Load register a with the word value stored in register b.  The
     * lowest two bits of the address are ignored. Subtract (???) im from
     * the contents of register b and store the result in register b.
     */
    case class POP (a : RegNo, b : RegNo, im : Imm) extends Instr {
        override def toString : String =
            s"pop ${sayReg (a)}, ${sayReg (b)}, $im"
    }

    /**
     * Store the contents of register a to memory at the address given
     * by the contents of register b plus the value im.  The lowest two
     * bits of the address are ignored.
     */
    case class STW (a : RegNo, b : RegNo, im : Imm) extends Instr {
        override def toString : String =
            s"stw ${sayReg (a)}, ${sayReg (b)}, $im"
    }

    /**
     * Store the least-significant byte of the contents of register a to
     * memory at the address given by the contents of register b plus the
     * value im.
     */
    case class STB (a : RegNo, b : RegNo, im : Imm) extends Instr {
        override def toString : String =
            s"stb ${sayReg (a)}, ${sayReg (b)}, $im"
    }

    /**
     * Add im to the contents of register b and store the
     * result in register b.  Store the value in register a into
     * memory at the address given by the contents of register b.
     * The lowest two bits of the address are ignored.
     */
    case class PSH (a : RegNo, b : RegNo, im : Imm) extends Instr {
        override def toString : String =
            s"psh ${sayReg (a)}, ${sayReg (b)}, $im"
    }

    /**
     * Read an integer variable from standard input and store the value
     * in register a.
     */
    case class RD (a : RegNo) extends Instr {
        override def toString : String = s"rd ${sayReg (a)}"
    }

    /**
     * Write a decimal representation of the value in register c to
     * standard output.
     */
    case class WRD (c : RegNo) extends Instr {
        override def toString : String = s"wrd ${sayReg (c)}"
    }

    /**
     * Write a hexadecimal representation of the value in register c to
     * standard output.
     */
    case class WRH (c : RegNo) extends Instr {
        override def toString : String = s"wrh ${sayReg (c)}"
    }

    /**
     * Write a newline to standard output.
     */
    case class WRL () extends Instr {
        override def toString : String = "wrl"
    }

    /**
     * Branch target labels. If the `disp` field is -1 then this label
     * has not been resolved. Otherwise, `disp` gives the offset from
     * the start of the code block.
     */
    case class Label (num : Int, disp : Int = -1) extends Instr {
        override def toString : String =
            if (disp == -1)
                s"label$num"
            else
                s"($disp)"
    }

    /**
     * Common type for all branch instructions.
     */
    sealed abstract class Branch extends Instr {
        def label : Label
    }

    /**
     * If the Z condition code is set, set the program counter to its
     * value plus four times disp.
     */
    case class BEQ (val label : Label) extends Branch {
        override def toString : String = s"beq $label"
    }

    /**
     * If the Z condition code is clear, set the program counter to its
     * value plus four times disp.
     */
    case class BNE (val label : Label) extends Branch {
        override def toString : String = s"bne $label"
    }

    /**
     * If the N condition code is set, set the program counter to its
     * value plus four times disp.
     */
    case class BLT (val label : Label) extends Branch {
        override def toString : String = s"blt $label"
    }

    /**
     * If the N condition code is clear, set the program counter to its
     * value plus four times disp.
     */
    case class BGE (val label : Label) extends Branch {
        override def toString : String = s"bge $label"
    }

    /**
     * If either of the Z or N condition codes is set, set the program
     * counter to its value plus four times disp.
     */
    case class BLE (val label : Label) extends Branch {
        override def toString : String = s"ble $label"
    }

    /**
     * If both of the Z and N condition codes are clear, set the program
     * counter to its value plus four times disp.
     */
    case class BGT (val label : Label) extends Branch {
        override def toString : String = s"bgt $label"
    }

    /**
     * Set the program counter to its value plus disp.
     */
    case class BR (val label : Label) extends Branch {
        override def toString : String = s"br $label"
    }

    /**
     * Set R31 to the value of the program counter plus one. Set the
     * program counter to its value plus disp.
     */
    case class BSR (val label : Label) extends Branch {
        override def toString : String = s"bsr $label"
    }

    /**
     * Set the program counter to the value in register c.  If that
     * value is zero, halt the machine.
     */
    case class RET (c : RegNo) extends Instr {
        override def toString : String = s"ret ${sayReg (c)}"
    }

    /**
     * Pseudo instructions. These aren't really RISC machine instructions but
     * are used to insert comments and labels into a RISC assembly code program
     */
    sealed abstract class Pseudo extends Assembler

    /**
     * Branch target label.
     */
    case class Target (label : Label) extends Pseudo {
        override def toString : String = s"$label:"
    }

    /**
     * Comment.
     */
    case class Comment (text : String) extends Pseudo {
        override def toString : String = s"! $text"
    }

    /**
     * Pretty-print a list of assembly code instructions to an emitter.
     */
     def prettyprint (emitter : Emitter, code : AssemCode) {
         for (line <- code) {
             line match {
                 case _ : Target => emitter.emitln (line)
                 case _          => emitter.emitln (s"    $line")
             }
         }
     }
}

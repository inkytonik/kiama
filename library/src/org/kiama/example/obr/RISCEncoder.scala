/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2015 Anthony M Sloane, Macquarie University.
 * Copyright (C) 2010-2015 Dominic Verity, Macquarie University.
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
package example.obr

import org.kiama.attribution.Attribution
import org.kiama.util.Emitter

/**
 * Translate RISC programs into RISC assembly code.  Completes the code
 * generation (e.g., by allocating registers). The resulting code is
 * made available as a list of RISC machine instructions.
 */
class RISCEncoder (labels : RISCLabels) extends Attribution {

    import labels.genlabelnum
    import RISCTree._
    import org.kiama.example.RISC.RISCISA.{Label => RISCLabel, _}
    import scala.collection.immutable.Seq
    import scala.math.max

    /**
     * The code sequence that is being assembled.
     */
    val code = Seq.newBuilder[Assembler]

    /**
     * Emit a RISC instruction.
     */
    def emit (instr : Assembler) {
        code += instr
    }

    /**
     * Emit a comment.
     */
    def emitcomment (text : String) {
        code += Comment (text)
    }

    /**
     * Reset the code cache
     */
    def resetcode () {
        code.clear
    }

    /**
     * Compute the final code sequence.  Compared to the one in the code
     * buffer, symbolic labels are resolved into numeric displacements
     * and all pseudo-instructions (comments and labels) are stripped
     * before the code is returned.
     */
    def getcode : Code = {

        // Get the final instruction sequence
        val instrs = code.result ()

        // Pass 1: compile mappings between labels and offsets
        val (labels, _) =
            instrs.foldLeft (Map[RISCLabel,Int] (), 0) {
                case ((labels, currloc), instr) =>
                    instr match {
                        case Target (lab) =>
                            (labels.updated (lab, currloc), currloc)
                        case _ : Instr =>
                            (labels, currloc + 1)
                        case _ =>
                            (labels, currloc)
                    }
            }

        // Pass 2: remove pseudo instructions, add displacements to labels
        val (newcode, _) =
            instrs.foldLeft (Seq.empty[Instr], 0) {
                case ((newcode, currloc), instr) =>
                    instr match {
                        case b : Branch =>
                            if (! (labels contains b.label))
                                sys.error (s"Assembler.resolve: unmarked label: ${b.label}")
                            val newlabel = b.label.copy (disp = labels (b.label) - currloc)
                            // I wish we could abstract over copy...
                            val newinstr =
                                b match {
                                    case _ : BEQ => BEQ (newlabel)
                                    case _ : BNE => BNE (newlabel)
                                    case _ : BLT => BLT (newlabel)
                                    case _ : BLE => BLE (newlabel)
                                    case _ : BGT => BGT (newlabel)
                                    case _ : BGE => BGE (newlabel)
                                    case _ : BR  => BR  (newlabel)
                                    case _ : BSR => BSR (newlabel)
                                }
                            (newcode :+ newinstr, currloc + 1)
                        case i : Instr =>
                            (newcode :+ i, currloc + 1)
                        case _ : Pseudo =>
                            (newcode, currloc)
                    }
            }

        newcode

    }

    /**
     * Return the raw assembler code
     */
    def getassem : AssemCode =
        code.result ()

    /**
     * Register allocations:
     *      $0          always set to 0
     *      $1-$26      temporaries
     *      $27         pointer to bottom of global data block
     *      $28         program counter
     *      $29         frame pointer
     *      $30         stack pointer
     *      $31         link pointer
     */

    /**
     * First and last temporary registers
     */
    val firsttemp = 1
    val lasttemp = 26

    /**
     * Local register to use for base address of memory block.
     */
    val memreg = 27


    /**
     * Encode the given RISC program by emitting the prologue, then the
     * encoding of each of the program's instructions, then the epilogue.
     */

    def encode (p : RISCProg) {

        // Tree for this RISC progarm
        val tree = new RISCTree (p)

        /*
         * Register allocation - we use an attribute grammar to implement
         * a stack style allocation of registers. Unless a specific node type
         * is handled by a special case, this attribution assumes that the
         * children of each node will be evaluated in left to right order.
         */
        lazy val reg : RISCNode => RegNo =
            attr {
                case tree.parent.pair (n, p) =>
                    p match {
                        // Base case
                        case _ : RISCProg =>
                            firsttemp

                        // Special cases for specific constructs
                        case p : Cond =>
                            reg (p)

                        case _ =>
                            n match {
                                case tree.prev (s : NeedsRegister) =>
                                    if (reg (s) >= lasttemp)
                                        sys.error ("out of local registers")
                                    reg (s) + 1
                                case tree.prev (s) =>
                                    reg (s)
                                case _ =>
                                    reg (p)
                            }
                    }
            }

        resetcode ()

        emitcomment("Prologue")
        emit (MOVI (memreg, 0, 0))

        val exitlab = genlabelnum ()
        (p.insns) map item

        emitcomment("Epilogue")
        emit (Target (RISCLabel (exitlab)))
        emit (RET (0))

        /*
         * Encode an item.  All registers are free for each item.  I.e.,
         * no values are passed between items via registers.
         */
        def item (i : Item) : Unit = {
            emitcomment (i.toString)
            i match {

                case Beq (cond, Label (dest)) =>
                    datum (cond)
                    emit (CMPI (reg (cond), 0))
                    emit (BEQ (RISCLabel (dest)))

                case Bne (cond, Label (dest)) =>
                    datum (cond)
                    emit (CMPI (reg (cond), 0))
                    emit (BNE (RISCLabel (dest)))

                case Jmp (Label (dest)) =>
                    emit (BR (RISCLabel (dest)))

                case LabelDef (lab) =>
                    emit (Target (RISCLabel (lab.num)))

                case Ret () =>
                    emit (BR (RISCLabel (exitlab)))

                // TODO check offsets are 16 bit (see toShort casts)
                case StW (Indexed (Local (locoff), indoff), d) =>
                    datum (indoff)
                    datum (d)
                    emit (ADD (reg (indoff), reg (indoff), memreg))
                    emit (STW (reg (d), reg (indoff), locoff.toShort))

                case StW (Local(offset), d) =>
                    datum (d)
                    emit (STW (reg (d), memreg, offset.toShort))

                case Write (d) =>
                    datum (d)
                    emit (WRD (reg (d)))
                    emit (WRL ())
            }
        }

        /*
         * Encode a comparison node
         */
        def compare (op : (RISCLabel) => Instr, l : Datum, r : Datum, d : Datum) {
            val lab = genlabelnum ()
            datum (l)
            datum (r)
            emit (CMP (reg (l), reg (r)))
            emit (MOVI (reg (d), 0, 1))
            emit (op (RISCLabel (lab)))
            emit (MOVI (reg (d), 0, 0))
            emit (Target (RISCLabel (lab)))
        }

        /*
         * Encode a 1-parameter arithmetic operation
         */
        def arith1 (op : (RegNo, RegNo) => Instr, e : Datum, d : Datum) {
            datum (e)
            emit (op (reg (d), reg (e)))
        }

        /*
         * Encode a 2-parameter arithmetic operation
         */
        def arith2 (op : (RegNo, RegNo, RegNo) => Instr, l : Datum, r : Datum, d : Datum) {
            datum (l)
            datum (r)
            emit (op (reg (d), reg (l), reg (r)))
        }

        /*
         * Encode a datum.
         */
        def datum (d : Datum) : Unit =
            d match {

                /*
                 * Arithmetic operations that correspond to a single RISC
                 * instruction.
                 */
                case AddW (l, r) =>
                    arith2 (ADD.apply _, l, r, d)

                case DivW (l, r) =>
                    arith2 (DIV.apply _, l, r, d)

                case MulW (l, r) =>
                    arith2 (MUL.apply _, l, r, d)

                case NegW (e) =>
                    arith1 (SUB.apply (_ : RegNo, 0, _ : RegNo), e, d)

                case Not (e) =>
                    val lab = genlabelnum ()
                    datum (e)
                    emit (CMPI (reg (e), 0))
                    emit (MOVI (reg (d), 0, 0))
                    emit (BNE (RISCLabel (lab)))
                    emit (MOVI (reg (d), 0, 1))
                    emit (Target (RISCLabel (lab)))

                case SubW (l, r) =>
                    arith2 (SUB.apply _, l, r, d)

                case RemW (l, r) =>
                    arith2 (MOD.apply _, l, r, d)

                /*
                 * Comparisons.
                 */
                case CmpeqW (l, r) =>
                    compare (BEQ.apply _, l, r, d)

                case CmpneW (l, r) =>
                    compare (BNE.apply _, l, r, d)

                case CmpgtW (l, r) =>
                    compare (BGT.apply _, l, r, d)

                case CmpltW (l, r) =>
                    compare (BLT.apply _, l, r, d)

                /*
                 * Since there is no single instruction that implements the
                 * semantics of a CondDatum, we need to use a sequence of
                 * instructions.  This code is essentially the same as would
                 * be generated by a conditional statement, but it returns
                 * a Boolean value as its result so it can be used inside
                 * other Datums.
                 */
                case Cond (cond, t, f) =>
                    val lab1 = genlabelnum ()
                    val lab2 = genlabelnum ()
                    datum (cond)
                    emit (CMPI (reg (cond), 0))
                    emit (BEQ (RISCLabel (lab1)))
                    datum (t)
                    emit (MOV (reg (d), 0, reg (t)))
                    emit (BR (RISCLabel (lab2)))
                    emit (Target (RISCLabel (lab1)))
                    datum (f)
                    emit (MOV (reg (d), 0, reg (f)))
                    emit (Target (RISCLabel (lab2)))

                /*
                 * An integer leaf seems a little bit more complicated
                 * than one might expect - but this is because we
                 * can only load constants into 32 bit registers in
                 * 16 bit chunks.
                 */
                case IntDatum (num) =>
                    if (num == num.toShort)
                        emit (MOVI (reg (d),0,num.toShort))
                    else {
                        emit (MOVI (reg (d), 0, 16))
                        emit (MOVI (reg (d), reg (d), (num >> 16).toShort))
                        emit (ORI (reg (d), reg (d), num.toShort))
                    }

                /*
                 * A load leaf just turns into an evaluation of the memory address
                 * and a load from that address into the datum's register.
                 */
                case LdW (Indexed (Local (locoff), indoff)) =>
                    datum (indoff)
                    emit (ADD (reg (indoff), reg (indoff), memreg))
                    emit (LDW (reg (d), reg (indoff), locoff.toShort))

                case LdW (Local (offset)) =>
                    emit (LDW (reg (d), memreg, offset.toShort))

                /*
                 * Read an integer value from the terminal.
                 */
                case Read () =>
                    emit (RD (reg (d)))

                /*
                 * Encode a compound sequence datum.
                 */
                case SequenceDatum (insns, d) =>
                    insns map item
                    datum (d)
            }

    }

}

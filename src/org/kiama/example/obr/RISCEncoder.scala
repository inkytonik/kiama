/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2010 Anthony M Sloane, Macquarie University.
 * Copyright (C) 2010-2012 Dominic Verity, Macquarie University.
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

import org.kiama.util.Emitter

/**
 * Translate RISC programs into RISC assembly code.  Completes the code
 * generation (e.g., by allocating registers). The resulting code is
 * made available as a list of RISC machine instructions.
 */
object RISCEncoder {

    import RISCTree._
    import org.kiama.example.RISC.RISCISA._
    import org.kiama.attribution.Attribution._

    import scala.collection.mutable.ArrayBuffer
    import scala.collection.immutable.HashMap
    import scala.math.max

    /**
     * The code sequence that is being assembled.
     */
    private val code = new ArrayBuffer[Assembler]

    /**
     * Emit a RISC instruction.
     */
    private def emit (instr : Assembler) {
        code append instr
    }

    /**
     * Emit a comment.
     */
    private def emitcomment (text : String) {
        code append Comment (text)
    }

    /**
     * Reset the code cache
     */
    def resetcode () {
        code.clear
    }

    /**
     * Return the code sequence that has been emitted.  Symbolic
     * labels are resolved into numeric displacements and all
     * pseudo-instructions (comments and labels) are stripped before
     * the code is returned.
     */
    def getcode : Code = {
        var labels = new HashMap[Int,Int]()

        var currloc = 0
        for (inst <- code) {
            inst match {
                case Target (lab)   => labels += (lab -> currloc)
                case i : Instr      => currloc += 1
                case _              =>
            }
        }

        currloc = 0
        for (inst <- code if inst.isInstanceOf[Instr]) yield {
            val newinst = (inst.asInstanceOf[Instr]) match {
                case b : Branch =>
                    if (! (labels contains b.label))
                        sys.error ("Assembler.resolve: unmarked label:" + b.label)
                    val newb = b.copy ()
                    newb.disp = labels (b.label) - currloc
                    newb
                case i          => i
            }
            currloc += 1
            newinst
        }
    }

    /**
     * Return the raw assembler code
     */
    def getassem : AssemCode = code

    /**
     * Generate a brand new target label.
     */
    private def gentarget () : Int = {
        val Label (label) = genlabel ()
        label
    }

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
    private final val firsttemp = 1
    private final val lasttemp = 26

    /**
     * Local register to use for base address of memory block.
     */
    private final val memreg = 27

    /**
     * Register allocation - we use an attribute grammar to implement
     * a stack style allocation of registers. Unless a specific node type
     * is handled by a special case, this attribution assumes that the
     * children of each node will be evaluated in left to right order.
     */
     private val reg : RISCNode => RegNo =
        childAttr {
            case d => {
                // Base case
                case _ : RISCProg                   => firsttemp

                // Special cases for specific constructs
                case p : Cond                       => p->reg

                // Default allocation algorithm for all other nodes
                case p : RISCNode if d.isFirst      => p->reg
                case _                              =>
                    d.prev[RISCNode] match {
                        case s : NeedsRegister  =>
                            if (s->reg >= lasttemp)
                                sys.error ("out of local registers")
                            (s->reg) + 1
                        case s =>
                            s->reg
                    }
            }
        }

    /**
     * Label for exit point
     */
    var exitlab : Int = _

    /**
     * Encode the given RISC program by emitting the prologue, then the
     * encoding of each of the program's instructions, then the epilogue.
     */

    def encode (p : RISCProg) {
        resetcode ()

        emitcomment("Prologue")
        emit (MOVI (memreg, 0, 0))

        exitlab = gentarget ()
        (p.insns) map encode

        emitcomment("Epilogue")
        emit (Target (exitlab))
        emit (RET (0))
    }

    /**
     * Encode an item.  All registers are free for each item.  I.e.,
     * no values are passed between items via registers.
     */
    private def encode (i : Item) : Unit = {
        emitcomment (i.toString)
        i match {

            case Beq (cond, Label (dest)) =>
                encode (cond)
                emit (CMPI (cond->reg, 0))
                emit (BEQ (dest))

            case Bne (cond, Label (dest)) =>
                encode (cond)
                emit (CMPI (cond->reg, 0))
                emit (BNE (dest))

            case Jmp (Label (dest)) =>
                emit (BR (dest))

            case LabelDef (lab) =>
                emit (Target (lab.num))

            case Ret () =>
                emit (BR (exitlab))

            // TODO check offsets are 16 bit (see toShort casts)
            case StW (Indexed (Local (locoff), indoff), d) =>
                encode (indoff)
                encode (d)
                emit (ADD (indoff->reg, indoff->reg, memreg))
                emit (STW (d->reg, indoff->reg, locoff.toShort))

            case StW (Local(offset), d) =>
                encode (d)
                emit (STW (d->reg, memreg, offset.toShort))

            case Write (d) =>
                encode (d)
                emit (WRD (d->reg))
                emit (WRL ())
        }
    }

    /**
     * Encode a comparison node
     */
    private def compare (op : (Int) => Instr, l : Datum, r : Datum, d : Datum) {
        val lab = gentarget ()
        encode (l)
        encode (r)
        emit (CMP (l->reg, r->reg))
        emit (MOVI (d->reg, 0, 1))
        emit (op (lab))
        emit (MOVI (d->reg, 0, 0))
        emit (Target (lab))
    }

    /**
     * Encode a 2-parameter arithmetic operation
     */
    private def arith (op : (RegNo, RegNo, RegNo) => Instr, l : Datum, r : Datum, d : Datum) {
        encode (l)
        encode (r)
        emit (op (d->reg, l->reg, r->reg))
    }

    /**
     * Encode a 1-parameter arithmetic operation
     */
    private def arith (op : (RegNo, RegNo) => Instr, e : Datum, d : Datum) {
        encode (e)
        emit (op (d->reg, e->reg))
    }

    /**
     * Encode a datum.
     */
    private def encode (d : Datum) : Unit =
        d match {

            /**
             * Arithmetic operations that correspond to a single RISC
             * instruction.
             */
            case AddW (l, r) =>
                arith (ADD.apply _, l, r, d)

            case DivW (l, r) =>
                arith (DIV.apply _, l, r, d)

            case MulW (l, r) =>
                arith (MUL.apply _, l, r, d)

            case NegW (e) =>
                arith (SUB.apply (_ : RegNo, 0, _ : RegNo), e, d)

            case Not (e) =>
                val lab = gentarget ()
                encode (e)
                emit (CMPI (e->reg, 0))
                emit (MOVI (d->reg, 0, 0))
                emit (BNE (lab))
                emit (MOVI (d->reg, 0, 1))
                emit (Target (lab))

            case SubW (l, r) =>
                arith (SUB.apply _, l, r, d)

            case RemW (l, r) =>
                arith (MOD.apply _, l, r, d)

            /**
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

            /**
             * Since there is no single instruction that implements the
             * semantics of a CondDatum, we need to use a sequence of
             * instructions.  This code is essentially the same as would
             * be generated by a conditional statement, but it returns
             * a Boolean value as its result so it can be used inside
             * other Datums.
             */
            case Cond (cond, t, f) =>
                val lab1 = gentarget ()
                val lab2 = gentarget ()
                encode (cond)
                emit (CMPI (cond->reg, 0))
                emit (BEQ (lab1))
                encode (t)
                emit (MOV (d->reg, 0, t->reg))
                emit (BR (lab2))
                emit (Target (lab1))
                encode (f)
                emit (MOV (d->reg, 0, f->reg))
                emit (Target (lab2))

            /**
             * An integer leaf seems a little bit more complicated
             * than one might expect - but this is because we
             * can only load constants into 32 bit registers in
             * 16 bit chunks.
             */
            case IntDatum (num) =>
                if (num == num.toShort)
                    emit (MOVI (d->reg,0,num.toShort))
                else {
                    emit (MOVI (d->reg, 0, 16))
                    emit (MOVI (d->reg, d->reg, (num >> 16).toShort))
                    emit (ORI (d->reg, d->reg, num.toShort))
                }

            /**
             * A load leaf just turns into an evaluation of the memory address
             * and a load from that address into the datum's register.
             */
            case LdW (Indexed (Local (locoff), indoff)) =>
                encode (indoff)
                emit (ADD (indoff->reg, indoff->reg, memreg))
                emit (LDW (d->reg, indoff->reg, locoff.toShort))

            case LdW (Local (offset)) =>
                emit (LDW (d->reg, memreg, offset.toShort))

            /**
             * Read an integer value from the terminal.
             */
            case Read () =>
                emit (RD (d->reg))

            /**
             * Encode a compound sequence datum.
             */
            case SequenceDatum (insns, d) =>
                insns map encode
                encode (d)
        }

}

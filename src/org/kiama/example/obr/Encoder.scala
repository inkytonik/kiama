/**
 * Encoder of SPARC language programs
 *
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2010 Anthony M Sloane, Macquarie University.
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
 * Translate SPARC programs into SPARC assembly code.  Completes the code
 * generation (e.g., by allocating registers) and outputs the code as text
 * in assembly code syntax.  Relies on the C run-time system for basic
 * facilities including input, output and some arithmetic operations. The
 * parameter is the emitter to use to actually output code.
 */
class Encoder (emitter : Emitter) {

    import SPARCTree._
    import org.kiama.attribution.Attribution._

    /**
     * Encode the given SPARC program by emitting the prologue, then the
     * encoding of each of the program's instructions, then the epilogue.
     */
    def encode (p : SPARC) {
        prologue (p.memsize)
        (p.insns) map encode
        epilogue
    }

    /**
     * Local register to use for base address of memory block.
     */
    private final val memreg = "%l0"

    /**
     * The available local registers for intermediate results.
     * We only allocate local registers from %l1 to %l7. If
     * availregs is n then it means that registers n+1 to 7
     * (inclusive) are free.
     */
    private var availregs = 0

    /**
     * Reset the available registers so that they are all free.
     */
    private def resetavailregs () {
        availregs = 0
    }

    /**
     * Select a register from the available registers.  We always
     * select the first free one.  Fatal error if we run out of
     * registers.
     */
    private def selectreg () : Int = {
        if (availregs == 7)
            error ("out of local registers")
        availregs = availregs + 1
        availregs
    }

    /**
     * Register allocation.  Each datum has a register in which its
     * value will be stored.  We reuse registers when we can.
     * E.g., in an addition the result is placed in the same register
     * as the right operand since the latter will not be needed again.
     * Leaves actually allocate a register from the available registers.
     */
    val reg : Datum ==> Int =
        attr {
            case AddW (_, r)       => r->reg
            case Cond (cond, t, f) => cond->reg
            case CmpeqW (_, r)     => r->reg
            case CmpneW (_, r)     => r->reg
            case CmpgtW (_, r)     => r->reg
            case CmpltW (_, r)     => r->reg
            case DivW (_, r)       => r->reg
            case IntDatum (_)      => selectreg
            case LdW (_)           => selectreg
            case MulW (_, r)       => r->reg
            case NegW (d)          => d->reg
            case Not (d)           => d->reg
            case RemW (_, r)       => r->reg
            case SubW (_, r)       => r->reg
        }

    /**
     * Register to use for passing the first argument to a function.
     */
    private final val arg1reg = "%o0"

    /**
     * Register to use for passing the second argument to a function.
     */
    private final val arg2reg = "%o1"

    /**
     * Register to use for getting a value back from a function.
     */
    private final val resreg = "%o0"

    /**
     * Emit a formatted instruction.
     */
    private def emit (insn : String) {
        emitter.emit ("    " + insn + "\n")
    }

    /**
     * Emit a string label.
     */
    private def label (lab : String) {
        emitter.emit (lab + ":\n")
    }

    /**
     * Emit a generated label.
     */
    private def label (lab : Label) {
        label (lab.toString)
    }

    /**
     * Convert a datum into an assembly language operand to access
     * the register where its value is stored.
     */
    private def opnd (d : Datum) : String =
        "%l" + (d->reg)

    /**
     * Convert an address to an assembly language operand.
     */
    private def opnd (a : Address) : String =
        a match {
            case Local (0)           => "[%l0]"
            case Local (offset)      => "[%l0+" + offset + "]"
            case Indexed (_, offset) => "[%l0+" + opnd (offset) + "]"
        }

    /**
     * Output the code prologue which sets things up so that the actual code
     * of the program can run.  Mainly this provides a "main" entry point
     * for the C run time to call the program "main" and allocates a memory
     * block of the appropriate size.
     */
    private def prologue (memsize : Int) {
        emit ("! Prologue")
        emit (".seg \"data\"")
        label ("ifmt")
        emit (".asciz \"%d\"")
        label ("ofmt")
        emit (".asciz \"%d\\n\"")
        emit (".align 4")
        label ("mem")
        emit (".skip " + memsize)
        emit (".seg \"text\"")
        emit (".globl main")
        label ("main")
        emit ("save %sp, -112, %sp")
        emit ("set mem, " + memreg)
    }

    /**
     * Encode an item.  All registers are free for each item.  I.e.,
     * no values are passed between items via registers.
     */
    private def encode (i : Item) : Unit = {
        resetavailregs
        emit ("! " + i)
        i match {

            case Beq (cond, dest) =>
                encode (cond)
                emit ("tst " + opnd (cond))
                emit ("be " + dest)
                emit ("nop")

            case Bne (cond, dest) =>
                encode (cond)
                emit ("tst " + opnd (cond))
                emit ("bne " + dest)
                emit ("nop")

            case Jmp (dest) =>
                emit ("ba " + dest)
                emit ("nop")

            case LabelDef (lab) =>
                label (lab.toString)

            case Read (Local (offset)) =>
                emit ("set ifmt, " + arg1reg)
                emit ("add " + memreg + ", " + offset + ", " + arg2reg)
                emit ("call scanf")
                emit ("nop")

            case Ret =>
                emit ("ba go")
                emit ("nop")

            case StW (mem, d) =>
                encode (d)
                encode (mem)
                emit ("st " + opnd (d) + ", " + opnd (mem))

            case Write (d) =>
                encode (d)
                emit ("set ofmt, " + arg1reg)
                emit ("mov " + opnd (d) + ", " + arg2reg)
                emit ("call printf")
                emit ("nop")

        }
    }

    /**
     * Output an arithmetic instruction with the given opcode, one
     * operand that must be encoded and a destination operand.
     */
    private def arith2 (opcode : String, e : Datum, d : Datum) {
        encode (e)
        emit (opcode + " " + opnd (e) + ", " + opnd (d))
    }

    /**
     * Output an arithmetic instruction with the given opcode, two
     * operands that must be encoded and a destination operand.
     */
    private def arith3 (opcode : String, l : Datum, r : Datum, d : Datum) {
        encode (l)
        encode (r)
        emit (opcode + " " + opnd (l) + ", " + opnd (r) + ", " + opnd (d))
    }

    /**
     * Output a comparison where l and r are the things to be compared,
     * d is the destination for the result and br gives the branch instruction
     * to use.
     */
    private def compare (br : String, l : Datum, r : Datum, d : Datum) {
        val lab = genlabel
        encode (l)
        encode (r)
        emit ("cmp " + opnd (l) + ", " + opnd (r))
        emit ("mov 1, " + opnd (d))
        emit (br + " " + lab)
        emit ("nop")
        emit ("mov 0, " + opnd (d))
        label (lab.toString)
    }

    /**
     * Encode a datum.
     */
    private def encode (d : Datum) : Unit =
        d match {

            /**
             * Arithmetic operations that correspond to a single SPARC
             * instruction.
             */
            case AddW (l, r) =>
                arith3 ("add", l, r, d)

            case DivW (l, r) =>
                arith3 ("sdiv", l, r, d)

            case MulW (l, r) =>
                arith3 ("smul", l, r, d)

            case NegW (e) =>
                arith2 ("neg", e, d)

            case Not (e) =>
                // Relies on d and e registers being the same
                encode (e)
                emit ("btog 1, " + opnd (e))

            case SubW (l, r) =>
                arith3 ("sub", l, r, d)

            /**
             * The SPARC does not have a standard instruction for remainder
             * so we call a C run time routine ".rem" that does the job.
             */
            case RemW (l, r) =>
                encode (l)
                encode (r)
                emit ("mov " + opnd (l) + ", " + arg1reg)
                emit ("mov " + opnd (r) + ", " + arg2reg)
                emit ("call .rem")
                emit ("nop")
                emit ("mov " + resreg + ", " + opnd (d))

            /**
             * Comparisons.
             */
            case CmpeqW (l, r) =>
                compare ("be", l, r, d)

            case CmpneW (l, r) =>
                compare ("bne", l, r, d)

            case CmpgtW (l, r) =>
                compare ("bg", l, r, d)

            case CmpltW (l, r) =>
                compare ("bl", l, r, d)

            /**
             * Since there is no single instruction that implements the
             * semantics of a CondDatum, we need to use a sequence of
             * instructions.  This code is essentially the same as would
             * be generated by a conditional statement, but it returns
             * a Boolean value as its result so it can be used inside
             * other Datums.
             */
            case Cond (cond, t, f) =>
                val lab1 = genlabel
                val lab2 = genlabel
                encode (cond)
                emit ("tst " + opnd (cond))
                emit ("be " + lab1)
                emit ("nop")
                encode (t)
                emit ("mov " + opnd (t) + ", " + opnd (d))
                emit ("ba " + lab2)
                emit ("nop")
                label (lab1)
                encode (f)
                emit ("mov " + opnd (f) + ", " + opnd (d))
                label (lab2)

            /**
             * An integer leaf just turns into a move of the integer into
             * the datum's register.
             */
            case IntDatum (num) =>
                emit ("mov " + num + ", " + opnd (d))

            /**
             * A load leaf just turns into an evaluation of the memory address
             * and a load from that address into the datum's register.
             */
            case LdW (mem) =>
                encode (mem)
                emit ("ld " + opnd (mem) + ", " + opnd (d))

        }

    /**
     * Encode an address (if necessary).  An indexed address needs to compute
     * the actual offset of the index from the local memory block by computing
     * the index offset and adding the base offset of the local from which
     * the index is being taken.
     */
    private def encode (a : Address) : Unit =
        a match {
            case Indexed (Local (locoff), indoff) =>
                encode (AddW (IntDatum (locoff), indoff))
            case Local (offset) =>
                // Do nothing
        }

    /**
     * Output the code epilogue to which control is transferred (label go)
     * when the program is finished.  It basically cleans up and returns
     * control to the C run time system which terminates the program.
     */
    private def epilogue () {
        emit ("! Epilogue")
        label ("go")
        emit ("ret")
        emit ("restore")
    }

}

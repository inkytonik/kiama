/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2012-2015 Anthony M Sloane, Macquarie University.
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
package example.minijava

/**
 * Code generator that prints JVM target trees to Jasmine assembler files.
 */
object CodeGenerator {

    import java.io.{BufferedWriter, FileWriter, PrintWriter}
    import JVMTree._
    import org.kiama.util.{Emitter, FileEmitter}

    /**
     * Generate the Jasmine code for a single classfile.
     */
    def generate (isTest : Boolean, classfile : ClassFile, emitter : Emitter) {

        /*
         * If it's a test use the provided emitter for output, otheriwse make
         * a file emitter that is based on the the class name and use that.
         */
        val codeEmitter =
            if (isTest)
                emitter
            else
                new FileEmitter (s"${classfile.name}.j")

        // Output header
        codeEmitter.emitln (s".source ${classfile.source}")
        codeEmitter.emitln (s".class public ${classfile.name}")
        codeEmitter.emitln (s".super ${classfile.superclassname}")

        // Output the fields
        classfile.fields.map (generateField)

        // Output the default constructor
        codeEmitter.emitln
        codeEmitter.emitln (".method <init>()V")
        codeEmitter.emitln (".limit stack 1")
        codeEmitter.emitln (".limit locals 1")
        codeEmitter.emitln ("    aload_0")
        codeEmitter.emitln (s"    invokespecial ${classfile.superclassname}/<init>()V")
        codeEmitter.emitln ("    return")
        codeEmitter.emitln (".end method")

        // Output the methods
        classfile.methods.map (generateMethod)

        /*
         * Generate a declaration for a field.
         */
        def generateField (field : JVMField) {
            codeEmitter.emitln (s".field public ${field.name} ${field.tipe}")
        }

        /*
         * Generate a declaration of a method including its code.
         */
        def generateMethod (method : JVMMethod) {

            /*
             * Calculate the maximum location number used in the method by going
             * through the instructions checking all loads and stores. The fold
             * propagates the maximum so far.
             */
            val maxloc =
                method.instrs.foldLeft (method.spec.argTypes.length) {
                    case (maxsofar, instr) =>
                        instr match {
                            case Iload (loc)  => maxsofar.max (loc)
                            case Istore (loc) => maxsofar.max (loc)
                            case Aload (loc)  => maxsofar.max (loc)
                            case Astore (loc) => maxsofar.max (loc)
                            case _            => maxsofar
                        }
                }

            /*
             * Calculate the maximum stack depth by going through the instructions
             * simulating the effect that each instruction has on the stack size.
             * The fold propagates the maximum so far and the current stack depth.
             */
            val (maxstack, _) =
                method.instrs.foldLeft ((0, 0)) {
                    case ((maxsofar, depth), instr) =>
                        val newdepth = depth + instr.stackChange
                        (maxsofar.max (newdepth), newdepth)
                }

            codeEmitter.emitln
            codeEmitter.emit (".method public ")
            if (method.isStatic)
                codeEmitter.emit ("static ")
            codeEmitter.emitln (method.spec)
            codeEmitter.emitln (s".limit stack $maxstack")
            codeEmitter.emitln (s".limit locals ${maxloc + 1}")
            method.instrs.map (generateInstr)
            codeEmitter.emitln (".end method")

            /*
             * Close up the file if we are using one.
             */
            if (!isTest)
                codeEmitter.close ()

        }

        /*
         * Generate an intstruction. Instructions that are not label declarations
         * are output using the name of their class converted to lower case. Each
         * argument is output in the order that it appears in the instruction
         * instance. Thus, this code does not have to be extended when new
         * instruction types are added.
         */
        def generateInstr (instr : JVMInstr) {
            instr match {
                case Label (label) =>
                    codeEmitter.emitln (s"$label:")
                case _ =>
                    codeEmitter.emit (s"    ${instr.productPrefix.toLowerCase}")
                    for (arg <- instr.productIterator) {
                        codeEmitter.emit (s" $arg")
                    }
                    codeEmitter.emitln
            }
        }

    }

}

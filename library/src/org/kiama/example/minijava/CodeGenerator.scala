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
object CodeGenerator extends org.kiama.output.PrettyPrinter {

    import JVMTree._
    import org.kiama.util.{Emitter, FileEmitter}

    /**
     * Generate the Jasmine code for a single classfile.
     */
    def generate (isTest : Boolean, classfile : ClassFile, emitter : Emitter) {

        // If it's a test use the provided emitter for output, otherwise make
        // a file emitter that is based on the the class name and use that.
        val codeEmitter =
            if (isTest)
                emitter
            else
                new FileEmitter (s"${classfile.name}.j")

        // Pretty-print and emit
        codeEmitter.emit (pretty (classFileToDoc (classfile)).layout)

        // Close up the file if we are using one.
        if (!isTest)
            codeEmitter.close ()

    }

    /**
     * Generate a file document for a classfile.
     */
    def classFileToDoc (classfile : ClassFile) : Doc = {

        val header =
            ".source" <+> classfile.filename <@>
            ".class public" <+> classfile.name <@>
            ".super" <+> classfile.superclassname

        val defaultConstructor =
            line <>
            ".method <init>()V" <@>
            ".limit stack 1" <@>
            ".limit locals 1" <@>
            "    aload_0" <@>
            "    invokespecial" <+> classfile.superclassname <> "/<init>()V" <@>
            "    return" <@>
            ".end method" <>
            line

        link (classfile.source,
            header <>
            hcat (classfile.fields.map (fieldToDoc)) <@>
            defaultConstructor <>
            hcat (classfile.methods.map (methodToDoc))) <>
        line

    }

    /*
     * Generate a declaration for a field.
     */
    def fieldToDoc (field : JVMField) : Doc =
        line <> link (field, ".field public" <+> field.name <+> value (field.tipe))

    /*
     * Generate a declaration of a method including its code.
     */
    def methodToDoc (method : JVMMethod) : Doc = {

        // Calculate the maximum location number used in the method by going
        // through the operations checking all loads and stores. The fold
        // propagates the maximum so far.
        val maxloc =
            method.instrs.foldLeft (method.spec.argTypes.length) {
                case (maxsofar, JVMInstr (op, _)) =>
                    op match {
                        case Iload (loc)  => maxsofar.max (loc)
                        case Istore (loc) => maxsofar.max (loc)
                        case Aload (loc)  => maxsofar.max (loc)
                        case Astore (loc) => maxsofar.max (loc)
                        case _            => maxsofar
                    }
            }

        /*
         * Calculate the maximum stack depth by going through the instructions
         * simulating the effect that each operation has on the stack size.
         * The fold propagates the maximum so far and the current stack depth.
         */
        val (maxstack, _) =
            method.instrs.foldLeft ((0, 0)) {
                case ((maxsofar, depth), JVMInstr (op, _)) =>
                    val newdepth = depth + op.stackChange
                    (maxsofar.max (newdepth), newdepth)
            }

        line <>
        link (method.source,
            ".method public" <+>
                (if (method.isStatic) "static " else empty) <>
                value (method.spec) <@>
            ".limit stack" <+> value (maxstack) <@>
            ".limit locals" <+> value (maxloc + 1) <>
            hcat (method.instrs.map (instrToDoc)) <@>
            ".end method") <>
        line

    }

    /*
     * Generate an instruction. Instructions that are not label declarations
     * are output using the name of their class converted to lower case. Each
     * argument is output in the order that it appears in the instruction
     * instance. Thus, this code does not have to be extended when new
     * instruction types are added.
     */
    def instrToDoc (instr : JVMInstr) : Doc =
        instr.op match {
            case Label (label) =>
                line <> link (instr.source, label <> colon)
            case op =>
                nest (
                    line <>
                    link (instr.source,
                        op.productPrefix.toLowerCase <>
                        hcat (op.productIterator.toVector.map {
                                 case arg => space <> value (arg)
                              }))
                )
        }

}

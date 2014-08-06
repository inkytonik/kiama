/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2012-2014 Anthony M Sloane, Macquarie University.
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
 * Module containing tree structures for representing JVM programs. Not all
 * features of the JVM are expressible with this representation.
 */
object JVMTree {

    import scala.collection.immutable.Seq

    /**
     * A class file defining a JVM class. `source` gives the name of the source
     * file from which this class file comes. `name` gives the name of the class
     * that is being defined, and `superClass` is the name of the super class
     * of this class. `fields` and `methods` define the components of the class.
     */
    case class ClassFile (source : String, name : String, superclassname : String,
                          fields : Seq[JVMField], methods : Seq[JVMMethod])

    /**
     * Base class for JVM types.
     */
    sealed abstract class JVMType

    /**
     * A JVM array with the given element type.
     */
    case class JVMArrayType (elemType : JVMType) extends JVMType {
        override def toString = s"[$elemType"
    }

    /**
     * The Boolean type.
     */
    case class JVMBooleanType () extends JVMType {
        override def toString = "Z"
    }

    /**
     * The integer type.
     */
    case class JVMIntType () extends JVMType {
        override def toString = "I"
    }

    /**
     * A class type.
     */
    case class JVMClassType (name : String) extends JVMType {
        override def toString = s"L$name;"
    }

    /**
     * The string type.
     */
    case class JVMStringType () extends JVMType {
        override def toString = "Ljava/lang/String;"
    }

    /**
     * The void type.
     */
    case class JVMVoidType () extends JVMType {
        override def toString = "V"
    }

    /**
     * A field in the class.
     */
    case class JVMField (name : String, tipe : JVMType)

    /**
     * A method in the class.
     */
    case class JVMMethod (spec : JVMMethodSpec, isStatic : Boolean,
                          instrs : Seq[JVMInstr])

    /**
     * A method specification.
     */
    case class JVMMethodSpec (name : String, argTypes : Seq[JVMType], retType : JVMType) {
        override def toString = name + argTypes.mkString ("(", "", ")") + retType
    }

    /**
     * Base class for JVM instructions. `stackChange` records the number of
     * elements by which the stack changes after execution of an instruction
     * of this type. Thus, zero means no change, a positive value means an
     * increase in stack size, and a negative value means a decrease in stack
     * size.
     */
    sealed abstract class JVMInstr extends Product {
        def stackChange : Int
    }

    /**
     * JVM instructions. See JVM documentation for descriptions.
     *
     * This list does not include all available instructions. To add a new one,
     * just add it as a case class whose name is the instruction name with at
     * least a starting capital letter.
     *
     * The arguments of the case class should be the instruction operands in the
     * order in which they must appear in an instruction instance.
     *
     * The `stackChange` value must be the net change in stack elements when an
     * instance of this instruction runs. E.g., zero means there is no change,
     * whereas a stack change of one means that there is one more element on
     * the operand stack after the instruction has executed than there is
     * before.
     */

    case class Aload (loc : Int) extends JVMInstr {
        override val stackChange = 1
    }

    case class ArrayLength () extends JVMInstr {
        override val stackChange = 1
    }

    case class Areturn () extends JVMInstr {
        override val stackChange = -1
    }

    case class Astore (loc : Int) extends JVMInstr {
        override val stackChange = -1
    }

    case class Bipush (num : Int) extends JVMInstr {
        override val stackChange = 1
    }

    case class Dup () extends JVMInstr {
        override val stackChange = 1
    }

    case class GetField (name : String, tipe : JVMType) extends JVMInstr {
        override val stackChange = 1
    }

    case class GetStatic (name : String, tipe : JVMType) extends JVMInstr {
        override val stackChange = 1
    }

    case class Goto (label : String) extends JVMInstr {
        override val stackChange = 0
    }

    case class Iadd () extends JVMInstr {
        override val stackChange = -1
    }

    case class Iload (loc : Int) extends JVMInstr {
        override val stackChange = 1
    }

    case class Iaload () extends JVMInstr {
        override val stackChange = -1
    }

    case class Iastore () extends JVMInstr {
        override val stackChange = -3
    }

    case class Iconst_m1 () extends JVMInstr {
        override val stackChange = 1
    }

    case class Iconst_0 () extends JVMInstr {
        override val stackChange = 1
    }

    case class Iconst_1 () extends JVMInstr {
        override val stackChange = 1
    }

    case class Iconst_2 () extends JVMInstr {
        override val stackChange = 1
    }

    case class Iconst_3 () extends JVMInstr {
        override val stackChange = 1
    }

    case class Iconst_4 () extends JVMInstr {
        override val stackChange = 1
    }

    case class Iconst_5 () extends JVMInstr {
        override val stackChange = 1
    }

    case class If_icmpge (label : String) extends JVMInstr {
        override val stackChange = -2
    }

    case class Ifeq (label : String) extends JVMInstr {
        override val stackChange = -1
    }

    case class Ifne (label : String) extends JVMInstr {
        override val stackChange = -1
    }

    case class Ireturn () extends JVMInstr {
        override val stackChange = -1
    }

    case class Istore (loc : Int) extends JVMInstr {
        override val stackChange = -1
    }

    case class Isub () extends JVMInstr {
        override val stackChange = -1
    }

    case class Imul () extends JVMInstr {
        override val stackChange = -1
    }

    case class InvokeSpecial (spec : JVMMethodSpec) extends JVMInstr {
        override val stackChange =
            (if (spec.retType == JVMVoidType ()) 0 else 1) -
                (1 + spec.argTypes.length)

    }

    case class InvokeVirtual (spec : JVMMethodSpec) extends JVMInstr {
        override val stackChange =
            (if (spec.retType == JVMVoidType ()) 0 else 1) -
                (1 + spec.argTypes.length)

    }

    case class Ldc (num : Int) extends JVMInstr {
        override val stackChange = 1
    }

    case class New (className : String) extends JVMInstr {
        override val stackChange = 1
    }

    case class NewArray (elemtype : String) extends JVMInstr {
        override val stackChange = 0
    }

    case class PutField (name : String, tipe : JVMType) extends JVMInstr {
        override val stackChange = -2
    }

    case class Return () extends JVMInstr {
        override val stackChange = 0
    }

    /**
     * A special instruction to mark the position of a label. These will be
     * output symbolically by the compiler but then replaced with offsets by
     * the assembler.
     */
    case class Label (label : String) extends JVMInstr {
        override val stackChange = 0
    }

}

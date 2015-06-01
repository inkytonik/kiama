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
 * Module containing tree structures for representing JVM programs. Not all
 * features of the JVM are expressible with this representation.
 */
object JVMTree {

    import MiniJavaTree.MiniJavaNode

    /**
     * A class file defining a JVM class. `source` gives the name of the source
     * file from which this class file comes. `name` gives the name of the class
     * that is being defined, and `superClass` is the name of the super class
     * of this class. `fields` and `methods` define the components of the class.
     */
    case class ClassFile (source : MiniJavaNode, filename : String, name : String,
                          superclassname : String, fields : List[JVMField],
                          methods : List[JVMMethod])

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
    case class JVMMethod (source : MiniJavaNode, spec : JVMMethodSpec, isStatic : Boolean,
                          instrs : Vector[JVMInstr])

    /**
     * A method specification.
     */
    case class JVMMethodSpec (name : String, argTypes : List[JVMType], retType : JVMType) {
        override def toString = name + argTypes.mkString ("(", "", ")") + retType
    }

    /*
     * An instruction and its associated MiniJava construct.
     */
    case class JVMInstr (op : JVMOp, source : MiniJavaNode)

    /**
     * Base class for JVM operations. `stackChange` records the number of
     * elements by which the stack changes after execution of an operation
     * of this type. Thus, zero means no change, a positive value means an
     * increase in stack size, and a negative value means a decrease in stack
     * size.
     */
    sealed abstract class JVMOp extends Product {
        def stackChange : Int
    }

    /**
     * JVM operations. See JVM documentation for descriptions.
     *
     * This list does not include all available operations. To add a new one,
     * just add it as a case class whose name is the operation name with at
     * least a starting capital letter.
     *
     * The arguments of the case class should be the operation operands in the
     * order in which they must appear in an operation instance.
     *
     * The `stackChange` value must be the net change in stack elements when an
     * instance of this operation runs. E.g., zero means there is no change,
     * whereas a stack change of one means that there is one more element on
     * the operand stack after the operation has executed than there is
     * before.
     */

    case class Aload (loc : Int) extends JVMOp {
        override val stackChange = 1
    }

    case class ArrayLength () extends JVMOp {
        override val stackChange = 1
    }

    case class Areturn () extends JVMOp {
        override val stackChange = -1
    }

    case class Astore (loc : Int) extends JVMOp {
        override val stackChange = -1
    }

    case class Bipush (num : Int) extends JVMOp {
        override val stackChange = 1
    }

    case class Dup () extends JVMOp {
        override val stackChange = 1
    }

    case class GetField (name : String, tipe : JVMType) extends JVMOp {
        override val stackChange = 1
    }

    case class GetStatic (name : String, tipe : JVMType) extends JVMOp {
        override val stackChange = 1
    }

    case class Goto (label : String) extends JVMOp {
        override val stackChange = 0
    }

    case class Iadd () extends JVMOp {
        override val stackChange = -1
    }

    case class Iload (loc : Int) extends JVMOp {
        override val stackChange = 1
    }

    case class Iaload () extends JVMOp {
        override val stackChange = -1
    }

    case class Iastore () extends JVMOp {
        override val stackChange = -3
    }

    case class Iconst_m1 () extends JVMOp {
        override val stackChange = 1
    }

    case class Iconst_0 () extends JVMOp {
        override val stackChange = 1
    }

    case class Iconst_1 () extends JVMOp {
        override val stackChange = 1
    }

    case class Iconst_2 () extends JVMOp {
        override val stackChange = 1
    }

    case class Iconst_3 () extends JVMOp {
        override val stackChange = 1
    }

    case class Iconst_4 () extends JVMOp {
        override val stackChange = 1
    }

    case class Iconst_5 () extends JVMOp {
        override val stackChange = 1
    }

    case class If_icmpge (label : String) extends JVMOp {
        override val stackChange = -2
    }

    case class Ifeq (label : String) extends JVMOp {
        override val stackChange = -1
    }

    case class Ifne (label : String) extends JVMOp {
        override val stackChange = -1
    }

    case class Ireturn () extends JVMOp {
        override val stackChange = -1
    }

    case class Istore (loc : Int) extends JVMOp {
        override val stackChange = -1
    }

    case class Isub () extends JVMOp {
        override val stackChange = -1
    }

    case class Imul () extends JVMOp {
        override val stackChange = -1
    }

    case class InvokeSpecial (spec : JVMMethodSpec) extends JVMOp {
        override val stackChange =
            (if (spec.retType == JVMVoidType ()) 0 else 1) -
                (1 + spec.argTypes.length)

    }

    case class InvokeVirtual (spec : JVMMethodSpec) extends JVMOp {
        override val stackChange =
            (if (spec.retType == JVMVoidType ()) 0 else 1) -
                (1 + spec.argTypes.length)

    }

    case class Ldc (num : Int) extends JVMOp {
        override val stackChange = 1
    }

    case class New (className : String) extends JVMOp {
        override val stackChange = 1
    }

    case class NewArray (elemtype : String) extends JVMOp {
        override val stackChange = 0
    }

    case class PutField (name : String, tipe : JVMType) extends JVMOp {
        override val stackChange = -2
    }

    case class Return () extends JVMOp {
        override val stackChange = 0
    }

    /**
     * A special instruction to mark the position of a label. These will be
     * output symbolically by the compiler but then replaced with offsets by
     * the assembler.
     */
    case class Label (label : String) extends JVMOp {
        override val stackChange = 0
    }

}

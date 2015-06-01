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

import MiniJavaTree.MiniJavaTree
import org.kiama.attribution.Attribution

/**
 * Translator from MiniJava source programs to JVM target programs.
 */
class Translator (tree : MiniJavaTree) extends Attribution {

    import JVMTree._
    import MiniJavaTree._
    import org.kiama.util.Counter
    import SymbolTable._

    /**
     * Return a JVM class file representation that implements the given
     * MiniJava program which came from the given source file. Return
     * one class file for each class in the MiniJava program.
     */
    def translate (sourcetree : Program, sourceFilename : String, analyser : SemanticAnalyser) : List[ClassFile] = {

        // An instruction buffer for translating statements and expressions.
        val instructions = Vector.newBuilder[JVMInstr]

        /*
         * Generate instructions by appending the operations and the associated
         * source node to the instruction buffer.
         */
        def gen (source : MiniJavaNode, ops : JVMOp*) {
            for (op <- ops)
                instructions += JVMInstr (op, source)
        }

        /*
         * Return the number of arguments that the method containing an
         * node has, or zero if the node doesn't occur in a method.
         */
        lazy val argCount : MiniJavaNode => Int =
            attr {
                case methodBody : MethodBody =>
                    methodBody.args.length
                case tree.parent (p) =>
                    argCount (p)
                case _ =>
                    0
            }

        /*
         * Counter of local variable locations used so far.
         */
        val varCounter = new Counter (0)

        /*
         * Reset the label count to zero.
         */
        def resetVarCount () {
            varCounter.reset ()
        }

        /*
         * The JVM local variable location number to use for the given variable.
         * or argument entity. Arguments also use local variable slots, so these
         * location numbers are offset after the argument slots. Thus, a location
         * number of m from this attribute means use slot m + n where n is the
         * number of arguments to the method in which the local is declared.
         */
        lazy val varnum : VariableEntity => Int =
            attr {
                case v =>
                    varCounter.next ()
            }

        /*
         * Return the local variable number to use for the given identifier
         * use, which can be assumed to be a use of a method argument or a
         * local variable.
         */
        def locnum (idnuse : IdnUse) : Int = {
            val numargs = argCount (idnuse)
            analyser.entity (idnuse) match {
                case ArgumentEntity (decl) =>
                    tree.index (decl)

                case varEntity : VariableEntity =>
                    numargs + varnum (varEntity)

                case _ =>
                    // Not reached
                    sys.error (s"locnum: non-argument/variable found: $idnuse")
            }
        }

        /*
         * Counter of labels used so far.
         */
        val labelCounter = new Counter (0)

        /*
         * Reset the label count to zero.
         */
        def resetLabelCount () {
            labelCounter.reset ()
        }

        /*
         * Allocate a new unique label and return it. The labels will comprise
         * the string "L" followed by a unique count.
         */
        def makeLabel () : String = {
            s"L${labelCounter.next ()}"
        }

        /*
         * Translate the main class.
         */
        def translateMainClass (m : MainClass) : ClassFile = {

            // Translate the main class's statement
            instructions.clear ()
            translateStmt (m.main.stmt)
            gen (m.main.stmt, Return ())
            val instrs = instructions.result

            // Make a main method containing the statement from this class
            val mainMethod =
                JVMMethod (m.main,
                           JVMMethodSpec ("main",
                                          List (JVMArrayType (JVMStringType ())),
                                          JVMVoidType ()),
                           true,
                           instrs)

            ClassFile (m, sourceFilename, m.name.idn, "java/lang/Object",
                       Nil, List (mainMethod))

        }

        /*
         * Translate a type into a JVM type.
         */
        def translateType (tipe : Type) : JVMType =
            tipe match {
                case BooleanType ()           => JVMBooleanType ()
                case IntType ()               => JVMIntType ()
                case IntArrayType ()          => JVMArrayType (JVMIntType ())
                case ClassType (IdnUse (idn)) => JVMClassType (idn)
            }

        /*
         * Translate the fields of a class.
         */
        def translateFields (fieldVars : List[Field]) : List[JVMField] =
            fieldVars.map {
                case Field (tipe, IdnDef (idn)) =>
                    JVMField (idn, translateType (tipe))
            }

        /*
         * The method spec of a method comprising the string name of the
         * method, a list of its argument JVM types and its return JVM
         * type.
         */
        def methodSpec (className : String, method : Method) : JVMMethodSpec = {
            val argTypes = method.body.args.map {
                               case Argument (tipe, _) =>
                                    translateType (tipe)
                           }
            val retType = translateType (method.body.tipe)
            val prefix = if (className == "") "" else s"$className/"
            JVMMethodSpec (prefix+ method.name.idn, argTypes, retType)
        }

        /*
         * Translate a single method.
         */
        def translateMethod (method : Method) : JVMMethod = {

            // Start counting labels and locals again for this method
            resetLabelCount ()
            resetVarCount ()

            // Translate the method's statements
            instructions.clear ()
            method.body.optStmts.map (translateStmt)

            // Emit the code to compute the return value
            translateExp (method.body.result.exp)

            // Emit the right return instruction
            gen (method.body.result,
                 method.body.tipe match {
                     case _ : IntType | _ : BooleanType =>
                         Ireturn ()
                     case _ =>
                         Areturn ()
                 })

            // Gather all of the method's instructions
            val instrs = instructions.result

            JVMMethod (method, methodSpec ("", method), false, instrs)

        }

        /*
         * Translate the methods of a class.
         */
        def translateMethods (methods : List[Method]) : List[JVMMethod] =
            methods.map (translateMethod)

        /*
         * Translate a single normal (i.e., non-main) class.
         */
        def translateClass (cls : Class) : ClassFile = {

            // Work out the name of the superclass
            val superclassName =
                if (cls.superclass == None)
                    "java/lang/Object"
                else
                    cls.superclass.get.idn

            // Translate the fields
            val fields = translateFields (cls.body.fields)

            // Translate the methods
            val methods = translateMethods (cls.body.methods)

            ClassFile (cls, sourceFilename, cls.name.idn, superclassName,
                       fields, methods)

        }

        /*
         * Is this a type or not (i.e., a reference type)?
         */
        def isIntegerType (tipe : Type) : Boolean =
            (tipe == IntType ()) || (tipe == BooleanType ())

        /*
         * Take an identifier use and translate into either a load of the
         * field that the identifier refers to, a load of the local
         * variable that corresponds to the identifier, or a load of the
         * method argument that corresponds to the identifier, depending
         * on which of these cases apply.
         */
        def translateIdnLoad (idnuse : IdnUse) {

            def loadField (field : Field) {
                field match {
                    case tree.parent (tree.parent (cls : Class)) =>
                        val className = cls.name.idn
                        val fieldName = field.name.idn
                        gen (idnuse,
                             Aload (0),
                             GetField (s"$className/$fieldName", translateType (field.tipe)))
                }
            }

            def loadLocal (tipe : Type) {
                gen (idnuse,
                     if (isIntegerType (tipe))
                         Iload (locnum (idnuse))
                     else
                         Aload (locnum (idnuse)))
            }

            analyser.entity (idnuse) match {
                case FieldEntity (decl) =>
                    loadField (decl)
                case ArgumentEntity (decl) =>
                    loadLocal (decl.tipe)
                case VariableEntity (decl) =>
                    loadLocal (decl.tipe)
            }

        }

        /*
         * Take an identifier use and translate into either a store to the
         * field that the identifier refers to, a store to the local
         * variable that corresponds to the identifier, or a store to the
         * method argument that corresponds to the identifier, depending
         * on which of these cases apply. The expression argument is the
         * thing that computes the value to be stored.
         */
        def translateIdnStore (stmt : Statement, idnuse : IdnUse, exp : Expression) {

            def storeField (field : Field) {
                field match {
                    case tree.parent (tree.parent (cls : Class)) =>
                        val className = cls.name.idn
                        val fieldName = field.name.idn
                        gen (idnuse, Aload (0))
                        translateExp (exp)
                        gen (stmt, PutField (s"$className/$fieldName", translateType (field.tipe)))
                }
            }

            def storeLocal (tipe : Type) {
                translateExp (exp)
                gen (stmt,
                     if (isIntegerType (tipe))
                         Istore (locnum (idnuse))
                     else
                         Astore (locnum (idnuse)))
            }

            analyser.entity (idnuse) match {
                case FieldEntity (decl) =>
                    storeField (decl)
                case ArgumentEntity (decl) =>
                    storeLocal (decl.tipe)
                case VariableEntity (decl) =>
                    storeLocal (decl.tipe)
            }
        }

        /*
         * Append the translation of a statement to the instruction buffer.
         */
        def translateStmt (stmt : Statement) {
            stmt match {

                case ArrayAssign (idnuse, ind, exp) =>
                    translateIdnLoad (idnuse)
                    translateExp (ind)
                    translateExp (exp)
                    gen (exp, Iastore ())

                case Block (stmts) =>
                    stmts.map (translateStmt)

                case If (cond, stmt1, stmt2) =>
                    val label1 = makeLabel ()
                    val label2 = makeLabel ()
                    translateCond (stmt, cond, label1)
                    translateStmt (stmt1)
                    gen (stmt, Goto (label2), Label (label1))
                    translateStmt (stmt2)
                    gen (stmt, Label (label2))

                case Println (exp) =>
                    gen (stmt,
                         GetStatic ("java/lang/System/out",
                                    JVMClassType ("java/io/PrintStream")))
                    translateExp (exp)
                    gen (stmt,
                         InvokeVirtual (JVMMethodSpec ("java/io/PrintStream/println",
                                                       List (JVMIntType ()),
                                                       JVMVoidType ())))

                case VarAssign (idnuse, exp) =>
                    translateIdnStore (stmt, idnuse, exp)

                case While (cond, body) =>
                    val label1 = makeLabel ()
                    val label2 = makeLabel ()
                    gen (stmt, Label (label1))
                    translateCond (stmt, cond, label2)
                    translateStmt (body)
                    gen (stmt, Goto (label1), Label (label2))

            }
        }

        /*
         * Append the translation of a condition to the instruction buffer.
         * The label is the place to which to jump if the condition is
         * false. If the condition is true, control just falls through to
         * the next instruction after the comparison. In the translation
         * of MiniJava Booleans zero is false and one is true.
         */
        def translateCond (stmt : Statement, cond : Expression, falseLabel : String) {
            translateExp (cond)
            gen (stmt, Ifeq (falseLabel))
        }

        /*
         * Translate a call expression. First, we translate the base
         * expression on which the call is being made into code that
         * puts that object on the top of the operand stack.
         */
        def translateCall (exp : CallExp) {
            analyser.tipe (exp.base) match {

                case ReferenceType (Class (IdnDef (className), _, _)) =>
                    // We are calling via a reference, ok

                    // Generate code to put the receiving object on the stack
                    translateExp (exp.base)

                    // Convert each of the actual arguments so that they are pushed
                    exp.args.map (translateExp)

                    // Get the method that is being called
                    val MethodEntity (method) = analyser.entity (exp.name)

                    // Generate an invocation of the correct method spec
                    gen (exp, InvokeVirtual (methodSpec (className, method)))

                case tipe =>
                    sys.error (s"translateCall: non-reference base type for call: $tipe")
            }
        }

        /*
         * Append the translation of an expression to the instruction buffer.
         */
        def translateExp (exp : Expression) {
            exp match {

                case AndExp (left, right) =>
                    val label1 = makeLabel ()
                    val label2 = makeLabel ()
                    translateExp (left)
                    gen (exp, Ifeq (label1))
                    translateExp (right)
                    gen (exp, Goto (label2), Label (label1), Iconst_0 (), Label (label2))

                case callexp : CallExp =>
                    translateCall (callexp)

                case FalseExp () =>
                    gen (exp, Iconst_0 ())

                case IdnExp (idnuse) =>
                    translateIdnLoad (idnuse)

                case IndExp (base, ind) =>
                    translateExp (base)
                    translateExp (ind)
                    gen (exp, Iaload ())

                case IntExp (i) =>
                    gen (exp,
                         i match {
                             case -1 => Iconst_m1 ()
                             case  0 => Iconst_0 ()
                             case  1 => Iconst_1 ()
                             case  2 => Iconst_2 ()
                             case  3 => Iconst_3 ()
                             case  4 => Iconst_4 ()
                             case  5 => Iconst_5 ()
                             case _ =>
                                 if ((i >= -128) && (i < 128))
                                     Bipush (i)
                                 else
                                     Ldc (i)
                         })

                case LengthExp (base) =>
                    translateExp (base)
                    gen (exp, ArrayLength ())

                case LessExp (left, right) =>
                    val label1 = makeLabel ()
                    val label2 = makeLabel ()
                    translateExp (left)
                    translateExp (right)
                    gen (exp, If_icmpge (label1), Iconst_1 (), Goto (label2),
                              Label (label1), Iconst_0 (), Label (label2))

                case MinusExp (left, right) =>
                    translateExp (left)
                    translateExp (right)
                    gen (exp, Isub ())

                case NewExp (IdnUse (idn)) =>
                    gen (exp, New (idn), Dup (),
                              InvokeSpecial (JVMMethodSpec (s"$idn/<init>", Nil,
                                                            JVMVoidType ())))

                case NewArrayExp (size) =>
                    translateExp (size)
                    gen (exp, NewArray ("int"))

                case NotExp (sub) =>
                    val label1 = makeLabel ()
                    val label2 = makeLabel ()
                    translateExp (sub)
                    gen (exp, Ifeq (label1), Iconst_0 (), Goto (label2),
                              Label (label1), Iconst_1 (), Label (label2))

                case PlusExp (left, right) =>
                    translateExp (left)
                    translateExp (right)
                    gen (exp, Iadd ())

                case StarExp (left, right) =>
                    translateExp (left)
                    translateExp (right)
                    gen (exp, Imul ())

                case ThisExp () =>
                    gen (exp, Aload (0))

                case TrueExp () =>
                    gen (exp, Iconst_1 ())

            }
        }

        // Translate all of the classes and return a list of the class files
        val mainClassfile = translateMainClass (sourcetree.main)
        val otherClassfiles = sourcetree.classes.map (translateClass)
        mainClassfile +: otherClassfiles

    }

}

/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2012-2013 Anthony M Sloane, Macquarie University.
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
 * Translator from MiniJava source programs to JVM target programs.
 */
object Translator {

    import JVMTree._
    import MiniJavaTree._
    import org.kiama.attribution.Attribution.attr
    import org.kiama.util.Counter
    import SymbolTable._

    /**
     * Return a JVM class file representation that implements the given
     * MiniJava program which came from the given source file. Return
     * one class file for each class in the MiniJava program.
     */
    def translate (sourcetree : Program, sourceFilename : String, analysis : SemanticAnalysis) : List[ClassFile] = {

        // An instruction buffer for translating statements and expressions into
        val instrBuffer = new scala.collection.mutable.ListBuffer[JVMInstr] ()

        /**
         * Generate an instruction by appending it to the instruction buffer.
         */
        def gen (instr : JVMInstr) {
            instrBuffer.append (instr)
        }

        /**
         * Return the number of arguments that the method containing an
         * node has, or zero if the node doesn't occur in a method.
         */
        lazy val argCount : SourceNode => Int =
            attr {
                case n if n.isRoot =>
                    0
                case methodBody : MethodBody =>
                    methodBody.args.length
                case n =>
                    (n.parent[SourceNode])->argCount
            }

        /**
         * Counter of local variable locations used so far.
         */
        val varCounter = new Counter (0)

        /**
         * Reset the label count to zero.
         */
        def resetVarCount () {
            varCounter.reset ()
        }

        /**
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

        /**
         * Return the local variable number to use for the given identifier
         * use, which can be assumed to be a use of a method argument or a
         * local variable.
         */
        def locnum (idnuse : IdnUse) : Int = {
            val numargs = idnuse->argCount
            analysis.entity (idnuse) match {
                case ArgumentEntity (decl) =>
                    decl.index

                case varEntity : VariableEntity =>
                    numargs + varnum (varEntity)

                case _ =>
                    // Not reached
                    sys.error (s"locnum: non-argument/variable found: $idnuse")
            }
        }

        /**
         * Counter of labels used so far.
         */
        val labelCounter = new Counter (0)

        /**
         * Reset the label count to zero.
         */
        def resetLabelCount () {
            labelCounter.reset ()
        }

        /**
         * Allocate a new unique label and return it. The labels will comprise
         * the string "L" followed by a unique count.
         */
        def makeLabel () : String = {
            s"L${labelCounter.next ()}"
        }

        /**
         * Translate the main class.
         */
        def translateMainClass (m : MainClass) : ClassFile = {

            // Translate the main class's statement
            instrBuffer.clear ()
            translateStmt (m.stmt)
            gen (Return ())
            val instrs = instrBuffer.result ()

            // Make a main method containing the statement from this class
            val mainMethod =
                JVMMethod (JVMMethodSpec ("main",
                                          List (JVMArrayType (JVMStringType ())),
                                          JVMVoidType ()),
                           true,
                           instrs)

            ClassFile (sourceFilename, m.name.idn, "java/lang/Object",
                       Nil, List (mainMethod))

        }

        /**
         * Translate a type into a JVM type.
         */
        def translateType (tipe : Type) : JVMType =
            tipe match {
                case BooleanType ()           => JVMBooleanType ()
                case IntType ()               => JVMIntType ()
                case IntArrayType ()          => JVMArrayType (JVMIntType ())
                case ClassType (IdnUse (idn)) => JVMClassType (idn)
            }

        /**
         * Translate the fields of a class.
         */
        def translateFields (fieldVars : List[Field]) : List[JVMField] =
            fieldVars.map {
                case Field (tipe, IdnDef (idn)) =>
                    JVMField (idn, translateType (tipe))
            }

        /**
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

        /**
         * Translate a single method.
         */
        def translateMethod (method : Method) : JVMMethod = {

            // Start counting labels and locals again for this method
            resetLabelCount ()
            resetVarCount ()

            // Translate the method's statements
            instrBuffer.clear ()
            method.body.optStmts.map (translateStmt)

            // Emit the code to compute the return value
            translateExp (method.body.result)

            // Emit the right return instruction
            method.body.tipe match {
                case _ : IntType | _ : BooleanType =>
                    gen (Ireturn ())
                case _ =>
                    gen (Areturn ())
            }

            // Gather all of the method's instructions
            val instrs = instrBuffer.result ()

            JVMMethod (methodSpec ("", method), false, instrs)

        }

        /**
         * Translate the methods of a class.
         */
        def translateMethods (methods : List[Method]) : List[JVMMethod] =
            methods.map (translateMethod)

        /**
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

            ClassFile (sourceFilename, cls.name.idn, superclassName,
                       fields, methods)

        }

        /**
         * Is this a type or not (i.e., a reference type)?
         */
        def isIntegerType (tipe : Type) : Boolean =
            (tipe == IntType ()) || (tipe == BooleanType ())

        /**
         * Take an identifier use and translate into either a load of the
         * field that the identifier refers to, a load of the local
         * variable that corresponds to the identifier, or a load of the
         * method argument that corresponds to the identifier, depending
         * on which of these cases apply.
         */
        def translateIdnLoad (idnuse : IdnUse) {

            def loadField (field : Field) {
                val cls = field.parent[ClassBody].parent[Class]
                val className = cls.name.idn
                val fieldName = field.name.idn
                gen (Aload (0))
                gen (GetField (s"$className/$fieldName", translateType (field.tipe)))
            }

            def loadLocal (tipe : Type) {
                if (isIntegerType (tipe))
                    gen (Iload (locnum (idnuse)))
                else
                    gen (Aload (locnum (idnuse)))
            }

            analysis.entity (idnuse) match {
                case FieldEntity (decl) =>
                    loadField (decl)
                case ArgumentEntity (decl) =>
                    loadLocal (decl.tipe)
                case VariableEntity (decl) =>
                    loadLocal (decl.tipe)
            }

        }

        /**
         * Take an identifier use and translate into either a store to the
         * field that the identifier refers to, a store to the local
         * variable that corresponds to the identifier, or a store to the
         * method argument that corresponds to the identifier, depending
         * on which of these cases apply. The expression argument is the
         * thing that computes the value to be stored.
         */
        def translateIdnStore (idnuse : IdnUse, exp : Expression) {

            def storeField (field : Field) {
                val cls = field.parent[ClassBody].parent[Class]
                val className = cls.name.idn
                val fieldName = field.name.idn
                gen (Aload (0))
                translateExp (exp)
                gen (PutField (s"$className/$fieldName", translateType (field.tipe)))
            }

            def storeLocal (tipe : Type) {
                translateExp (exp)
                if (isIntegerType (tipe))
                    gen (Istore (locnum (idnuse)))
                else
                    gen (Astore (locnum (idnuse)))
            }

            analysis.entity (idnuse) match {
                case FieldEntity (decl) =>
                    storeField (decl)
                case ArgumentEntity (decl) =>
                    storeLocal (decl.tipe)
                case VariableEntity (decl) =>
                    storeLocal (decl.tipe)
            }
        }

        /**
         * Append the translation of a statement to the instruction buffer.
         */
        def translateStmt (stmt : Statement) {
            stmt match {

                case ArrayAssign (idnuse, ind, exp) =>
                    translateIdnLoad (idnuse)
                    translateExp (ind)
                    translateExp (exp)
                    gen (Iastore ())

                case Block (stmts) =>
                    stmts.map (translateStmt)

                case If (cond, stmt1, stmt2) =>
                    val label1 = makeLabel ()
                    val label2 = makeLabel ()
                    translateCond (cond, label1)
                    translateStmt (stmt1)
                    gen (Goto (label2))
                    gen (Label (label1))
                    translateStmt (stmt2)
                    gen (Label (label2))

                case Println (exp) =>
                    gen (
                        GetStatic ("java/lang/System/out",
                                   JVMClassType ("java/io/PrintStream")))
                    translateExp (exp)
                    gen (
                        InvokeVirtual (JVMMethodSpec ("java/io/PrintStream/println",
                                                      List (JVMIntType ()),
                                                      JVMVoidType ())))

                case VarAssign (idnuse, exp) =>
                    translateIdnStore (idnuse, exp)

                case While (cond, stmt) =>
                    val label1 = makeLabel ()
                    val label2 = makeLabel ()
                    gen (Label (label1))
                    translateCond (cond, label2)
                    translateStmt (stmt)
                    gen (Goto (label1))
                    gen (Label (label2))

            }
        }

        /**
         * Append the translation of a condition to the instruction buffer.
         * The label is the place to which to jump if the condition is
         * false. If the condition is true, control just falls through to
         * the next instruction after the comparison. In the translation
         * of MiniJava Booleans zero is false and one is true.
         */
        def translateCond (cond : Expression, falseLabel : String) {
            translateExp (cond)
            gen (Ifeq (falseLabel))
        }

        /**
         * Translate a call expression. First, we translate the base
         * expression on which the call is being made into code that
         * puts that object on the top of the operand stack.
         */
        def translateCall (exp : CallExp) {
            analysis.tipe (exp.base) match {

                case ReferenceType (Class (IdnDef (className), _, _)) =>
                    // We are calling via a reference, ok

                    // Generate code to put the receiving object on the stack
                    translateExp (exp.base)

                    // Convert each of the actual arguments so that they are pushed
                    exp.args.map (translateExp)

                    // Get the method that is being called
                    val MethodEntity (method) = analysis.entity (exp.name)

                    // Generate an invocation of the correct method spec
                    gen (InvokeVirtual (methodSpec (className, method)))

                case tipe =>
                    sys.error (s"translateCall: non-reference base type for call: $tipe")
            }
        }

        /**
         * Append the translation of an expression to the instruction buffer.
         */
        def translateExp (exp : Expression) {
            exp match {

                case AndExp (left, right) =>
                    val label1 = makeLabel ()
                    val label2 = makeLabel ()
                    translateExp (left)
                    gen (Ifeq (label1))
                    translateExp (right)
                    gen (Goto (label2))
                    gen (Label (label1))
                    gen (Iconst_0 ())
                    gen (Label (label2))

                case callexp : CallExp =>
                    translateCall (callexp)

                case FalseExp () =>
                    gen (Iconst_0 ())

                case IdnExp (idnuse) =>
                    translateIdnLoad (idnuse)

                case IndExp (base, ind) =>
                    translateExp (base)
                    translateExp (ind)
                    gen (Iaload ())

                case IntExp (i) =>
                    i match {
                        case -1 => gen (Iconst_m1 ())
                        case  0 => gen (Iconst_0 ())
                        case  1 => gen (Iconst_1 ())
                        case  2 => gen (Iconst_2 ())
                        case  3 => gen (Iconst_3 ())
                        case  4 => gen (Iconst_4 ())
                        case  5 => gen (Iconst_5 ())
                        case _ =>
                            if ((i >= -128) && (i < 128))
                                gen (Bipush (i))
                            else
                                gen (Ldc (i))
                    }

                case LengthExp (base) =>
                    translateExp (base)
                    gen (ArrayLength ())

                case LessExp (left, right) =>
                    val label1 = makeLabel ()
                    val label2 = makeLabel ()
                    translateExp (left)
                    translateExp (right)
                    gen (If_icmpge (label1))
                    gen (Iconst_1 ())
                    gen (Goto (label2))
                    gen (Label (label1))
                    gen (Iconst_0 ())
                    gen (Label (label2))

                case MinusExp (left, right) =>
                    translateExp (left)
                    translateExp (right)
                    gen (Isub ())

                case NewExp (IdnUse (idn)) =>
                    gen (New (idn))
                    gen (Dup ())
                    gen (InvokeSpecial (JVMMethodSpec (s"$idn/<init>", Nil,
                                                       JVMVoidType ())))

                case NewArrayExp (exp) =>
                    translateExp (exp)
                    gen (NewArray ("int"))

                case NotExp (exp) =>
                    val label1 = makeLabel ()
                    val label2 = makeLabel ()
                    translateExp (exp)
                    gen (Ifeq (label1))
                    gen (Iconst_0 ())
                    gen (Goto (label2))
                    gen (Label (label1))
                    gen (Iconst_1 ())
                    gen (Label (label2))

                case PlusExp (left, right) =>
                    translateExp (left)
                    translateExp (right)
                    gen (Iadd ())

                case StarExp (left, right) =>
                    translateExp (left)
                    translateExp (right)
                    gen (Imul ())

                case ThisExp () =>
                    gen (Aload (0))

                case TrueExp () =>
                    gen (Iconst_1 ())

            }
        }

        // Translate all of the classes and return a list of the class files
        val mainClassfile = translateMainClass (sourcetree.main)
        val otherClassfiles = sourcetree.classes.map (translateClass)
        mainClassfile :: otherClassfiles

    }

}

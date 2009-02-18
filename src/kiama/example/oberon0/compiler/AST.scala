/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009 Anthony M Sloane, Macquarie University.
 *
 * Contributed by Ben Mockler.
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
 
package kiama.example.oberon0.compiler

import kiama.attribution.Attribution._

/**
 * Abstract syntax for Oberon-0.
 */
object AST {

    import TypeAnalysis._

    /**
     * Simple interface for pretty-printing capabilities.
     */
    trait PrettyPrintable {

        /**
         * Print tabs
         */
        def printTabs (o : StringBuilder, indent : Int) {
            var i: Int = 0

            while (i < indent) {
                o.append ("    ")
                i = i + 1
            }
        }

        /**
         * Pretty-print a list
         */
        def printList (o : StringBuilder, indent : Int, lst : List[PrettyPrintable], heading : String) {
            if (!lst.isEmpty) {
                printTabs (o, indent)
                o.append (heading)
                o.append ("List(\n")
                lst.foreach(obj => obj.pretty(o, indent + 3))
                printTabs (o, indent + 2)
                o.append (")\n")
            }
        }

        /**
         * Pretty-print the object at the end of the given string builder.
         */
        def pretty (o : StringBuilder, indent : Int) {
            printTabs (o, indent)
            o.append (this)
        }
    }

    abstract class Exp extends Attributable with PrettyPrintable

    case class Ident (name: String) extends Exp
    case class FieldDesig (left : Exp, id : Ident) extends Exp
    case class ArrayDesig (left : Exp, exp : Exp) extends Exp

    abstract class Literal extends Exp
    case class IntegerLiteral (num: Int) extends Literal

    abstract class UnaryNumExp (e : Exp) extends Exp {
        def getExp = e
        val op : Int => Int
    }

    case class Pos (e : Exp) extends UnaryNumExp (e) {
        override val op = {x : Int => x}
    }

    case class Neg (e : Exp) extends UnaryNumExp (e) {
        override val op = {x : Int => -x}
    }

    abstract class BinaryNumExp (l : Exp, r : Exp) extends Exp {
        def getLeft = l
        def getRight = r
        val op : (Int, Int) => Int
    }

    case class Mult (l : Exp, r : Exp) extends BinaryNumExp (l, r) {
        override val op = {(x : Int, y : Int) => x * y}
    }
    
    case class Div (l : Exp, r : Exp) extends BinaryNumExp (l, r) {
        override val op = {(x : Int, y : Int) => x / y}
    }

    case class Mod (l : Exp, r : Exp) extends BinaryNumExp (l, r) {
        override val op = {(x : Int, y : Int) => x % y}
    }

    case class Plus (l : Exp, r : Exp) extends BinaryNumExp (l, r) {
        override val op = {(x : Int, y : Int) => x + y}
    }

    case class Minus (l : Exp, r : Exp) extends BinaryNumExp (l, r) {
        override val op = {(x : Int, y : Int) => x - y}
    }

    case class Not (e : Exp) extends Exp

    abstract class BinaryBoolExp (l : Exp, r : Exp) extends Exp
    {
        def getLeft = l
        def getRight = r
    }
 
    case class And (l : Exp, r : Exp) extends BinaryBoolExp (l, r)
    case class Or (l : Exp, r : Exp) extends BinaryBoolExp (l, r)
    case class Equal (l : Exp, r: Exp) extends BinaryBoolExp (l, r)
    case class NotEqual (l : Exp, r: Exp) extends BinaryBoolExp (l, r)
    case class LessThan (l : Exp, r: Exp) extends BinaryBoolExp (l, r)
    case class LessThanOrEqual (l : Exp, r : Exp) extends BinaryBoolExp (l, r)
    case class GreaterThan (l : Exp, r : Exp) extends BinaryBoolExp (l, r)
    case class GreaterThanOrEqual (l : Exp, r : Exp) extends BinaryBoolExp (l, r)

    abstract class Statement extends Attributable with PrettyPrintable {
        override def pretty (o : StringBuilder, indent : Int) {
            super.pretty (o, indent)
            o.append ("\n")
        }
    }

    case class Assignment (desig : Exp, exp : Exp) extends Statement
    case class ProcedureCall (desig : Exp, aps : List[Exp]) extends Statement
    
    case class IfStatement (condexp : Exp, thenstmts : List[Statement], elsestmts: List[Statement]) extends Statement {
        override def pretty (o : StringBuilder, indent : Int) {
            printTabs (o, indent)
            o.append ("IfStatement(condexp = ")
            condexp.pretty (o, 0)
            o.append ("\n")

            printList (o, indent + 1, thenstmts, "thenstmts = ")
            printList (o, indent + 1, elsestmts, "elsestmts = ")

            printTabs (o, indent)
            o.append (" )\n")
        }
    }

    case class WhileStatement (condexp : Exp, bodystmts : List[Statement]) extends Statement

    abstract class Declaration (id: Ident) extends Attributable with PrettyPrintable {
        def getId = id 

        var byteOffset : Int = _

        override def pretty (o : StringBuilder, indent : Int) {
            super.pretty(o, indent)
            o.append("\n")
        }
    }

    case class ConstDecl (id : Ident, constval : Exp) extends Declaration (id)
    case class VarDecl (id : Ident, tp : Type) extends Declaration (id)
    case class RefVarDecl (id : Ident, tp : Type) extends Declaration (id)
    case class TypeDecl (id : Ident, tp : Type) extends Declaration (id)
    case class FieldDecl (id : Ident, tp : Type) extends Declaration (id)

    case class ModuleDecl (id : Ident, decls : List[Declaration], stmts : List[Statement], id2 : Ident, tp : ModuleType) extends Declaration (id) {
        override def pretty (o : StringBuilder, indent : Int) {
            o.append ("ModuleDecl(id = ")
            id.pretty (o, 0)
            o.append ("\n")

            printList (o, indent + 1, decls, "decls = ")
            printList (o, indent + 1, stmts, "stmts = ")

            printTabs (o, indent + 1)
            o.append ("id2 = ")
            id2.pretty (o, 0)
            o.append (" )\n")
        }
    }

    case class ProcDecl (id : Ident, fps : List[Declaration], decls : List[Declaration], stmts : List[Statement], id2 : Ident, tp: ProcType) extends Declaration (id) {
        override def pretty (o : StringBuilder, indent : Int) {
            printTabs (o, indent)
            o.append ("ProcDecl(id = ")
            id.pretty (o, 0)
            o.append ("\n")

            printList (o, indent + 1, fps, "fps = ")
            printList (o, indent + 1, decls, "decls = ")
            printList (o, indent + 1, stmts, "stmts = ")

            printTabs (o, indent + 1)
            o.append ("id2 = ")
            id2.pretty (o, 0)
            o.append (" )\n")
        }
    }

    case class UnknownDecl (id : Ident) extends Declaration(id)

    abstract class Type extends Attributable with PrettyPrintable

    case class NamedType (id : Ident) extends Type
    case class ArrayType (size : Exp, tp : Type) extends Type
    case class RecordType (fldlst : List[FieldDecl]) extends Type
    case class ProcType (fps : List[Declaration]) extends Type
    case class ModuleType () extends Type

    case object IntegerType extends Type
    case object BooleanType extends Type
    case object InvalidType extends Type
    case object StatementType extends Type
}

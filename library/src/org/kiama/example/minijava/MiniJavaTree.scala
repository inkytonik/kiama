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
 * Module containing tree structures for representing MiniJava programs.
 */
object MiniJavaTree {

    import org.kiama.output.{Infix, LeftAssoc, NonAssoc, Prefix,
        PrettyBinaryExpression, PrettyExpression, PrettyUnaryExpression}
    import org.kiama.util.Tree
    import scala.collection.immutable.Seq

    /**
     * The common supertype of all source tree nodes.
     */
    sealed abstract class MiniJavaTree extends Tree

    /**
     * A main program consisting of a main class and a possibly empty list of
     * other classes (defines the root scope).
     */
    case class Program (main : MainClass, classes : Seq[Class]) extends MiniJavaTree

    /**
     * A main class with a given name and body given by a single statement.
     */
    case class MainClass (name : IdnDef, stmt : Statement) extends MiniJavaTree

    /**
     * A general class with a given name, optional super class, possibly empty
     * list of instance variables, and a possibly empty list of methods.
     */
    case class Class (name : IdnDef, superclass : Option[IdnUse],
                      body : ClassBody) extends MiniJavaTree

    /**
     * The body of a class.
     */
    case class ClassBody (fields : Seq[Field], methods : Seq[Method]) extends MiniJavaTree

    /**
     * A class field with a given type and name.
     */
    case class Field (tipe : Type, name : IdnDef) extends MiniJavaTree

    /**
     * A variable with a given type and name.
     */
    case class Var (tipe : Type, name : IdnDef) extends MiniJavaTree

    /**
     * A method with a given return type, name, possibly empty list of arguments,
     * possibly empty list of local variables, an optional list of statements
     * that comprise the method body, and an expression whose value is to be
     * returned by the method.
     */
    case class Method (name : IdnDef, body : MethodBody) extends MiniJavaTree

    /**
     * The body of a method.
     */
    case class MethodBody (tipe : Type, args : Seq[Argument],
                           vars : Seq[Var],
                           optStmts : Seq[Statement],
                           result : Expression) extends MiniJavaTree

    /**
     * An argument with a given type and name.
     */
    case class Argument (tipe : Type, name : IdnDef) extends MiniJavaTree

    /**
     * Common superclass for types.
     */
    abstract class Type extends MiniJavaTree

    /**
     * The basic integer type.
     */
    case class IntType () extends Type {
        override def toString () = "int"
    }

    /**
     * The basic Boolean type.
     */
    case class BooleanType () extends Type {
        override def toString () = "boolean"
    }

    /**
     * An integer array type.
     */
    case class IntArrayType () extends Type {
        override def toString () = "int[]"
    }

    /**
     * A type given by the named class.
     */
    case class ClassType (name : IdnUse) extends Type {
        override def toString () = name.idn
    }

    /**
     * Common superclass of statements.
     */
    sealed abstract class Statement extends MiniJavaTree

    /**
     * A block containing a possibly empty list of statements.
     */
    case class Block (stmts : Seq[Statement]) extends Statement

    /**
     * A conditional statement that tests the given expression, choosing `stmt1`
     * if the expression is true, otherwise choosing `stmt2`.
     */
    case class If (exp : Expression, stmt1 : Statement,
                                     stmt2 : Statement) extends Statement

    /**
     * A while loop that tests the given expression and has as body the given
     * statement.
     */
    case class While (exp : Expression, stmt : Statement) extends Statement

    /**
     * An output statement that prints the value of the given expression followed
     * by a newline.
     */
    case class Println (exp : Expression) extends Statement

    /**
     * An assignment of the value of the given expression to the variable with the
     * given name.
     */
    case class VarAssign (name : IdnUse, exp : Expression) extends Statement

    /**
     * An assignment of the value of the `exp` expression to the array element
     * of the named array whose index is given by the `ind` expression.
     */
    case class ArrayAssign (name : IdnUse, ind : Expression,
                            exp : Expression) extends Statement

    /**
     * Common superclass of expressions.
     */
    abstract class Expression extends MiniJavaTree with PrettyExpression

    /**
     * Common interface for binary expressions.
     */
    abstract class BinaryExpression (val op : String) extends Expression with PrettyBinaryExpression {
        def left : Expression
        def right : Expression
        val fixity = Infix (LeftAssoc)
    }

    /**
     * Common interface for unary expressions.
     */
    abstract class UnaryExpression (val op : String) extends Expression with PrettyUnaryExpression {
        def exp : Expression
        override val priority = 1
        val fixity = Prefix
    }

    /**
     * Boolean conjunction (AND) expression.
     */
    case class AndExp (left : Expression, right : Expression) extends BinaryExpression ("&&") {
        override val priority = 5
    }

    /**
     * Less than expression.
     */
    case class LessExp (left : Expression, right : Expression) extends BinaryExpression ("<") {
        override val priority = 4
    }

    /**
     * Addition expression.
     */
    case class PlusExp (left : Expression, right : Expression) extends BinaryExpression ("+") {
        override val priority = 3
    }

    /**
     * Subtraction expression.
     */
    case class MinusExp (left : Expression, right : Expression) extends BinaryExpression ("-") {
        override val priority = 3
    }

    /**
     * Multiplication expression.
     */
    case class StarExp (left : Expression, right : Expression) extends BinaryExpression ("*") {
        override val priority = 2
    }

    /**
     * Array index epression. Yields the value of the `ind` element of the array
     * given by `base`.
     */
    case class IndExp (base : Expression, ind : Expression) extends Expression

    /**
     * Array length expression. Yields the length of the array `base`.
     */
    case class LengthExp (base : Expression) extends Expression

    /**
     * Method call expression. Yield the value returned by the method with the
     * given name called on the object given by the `base` expression with the
     * given argument expressions.
     */
    case class CallExp (base : Expression, name : IdnUse,
                        args : Seq[Expression]) extends Expression

    /**
     * Integer value expression.
     */
    case class IntExp (value : Int) extends Expression

    /**
     * Boolean TRUE expression.
     */
    case class TrueExp () extends Expression

    /**
     * Boolean FALSE expression.
     */
    case class FalseExp () extends Expression

    /**
     * Identifier expression.
     */
    case class IdnExp (name : IdnUse) extends Expression

    /**
     * THIS expression.
     */
    case class ThisExp () extends Expression

    /**
     * Array creation expression. Yields a new integer array whose number of
     * elements is given by `exp`.
     */
    case class NewArrayExp (exp : Expression) extends Expression

    /**
     * Instance creation expression. Yields a new instance of the given
     * class type.
     */
    case class NewExp (name : IdnUse) extends Expression

    /**
     * Boolean NOT expression.
     */
    case class NotExp (exp : Expression) extends UnaryExpression ("!")

    /**
     * An identifier reference.
     */
    abstract class IdnTree extends MiniJavaTree {
        def idn : String
    }

    /**
     * A defining occurrence of an identifier.
     */
    case class IdnDef (idn : Identifier) extends IdnTree

    /**
     * An applied occurrence (use) of an identifier.
     */
    case class IdnUse (idn : Identifier) extends IdnTree

    /**
     * A representation of identifiers as strings.
     */
    type Identifier = String

}

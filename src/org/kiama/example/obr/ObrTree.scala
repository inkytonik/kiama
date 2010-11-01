/**
 * Obr language tree definition.
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

/**
 * Module containing structures for representing Obr programs.
 */
object ObrTree {

    import org.kiama.attribution.Attributable

    /**
     * Interface for all Obr tree nodes.
     */
    sealed abstract class ObrNode extends Attributable

    /**
     * An Obr program consisting of the given declarations and statements and
     * returning an integer value.  The two identifiers name the program and
     * must be the same.
     */
    case class ObrInt (idn1: Identifier, decls : List[Declaration],
                       stmts : List[Statement], idn2 : Identifier) extends ObrNode

    /**
     * Marker trait for all node types that have an entity.
     */
    trait EntityNode extends ObrNode

    /**
     * Marker trait for all expression node types that can be assigned.
     */
    trait AssignNode extends Expression with EntityNode

    /**
     * Superclass of all declaration classes.
     */
    sealed abstract class Declaration extends ObrNode with EntityNode

    /**
     * A declaration of an integer variable.
     */
    case class IntVar (idn: Identifier) extends Declaration

    /**
     * A declaration of an integer parameter.
     */
    case class IntParam (idn: Identifier) extends Declaration

    /**
     * A declaration of a Boolean variable.
     */
    case class BoolVar (idn: Identifier) extends Declaration

    /**
     * A declaration of an array variable of the given size.
     */
    case class ArrayVar (idn: Identifier, size : Int) extends Declaration

    /**
     * A declaration of a record variable with the given fields.
     */
    case class RecordVar (idn: Identifier, fields : List[Identifier]) extends Declaration

    /**
     * A declaration of an integer constant with the given value.
     */
    case class IntConst (idn: Identifier, value : Int) extends Declaration

    /**
    * Superclass of all statement classes.
    */
    sealed abstract class Statement extends ObrNode

    /**
    * A statement that evaluates its second expression and assigns it to the
     * variable or array element denoted by its first expression.
    */
    case class AssignStmt (left : AssignNode, right : Expression) extends Statement

    /**
    * A statement that exits the nearest enclosing loop.
    */
    case class ExitStmt () extends Statement

    /**
     * A statement that executes its body statements for each value of a variable in
     * a range given by its two expressions.
     */
    case class ForStmt (idn : Identifier, min : Expression, max : Expression,
                        body : List[Statement]) extends Statement with EntityNode

    /**
     * A conditional statement that evaluates a Boolean expression and, if it is
     * true, executes its first sequence of statements, and if its is false, executes
     * its second sequence of statements.
     */
    case class IfStmt (cond : Expression, thens : List[Statement],
                       elses : List[Statement]) extends Statement

    /**
     * A loop that executes forever.
     */
    case class LoopStmt (body : List[Statement]) extends Statement

    /**
     * A statement that returns a value and terminates the program.
     */
    case class ReturnStmt (value : Expression) extends Statement

    /**
     * A statement that executes its body while its expression is true.
     */
    case class WhileStmt (cond : Expression, body : List[Statement]) extends Statement

    /**
    * Superclass of all expression classes.
    */
    abstract class Expression extends ObrNode

    /**
     * An expression whose value is the logical AND of the values of two expressions.
     */
    case class AndExp (left : Expression, right : Expression) extends Expression

    /**
     * An expression whose value is an Boolean constant.
     */
    case class BoolExp (value : Boolean) extends Expression

    /**
     * An expression that compares the values of two expressions for equality.
     */
    case class EqualExp (left : Expression, right : Expression) extends Expression

    /**
     * An expression that accesses a field of a record.
     */
    case class FieldExp (idn : Identifier, field : Identifier) extends AssignNode

    /**
     * An expression that compares the values of two expressions for greater-than order.
     */
    case class GreaterExp (left : Expression, right : Expression) extends Expression

    /**
     * An expression whose value is the current value of a named variable or constant.
     */
    case class IdnExp (idn : Identifier) extends AssignNode

    /**
     * An expression that indexes an array.
     */
    case class IndexExp (idn : Identifier, indx : Expression) extends AssignNode

    /**
     * An expression whose value is an integer constant.
     */
    case class IntExp (num : Int) extends Expression

    /**
     * An expression that compares the values of two expressions for less-than order.
     */
    case class LessExp (left : Expression, right : Expression) extends Expression

    /**
     * An expression whose value is the difference between the values of
     * two expressions.
     */
    case class MinusExp (left : Expression, right : Expression) extends Expression

    /**
     * An expression whose value is the modulus of its two expressions.
     */
    case class ModExp (left : Expression, right : Expression) extends Expression

    /**
     * An expression whose value is the negation of the value of an expression.
     */
    case class NegExp (exp : Expression) extends Expression

    /**
     * An expression that compares the values of two expressions for inequality.
     */
    case class NotEqualExp (left : Expression, right : Expression) extends Expression

    /**
     * An expression whose value is the logical negation of the value of an expression.
     */
    case class NotExp (exp : Expression) extends Expression

    /**
     * An expression whose value is the logical OR of the values of two expressions.
     */
    case class OrExp (left : Expression, right : Expression) extends Expression

    /**
     * An expression whose value is the sum of the values of two expressions.
     */
    case class PlusExp (left : Expression, right : Expression) extends Expression

    /**
    * An expression whose value is the division of the values of two expressions.
    */
    case class SlashExp (left : Expression, right : Expression) extends Expression

    /**
     * An expression whose value is the product of the values of two expressions.
     */
    case class StarExp (left : Expression, right : Expression) extends Expression

    /**
    * A representation of identifiers as strings.
    */
    type Identifier = String

 }

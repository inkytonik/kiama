/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2014 Anthony M Sloane, Macquarie University.
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
package example.oberon0
package L0.source

import base.source.{Declaration, Expression, IdnDef, IdnUse, SourceTree,
    Statement}
import org.kiama.output.{Infix, LeftAssoc, NonAssoc, Prefix,
    PrettyBinaryExpression, PrettyUnaryExpression}
import scala.collection.immutable.Seq

/**
 * Constant declarations.
 */
case class ConstDecl (idndef : IdnDef, exp: Expression) extends Declaration

/**
 * Variable declarations.
 */
case class VarDecl (idndefs : Seq[IdnDef], tipe: TypeDef) extends Declaration

/**
 * Type declarations.
 */
case class TypeDecl (idndef : IdnDef, tipe: TypeDef) extends Declaration

/**
 * Non-terminal type for type definitions.
 */
abstract class TypeDef extends SourceTree

/**
 * Types defined by naming another type.
 */
case class NamedType (idnuse : IdnUse) extends TypeDef

/**
 * Assignment statements.
 */
case class Assignment (desig : Expression, exp : Expression) extends Statement

/**
 * Common interface for binary expressions.
 */
abstract class BinaryExpression extends Expression with PrettyBinaryExpression {
    def left : Expression
    def right : Expression
}

/**
 * Common interface for relational expressions.
 */
abstract class RelationalExpression (val op : String) extends BinaryExpression {
    override val priority = 4
    val fixity = Infix (NonAssoc)
}

/**
 * Equality expressions.
 */
case class EqExp (left : Expression, right : Expression) extends RelationalExpression ("=")

/**
 * Ineuality expressions.
 */
case class NeExp (left : Expression, right : Expression) extends RelationalExpression ("#")

/**
 * Less-than expressions.
 */
case class LtExp (left : Expression, right : Expression) extends RelationalExpression ("<")

/**
 * Less-than or equal expressions.
 */
case class LeExp (left : Expression, right : Expression) extends RelationalExpression ("<=")

/**
 * Greater-than expressions.
 */
case class GtExp (left : Expression, right : Expression) extends RelationalExpression (">")

/**
 * Greater-than or equal expressions.
 */
case class GeExp (left : Expression, right : Expression) extends RelationalExpression (">=")

/**
 * Common interface for sum expressions.
 */
abstract class SumExpression (val op : String) extends BinaryExpression {
    override val priority = 3
    val fixity = Infix (LeftAssoc)
}

/**
 * Addition expressions.
 */
case class AddExp (left : Expression, right : Expression) extends SumExpression ("+")

/**
 * Subtraction expressions.
 */
case class SubExp (left : Expression, right : Expression) extends SumExpression ("-")

/**
 * Or expressions.
 */
case class OrExp (left : Expression, right : Expression) extends SumExpression ("OR")

/**
 * Common interface for product expressions.
 */
abstract class ProdExpression (val op : String) extends BinaryExpression {
    override val priority = 2
    val fixity = Infix (LeftAssoc)
}

/**
 * Multiplication expressions.
 */
case class MulExp (left : Expression, right : Expression) extends ProdExpression ("*")

/**
 * Division expressions.
 */
case class DivExp (left : Expression, right : Expression) extends ProdExpression ("DIV")

/**
 * Modulus expressions.
 */
case class ModExp (left : Expression, right : Expression) extends ProdExpression ("MOD")

/**
 * And expressions.
 */
case class AndExp (left : Expression, right : Expression) extends ProdExpression ("&")

/**
 * Common interface for unary expressions.
 */
abstract class UnaryExpression extends Expression with PrettyUnaryExpression {
    def exp : Expression
    val fixity = Prefix
}

/**
 * Negation expressions.
 */
case class NegExp (exp : Expression) extends UnaryExpression {
    override val priority = 3
    val op = "-"
}

/**
 * Complement expressions.
 */
case class NotExp (exp : Expression) extends UnaryExpression {
    override val priority = 1
    val op = "~"
}

/**
 * Integer expressions.
 */
case class IntExp (v : Int) extends Expression

/**
 * Identifier expressions.
 */
case class IdnExp (idnuse : IdnUse) extends Expression

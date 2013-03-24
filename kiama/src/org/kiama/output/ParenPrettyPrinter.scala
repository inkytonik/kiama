/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2013 Anthony M Sloane, Macquarie University.
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
package output

/**
 * The sides that an expression may appear on inside another expression
 * or associativities that infix operators can have.
 */
abstract class Side

/**
 * The left side or left associativitiy of an infix operator.
 */
case object LeftAssoc extends Side

/**
 * The right side or right associativitiy of an infix operator.
 */
case object RightAssoc extends Side

/**
 * No side or non-associativitiy of an infix operator.
 */
case object NonAssoc extends Side

/**
 * The possible fixities of operators.
 */
abstract class Fixity

/**
 * The unary operator occurs in prefix position (i.e., before its operand).
 */
case object Prefix extends Fixity

/**
 * The unary operator occurs in postfix position (i.e., after its operand).
 */
case object Postfix extends Fixity

/**
 * The binary operator occurs in infix position (i.e., between its two operands).
 */
case class Infix (side : Side) extends Fixity

/**
 * Super type of all expressions that are to be pretty-printed.
 */
trait PrettyExpression

/**
 * An expression that contains an operator.  Defines `priority` to relate
 * the operator to other operators (lower number is higher priority, no
 * default). Also defines `fixity` to specify the relationship between the
 * operator and its operand(s) (no default).
 */
trait PrettyOperatorExpression extends PrettyExpression {
    def priority : Int
    def fixity : Fixity
}

/**
 * Binary expressions that are to be pretty-printed. `left` and `right`
 * give the two operand expressions and `op` the string that is to be
 * used as the output of the operator.
 */
trait PrettyBinaryExpression extends PrettyOperatorExpression {
    def left : PrettyExpression
    def op : String
    def right : PrettyExpression
}

/**
 * Unary expressions that are to be pretty-printed. `exp` gives the operand
 * expressions and `op` the string that is to be used as the output of the
 * operator.
 */
trait PrettyUnaryExpression extends PrettyOperatorExpression {
    def op : String
    def exp : PrettyExpression
}

/**
 * A pretty-printer with support for pretty-printing expressions with minimal
 * parenthesisation.
 *
 * Based on algorithm in "Unparsing expressions with prefix and postfix operators",
 * Ramsey, SP&E, 28 (12), October 1998.  We have not implemented support for
 * arbitrary arity infix operators.
 */
trait ParenPrettyPrinter {

    self : PrettyPrinter =>

    def toParenDoc (e : PrettyExpression) : Doc =
        e match {
            case b : PrettyBinaryExpression =>
                val ld =
                    b.left match {
                        case l : PrettyOperatorExpression =>
                            bracket (l, b, LeftAssoc)
                        case l =>
                            toParenDoc (l)
                    }
                val rd =
                    b.right match {
                        case r : PrettyOperatorExpression =>
                            bracket (r, b, RightAssoc)
                        case r =>
                            toParenDoc (r)
                    }
                ld <+> text (b.op) <+> rd

            case u : PrettyUnaryExpression =>
                val ed =
                    u.exp match {
                        case e : PrettyOperatorExpression =>
                            bracket (e, u, NonAssoc)
                        case e =>
                            toParenDoc (e)
                    }
                if (u.fixity == Prefix)
                    text (u.op) <> ed
                else
                    ed <> text (u.op)
        }

    /**
     * Optionally parenthesise an operator expression based on the precedence relation
     * with an outer expression's operator.
     */
    def bracket (inner : PrettyOperatorExpression, outer : PrettyOperatorExpression,
                 side : Side) : Doc = {
        val d = toParenDoc (inner)
        if (noparens (inner, outer, side)) d else parens (d)
    }

    /**
     * Return true if the inner expression should be parenthesised when appearing
     * on the given side with the outer expression.
     */
    def noparens (inner : PrettyOperatorExpression, outer : PrettyOperatorExpression,
                  side : Side) : Boolean = {
        val pi = inner.priority
        val po = outer.priority
        lazy val fi = inner.fixity
        lazy val fo = outer.fixity
        (pi < po) ||
            ((fi, side) match {
                case (Postfix, LeftAssoc) =>
                    true
                case (Prefix, RightAssoc) =>
                    true
                case (Infix (LeftAssoc), LeftAssoc) =>
                    (pi == po) && (fo == Infix (LeftAssoc))
                case (Infix (RightAssoc), RightAssoc) =>
                    (pi == po) && (fo == Infix (RightAssoc))
                case (_, NonAssoc) =>
                    fi == fo
                case _ =>
                    false
            })
    }

}

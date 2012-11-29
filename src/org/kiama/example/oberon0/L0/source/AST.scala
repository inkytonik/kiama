package org.kiama
package example.oberon0
package L0.source

import base.source.{Declaration, IdnDef, IdnUse, SourceASTNode, Statement}
import org.kiama.output.{Infix, LeftAssoc, NonAssoc, Prefix,
    PrettyBinaryExpression, PrettyExpression, PrettyUnaryExpression}

/**
 * Constant declarations.
 */
case class ConstDecl (idndef : IdnDef, exp: Expression) extends Declaration

/**
 * Variable declarations.
 */
case class VarDecl (idndefs : List[IdnDef], tipe: TypeDef) extends Declaration

/**
 * Type declarations.
 */
case class TypeDecl (idndef : IdnDef, tipe: TypeDef) extends Declaration

/**
 * Non-terminal type for type definitions.
 */
abstract class TypeDef extends SourceASTNode

/**
 * Types defined by naming another type.
 */
case class NamedType (idnuse : IdnUse) extends TypeDef

/**
 * Assignment statements.
 */
case class Assignment (desig : Expression, exp : Expression) extends Statement

/**
 * Non-terminal type for expressions.
 */
abstract class Expression extends SourceASTNode with PrettyExpression

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

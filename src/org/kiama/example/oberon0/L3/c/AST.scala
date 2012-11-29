package org.kiama
package example.oberon0
package L3.c

import base.c.{CExpression, CStatement, CType}
import L0.c.CUnaryExpression

/**
 * C void type.
 */
case class CVoidType () extends CType

/**
 * C reference type.
 */
case class CAddrType (basetype : CType) extends CType

/**
 * C call statements.
 */
case class CCall (s : String, ps : List[CExpression]) extends CStatement

/**
 * C string expressions.
 */
case class CStrExp (s : String) extends CExpression

/**
 * C address-of expressions.
 */
case class CAddrExp (exp : CExpression) extends CUnaryExpression {
    override val priority = 2
    val op = "&"
}

/**
 * C dereference expressions.
 */
case class CDerefExp (exp : CExpression) extends CUnaryExpression {
    override val priority = 2
    val op = "*"
}

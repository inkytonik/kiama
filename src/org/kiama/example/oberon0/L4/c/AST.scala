package org.kiama
package example.oberon0
package L4.c

import base.c.{CDeclaration, CExpression, CType, CVarDecl}

/**
 * C record types.
 */
case class CRecordType (fields : List[CVarDecl]) extends CType

/**
 * C array index expressions.
 */
case class CIndexExp (array : CExpression, index : CExpression) extends CExpression

/**
 * C record field access expressions.
 */
case class CFieldExp (record : CExpression, field : String) extends CExpression

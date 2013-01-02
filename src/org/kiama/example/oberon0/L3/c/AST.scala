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

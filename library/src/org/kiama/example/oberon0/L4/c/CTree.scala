/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2015 Anthony M Sloane, Macquarie University.
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

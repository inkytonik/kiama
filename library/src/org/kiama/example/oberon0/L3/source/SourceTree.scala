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
package L3.source

import base.source.{Block, Declaration, Expression, IdnDef, IdnUse, SourceTree,
    Statement}
import L0.source.TypeDef
import scala.collection.immutable.Seq

/**
 * Procedure declarations.
 */
case class ProcDecl (idndef : IdnDef, params : Seq[FPSection], body : Block,
                     idnuse : IdnUse) extends Declaration

/**
 * Non-terminal type for parameter passing modes.
 */
sealed abstract class Mode

/**
 * Pass by variable (reference) mode.
 */
case class VarMode () extends Mode

/**
 * Pass by value mode.
 */
case class ValMode () extends Mode

/**
 * Formal parameter sections.
 */
case class FPSection (mode : Mode, idndefs : Seq[IdnDef], tipe : TypeDef) extends SourceTree

/**
 * Call statements.
 */
case class Call (idnuse : IdnUse, params : Seq[Expression]) extends Statement

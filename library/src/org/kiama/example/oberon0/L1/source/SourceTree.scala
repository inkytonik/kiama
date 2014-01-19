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
package L1.source

import base.source.{Block, Expression, Statement}
import scala.collection.immutable.Seq

/**
 * Conditional statements containing a main expression and then block, zero
 * or more else if blocks, and an optional else block.
 */
case class IfStatement (cond : Expression, block : Block,
                        elsifs : Seq[(Expression,Block)],
                        optelse : Option[Block]) extends Statement

/**
 * While statements.
 */
case class WhileStatement (cond : Expression, block : Block) extends Statement

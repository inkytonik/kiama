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
package L2.source

import base.source.{Block, Expression, SourceTree, Statement}
import L0.source.IdnExp
import scala.collection.immutable.Seq

/**
 * FOR statements.
 */
case class ForStatement (idn : IdnExp, lower : Expression,
                         upper : Expression, by : Option[Expression],
                         block : Block) extends Statement

/**
 * Case statements.
 */
case class CaseStatement (exp : Expression, cases : Seq[Case],
                          optelse : Option[Block]) extends Statement

/**
 * A single case of a case statement.
 */
case class Case (conds : Seq[Condition], block : Block) extends Statement

/**
 * Non-terminal type for case conditions.
 */
abstract class Condition extends SourceTree

/**
 * A condition that matches a single value.
 */
case class ValCond (value : Expression) extends Condition

/**
 * A condition that matches a range of values, inclusive of the minimum
 * and maximum.
 */
case class MinMaxCond (minvalue : Expression, maxvalue : Expression) extends Condition

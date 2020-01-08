/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package L2.source

import base.source.{Block, Expression, SourceNode, Statement}
import L0.source.IdnExp

/**
 * FOR statements.
 */
case class ForStatement(idn : IdnExp, lower : Expression,
    upper : Expression, by : Option[Expression],
    block : Block) extends Statement

/**
 * Case statements.
 */
case class CaseStatement(exp : Expression, cases : Vector[Case],
    optelse : Option[Block]) extends Statement

/**
 * A single case of a case statement.
 */
case class Case(conds : Vector[Condition], block : Block) extends Statement

/**
 * Non-terminal type for case conditions.
 */
abstract class Condition extends SourceNode

/**
 * A condition that matches a single value.
 */
case class ValCond(value : Expression) extends Condition

/**
 * A condition that matches a range of values, inclusive of the minimum
 * and maximum.
 */
case class MinMaxCond(minvalue : Expression, maxvalue : Expression) extends Condition

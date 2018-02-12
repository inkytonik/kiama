/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package L1.source

import base.source.{Block, Expression, Statement}

/**
 * Conditional statements containing a main expression and then block, zero
 * or more else if blocks, and an optional else block.
 */
case class IfStatement(cond : Expression, block : Block,
    elsifs : Vector[(Expression, Block)],
    optelse : Option[Block]) extends Statement

/**
 * While statements.
 */
case class WhileStatement(cond : Expression, block : Block) extends Statement

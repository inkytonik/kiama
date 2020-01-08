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
package L1.c

import base.c.{CExpression, CStatement}

/**
 * C one-sided conditional expressions.
 */
case class CIfStatement(cond : CExpression, tstmt : CStatement) extends CStatement

/**
 * C two-sided conditional expressions.
 */
case class CIfElseStatement(cond : CExpression, tstmt : CStatement, estmt : CStatement) extends CStatement

/**
 * C while statements.
 */
case class CWhileStatement(cond : CExpression, stmt : CStatement) extends CStatement

/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package L4.c

import base.c.{CExpression, CType, CVarDecl}

/**
 * C record types.
 */
case class CRecordType(fields : Vector[CVarDecl]) extends CType

/**
 * C array index expressions.
 */
case class CIndexExp(array : CExpression, index : CExpression) extends CExpression

/**
 * C record field access expressions.
 */
case class CFieldExp(record : CExpression, field : String) extends CExpression

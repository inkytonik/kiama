/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package L3.source

import base.source.{
    Block,
    Declaration,
    Expression,
    IdnDef,
    IdnUse,
    SourceNode,
    Statement
}
import L0.source.TypeDef

/**
 * Procedure declarations.
 */
case class ProcDecl(idndef : IdnDef, params : Vector[FPSection], body : Block,
    idnuse : IdnUse) extends Declaration

/**
 * Non-terminal type for parameter passing modes.
 */
sealed abstract class Mode extends SourceNode

/**
 * Pass by variable (reference) mode.
 */
case class VarMode() extends Mode

/**
 * Pass by value mode.
 */
case class ValMode() extends Mode

/**
 * Formal parameter sections.
 */
case class FPSection(mode : Mode, idndefs : Vector[IdnDef], tipe : TypeDef) extends SourceNode

/**
 * Call statements.
 */
case class Call(idnuse : IdnUse, params : Vector[Expression]) extends Statement

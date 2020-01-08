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
package L4.source

import base.source.{Expression, SourceNode}
import L0.source.TypeDef

/**
 * Array type definitions.
 */
case class ArrayTypeDef(size : Expression, tipe : TypeDef) extends TypeDef

/**
 * Array index expressions.
 */
case class IndexExp(base : Expression, exp : Expression) extends Expression

/**
 * Record type definitions.
 */
case class RecordTypeDef(fields : Vector[Fields]) extends TypeDef

/**
 * Record fields.
 */
case class Fields(idndefs : Vector[String], tipe : TypeDef) extends SourceNode

/**
 * Record field access expressions.
 */
case class FieldExp(base : Expression, fieldname : FieldIdn) extends Expression

/**
 * Field identifier. We don't use IdnDef for fields, since we don't perform
 * the same kind of name analysis on them. Fields only need to be looked up
 * in the appropriate record type, not have the full scope handling performed.
 */
case class FieldIdn(ident : String) extends SourceNode

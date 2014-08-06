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
package L4.source

import base.source.{Expression, SourceNode}
import L0.source.TypeDef
import scala.collection.immutable.Seq

/**
 * Array type definitions.
 */
case class ArrayTypeDef (size : Expression, tipe: TypeDef) extends TypeDef

/**
 * Array index expressions.
 */
case class IndexExp (base : Expression, exp : Expression) extends Expression

/**
 * Record type definitions.
 */
case class RecordTypeDef (fields : Seq[FieldList]) extends TypeDef

/**
 * Record field lists.
 */
case class FieldList (idndefs : Seq[String], tipe : TypeDef) extends SourceNode

/**
 * Record field access expressions.
 */
case class FieldExp (base : Expression, fieldname : FieldIdn) extends Expression

/**
 * Field identifier. We don't use IdnDef for fields, since we don't perform
 * the same kind of name analysis on them. Fields only need to be looked up
 * in the appropriate record type, not have the full scope handling performed.
 */
case class FieldIdn (ident : String) extends SourceNode

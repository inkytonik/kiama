package org.kiama
package example.oberon0
package L4.source

import base.source.SourceASTNode
import L0.source.{Expression, TypeDef}

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
case class RecordTypeDef (fields : List[FieldList]) extends TypeDef

/**
 * Record field lists.
 */
case class FieldList (idndefs : List[String], tipe : TypeDef) extends SourceASTNode

/**
 * Record field access expressions.
 */
case class FieldExp (base : Expression, fieldname : FieldIdn) extends Expression

/**
 * Field identifier. We don't use IdnDef for fields, since we don't perform
 * the same kind of name analysis on them. Fields only need to be looked up
 * in the appropriate record type, not have the full scope handling performed.
 */
case class FieldIdn (ident : String) extends SourceASTNode

package org.kiama
package example.oberon0
package L3.source

import base.source.{Block, Declaration, IdnDef, IdnUse, SourceASTNode,
    Statement}
import L0.source.{Expression, TypeDef}

/**
 * Procedure declarations.
 */
case class ProcDecl (idndef : IdnDef, params : List[FPSection], body : Block,
                     idnuse : IdnUse) extends Declaration

/**
 * Non-terminal type for parameter passing modes.
 */
sealed abstract class Mode

/**
 * Pass by variable (reference) mode.
 */
case class VarMode () extends Mode

/**
 * Pass by value mode.
 */
case class ValMode () extends Mode

/**
 * Formal parameter sections.
 */
case class FPSection (mode : Mode, idndefs : List[IdnDef], tipe : TypeDef) extends SourceASTNode

/**
 * Call statements.
 */
case class Call (idnuse : IdnUse, params : List[Expression]) extends Statement

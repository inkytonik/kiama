package org.kiama
package example.oberon0
package base.source

import org.kiama.attribution.Attributable
import scala.util.parsing.input.Positional

/**
 * Root type of all source abstract syntax tree nodes.
 */
abstract class SourceASTNode extends Attributable with Positional

/**
 * Non-terminal type for declarations.
 */
abstract class Declaration extends SourceASTNode

/**
 * Module declarations.
 */
case class ModuleDecl (idndef : IdnDef, block : Block, idnuse : IdnUse) extends SourceASTNode

/**
 * Non-terminal type for statements.
 */                       
abstract class Statement extends SourceASTNode

/**
 * Block of declarations and statements.
 */
case class Block (decls : List[Declaration], stmts: List[Statement]) extends Statement

/**
 * Empty statements.
 */
case class EmptyStmt () extends Statement

/**
 * Common interface for all identifier occurrences.
 */
abstract class Identifier extends SourceASTNode {
    def ident : String
}

/**
 * Defining occurrences of identifiers
 */
case class IdnDef (ident : String) extends Identifier

/**
 * Applied occurrences (uses) of identifiers.
 */
case class IdnUse (ident : String) extends Identifier

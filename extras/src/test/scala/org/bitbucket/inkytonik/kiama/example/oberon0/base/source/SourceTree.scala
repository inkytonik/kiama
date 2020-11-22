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
package base.source

import org.bitbucket.inkytonik.kiama.output.PrettyExpression

/**
 * Module for common source tree definitions.
 */
object SourceTree {

    import org.bitbucket.inkytonik.kiama.relation.Tree

    /**
     * Type for Oberon0 program trees.
     */
    type SourceTree = Tree[SourceNode, ModuleDecl]

}

/**
 * Root type of all source abstract syntax tree nodes.
 */
abstract class SourceNode extends Product

/**
 * Non-terminal type for declarations.
 */
abstract class Declaration extends SourceNode

/**
 * Module declarations.
 */
case class ModuleDecl(idndef : IdnDef, block : Block, idnuse : IdnUse) extends SourceNode

/**
 * Non-terminal type for statements.
 */
abstract class Statement extends SourceNode

/**
 * Block of declarations and statements.
 */
case class Block(decls : Vector[Declaration], stmts : Vector[Statement]) extends Statement

/**
 * Empty statements.
 */
case class EmptyStmt() extends Statement

/**
 * Non-terminal type for expressions.
 */
abstract class Expression extends SourceNode with PrettyExpression

/**
 * Common interface for all identifier occurrences.
 */
sealed abstract class Identifier extends SourceNode {
    def ident : String
}

/**
 * Defining occurrences of identifiers
 */
case class IdnDef(ident : String) extends Identifier

/**
 * Applied occurrences (uses) of identifiers.
 */
case class IdnUse(ident : String) extends Identifier

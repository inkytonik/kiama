package org.kiama
package example.oberon0
package L1.source

import base.source.{Block, Statement}
import L0.source.Expression

/**
 * Conditional statements containing a main expression and then block, zero
 * or more else if blocks, and an optional else block.
 */
case class IfStatement (cond : Expression, block : Block,
                        elsifs : List[(Expression,Block)],
                        optelse : Option[Block]) extends Statement

/**
 * While statements.
 */
case class WhileStatement (cond : Expression, block : Block) extends Statement

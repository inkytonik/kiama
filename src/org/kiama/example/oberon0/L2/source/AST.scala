package org.kiama
package example.oberon0
package L2.source

import base.source.{Block, SourceASTNode, Statement}
import L0.source.{Expression, IdnExp}

/**
 * FOR statements.
 */
case class ForStatement (idn : IdnExp, lower : Expression,
                         upper : Expression, by : Option[Expression],
                         block : Block) extends Statement

/**
 * Case statements.
 */
case class CaseStatement (exp : Expression, cases : List[Case],
                          optelse : Option[Block]) extends Statement
                          
/**
 * A single case of a case statement.
 */
case class Case (conds : List[Condition], block : Block) extends Statement

/**
 * Non-terminal type for case conditions.
 */
abstract class Condition extends SourceASTNode

/**
 * A condition that matches a single value.
 */
case class ValCond (value : Expression) extends Condition

/**
 * A condition that matches a range of values, inclusive of the minimum
 * and maximum.
 */
case class MinMaxCond (minvalue : Expression, maxvalue : Expression) extends Condition

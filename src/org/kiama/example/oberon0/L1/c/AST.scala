package org.kiama
package example.oberon0
package L1.c

import base.c.{CExpression, CStatement}

/**
 * C one-sided conditional expressions.
 */
case class CIfStatement (cond : CExpression, tstmt : CStatement) extends CStatement

/**
 * C two-sided conditional expressions.
 */
case class CIfElseStatement (cond : CExpression, tstmt : CStatement, estmt : CStatement) extends CStatement

/**
 * C while statements.
 */
case class CWhileStatement (cond : CExpression, stmt : CStatement) extends CStatement

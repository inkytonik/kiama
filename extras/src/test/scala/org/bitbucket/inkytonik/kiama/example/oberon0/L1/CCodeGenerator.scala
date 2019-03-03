/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package L1

/**
 * C Code generator for the L1 language.
 */
trait CCodeGenerator extends L0.CCodeGenerator {

    import base.c.CStatement
    import base.source.{Block, Expression, Statement}
    import c.{CIfElseStatement, CIfStatement, CWhileStatement}
    import source.{IfStatement, WhileStatement}

    /**
     * Add translation of IF and WHILE statements.
     */
    override def translate(s : Statement) : CStatement =
        s match {
            case IfStatement(c, ss, eis, oe) =>
                translate((c, ss) +: eis, oe)

            case WhileStatement(c, b) =>
                CWhileStatement(translate(c), translate(b))

            case _ =>
                super.translate(s)
        }

    /**
     * Translation of expression, block pairs from an IF statement into
     * cascading C IFs.
     */
    def translate(eis : Vector[(Expression, Block)], oe : Option[Block]) : CStatement = {
        val (e, ss) = eis.last
        val te = translate(e)
        val tss = translate(ss)
        val tail = oe.map(b => CIfElseStatement(te, tss, translate(b))).getOrElse(CIfStatement(te, tss))
        eis.init.foldRight(tail) {
            case ((e, ss), s) =>
                CIfElseStatement(translate(e), translate(ss), s)
        }
    }

}

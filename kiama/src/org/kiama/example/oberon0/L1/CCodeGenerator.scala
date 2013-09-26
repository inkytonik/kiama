/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2013 Anthony M Sloane, Macquarie University.
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
package L1

/**
 * C Code generator for the L1 language.
 */
trait CCodeGenerator extends L0.CCodeGenerator {

    import base.c.CStatement
    import base.source.{Statement, Block}
    import c.{CIfElseStatement, CIfStatement, CWhileStatement}
    import L0.source.Expression
    import source.{IfStatement, WhileStatement}

    /**
     * Add translation of IF and WHILE statements.
     */
    override def translate (s : Statement) : CStatement =
        s match {
            case IfStatement (c, ss, eis, oe) =>
                translate ((c,ss) :: eis, oe)

            case WhileStatement (c, b) =>
                CWhileStatement (translate (c), translate (b))

            case _ =>
                super.translate (s)
        }

    /**
     * Translation of expression, block pairs from an IF statement into
     * cascading C IFs.
     */
    def translate (eis : List[(Expression,Block)], oe : Option[Block]) : CStatement = {
        val (e, ss) = eis.last
        val te = translate (e)
        val tss = translate (ss)
        val tail = oe.map (b => CIfElseStatement (te, tss, translate (b))).getOrElse (CIfStatement (te, tss))
        eis.init.foldRight (tail) {
            case ((e, ss), s) =>
                CIfElseStatement (translate (e), translate (ss), s)
        }
    }

}

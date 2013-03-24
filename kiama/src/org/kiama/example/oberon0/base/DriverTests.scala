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
package base

import org.kiama.util.TestCompiler
import source.ModuleDecl

/**
 * A driver for testing.
 */
trait TestDriver extends TestCompiler[ModuleDecl] {

    this : Driver =>

    import scala.collection.mutable.ListBuffer

    /**
     * The language level of this program.  The levels are:
     *   base: basic module structure; empty statements
     *   L0: const, var, type decls; basic types; expressions; assignment stmts
     *   L1: if and while statements
     *   L2: for and case statements
     *   L3: procedures, only local access to variables
     *   L4: arrays and records
     *   L5: L4 with unrestricted access to variables (FIXME: not implemented)
     */
    def langlevel : Int

    /**
     * The maximum language level we support.
     */
    val maxlanglevel = 5

    /**
     * The highest task level of this program.  The levels are:
     *    1 - parsing and pretty printing (LDTA 1)
     *    2 - name analysis (LDTA 2)
     *    3 - type analysis (LDTA 3)
     *    4 - desugar (LDTA 4a)
     *    5 - optimisation (LDTA 4b) (FIXME: not implemented)
     *    6 - C code gen (LDTA 5a)
     */
    def tasklevel : Int

    /**
     * Make the tests for a given language subset. proglang denotes the
     * language subset whose tests are used.
     */
    def mktests (proglang : String) {
        val name = "Oberon0 testing " + artefact + " on " + proglang + " tests"
        val path = "kiama/src/org/kiama/example/oberon0/" + proglang + "/tests"
        filetests (name, path, ".ob", ".out")
    }

    // Actually create the tests, always including base and LO, then the other
    // levels if the driver supports at least that language level.
    mktests ("base")
    mktests ("L0")
    if (langlevel > 0) mktests ("L1")
    if (langlevel > 1) mktests ("L2")
    if (langlevel > 2) mktests ("L3")
    if (langlevel > 3) mktests ("L4")
    if (langlevel > 4) mktests ("L5")

    /**
     * Sanitise the output from a test.  Remove any output that doesn't
     * make sense to this program.  I.e., if we are running a program that
     * performs tasks 1-m, then any lines marked with [p], where p > m
     * should be removed before comparison.  Also, there can be three
     * numbers, a'la [p,q,r] where p is as before and q (r) are lower
     * (upper) inclusive bounds on the language level to which this output
     * should apply. If p is omitted, the output applies to all task levels.
     * If p is given, but q and r are omitted, the output also applies to
     * all language levels.
     */
    override def sanitise (s : String) : String = {
        val b = new ListBuffer[String]

        // Pattern for a line marked with just p
        val MarkedLine1 = """\[([0-9]+)\](.*)""".r

        // Pattern for a line marks with p, q and r
        val MarkedLine2 = """\[([0-9]+),([0-9]+),([0-9]+)\](.*)""".r

        /**
         * Include line in the output if it meets the criteria.
         */
        def processline (line : String, p : Int, q : Int = 0, r : Int = maxlanglevel) {
            if ((p <= tasklevel) && (langlevel >= q) && (langlevel <= r))
                b += line
        }

        // Iterate over all possible output lines, checking them if they are
        // marked. Unmarked lines are always included.
        for (t <- s.lines) {
            t match {
                case MarkedLine1 (ps, line) =>
                    processline (line, ps.toInt)
                case MarkedLine2 (ps, qs, rs, line) =>
                    processline (line, ps.toInt, qs.toInt, rs.toInt)
                case _ =>
                    b += t
            }
        }

        // Return the selected lines
        b.result ().mkString ("\n")
    }

    // Always pretty-print AST
    override val pprintastFlagDefault = true

}

/**
 * Driver for testing a translator.
 */
trait TranslatingTestDriver extends TestDriver {

    this : TranslatingDriver =>

    // Always pretty-print C AST
    override val pprintcastFlagDefault = true

}

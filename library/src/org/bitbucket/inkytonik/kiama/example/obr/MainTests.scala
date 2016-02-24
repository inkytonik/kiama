/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2016 Anthony M Sloane, Macquarie University.
 * Copyright (C) 2010-2016 Dominic Verity, Macquarie University.
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

package org.bitbucket.inkytonik.kiama
package example.obr

import ObrTree.ObrInt
import org.bitbucket.inkytonik.kiama.util.TestCompilerWithConfig

/**
 * A driver which compiles a file and allows a test to be run on the resulting
 * target tree.
 */
trait TreeTestDriver extends Driver with TestCompilerWithConfig[ObrInt, ObrConfig] {

    import ObrTree.ObrTree
    import RISCTree._
    import org.bitbucket.inkytonik.kiama.example.obr.RISCTransformer
    import org.bitbucket.inkytonik.kiama.util.{Config, Emitter, FileSource}
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._

    /**
     * Method to compile an Obr program and to apply a specified test to
     * the resulting target tree.
     */
    def targettreetest(name : String, dirname : String, obrfile : String,
        tester : (String, Emitter, RISCNode) => Unit) {
        val title = s"$name processing $obrfile"

        test(title) {
            val filename = dirname + obrfile
            val config = createAndInitConfig(Array(filename))
            val source = FileSource(filename)
            makeast(source, config) match {
                case Left(ast) =>
                    val tree = new ObrTree(ast)
                    val analyser = new SemanticAnalyser(tree)
                    val messages = analyser.errors
                    messages shouldBe empty
                    val labels = new RISCLabels
                    val transformer = new RISCTransformer(analyser, labels)
                    tester(title, config.error, transformer.code(ast))
                case Right(msgs) =>
                    msgs shouldBe empty
            }
        }
    }

    /**
     * Test a target tree by collecting together its IntDatum leaves and checking the resulting
     * sequence of integers to see if it contains an expected sequence of integers.
     */
    def checkintdatums(expected : List[Int])(title : String, emitter : Emitter, code : RISCNode) {
        val realised = List.newBuilder[Int]
        bottomup(attempt(query[RISCNode] {
            case IntDatum(num) =>
                realised += num
            case n : RISCProg =>
                realised.result shouldBe expected
        }))(code)
    }

}

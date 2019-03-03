/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2019 Anthony M Sloane, Macquarie University.
 * Copyright (C) 2010-2019 Dominic Verity, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
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
    import org.bitbucket.inkytonik.kiama.util.{Emitter, FileSource}
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._

    /**
     * Method to compile an Obr program and to apply a specified test to
     * the resulting target tree.
     */
    def targettreetest(name : String, relDirname : String, obrfile : String,
        tester : (String, Emitter, RISCNode) => Unit) {
        val title = s"$name processing $obrfile"
        val dirname = "src/test/scala/org/bitbucket/inkytonik/kiama/" + relDirname

        test(title) {
            val filename = dirname + obrfile
            createAndInitConfig(Array(filename)) match {
                case Left(message) =>
                    fail(message)
                case Right(config) =>
                    val source = FileSource(filename)
                    makeast(source, config) match {
                        case Left(ast) =>
                            val tree = new ObrTree(ast)
                            val analyser = new SemanticAnalyser(tree)
                            val messages = analyser.errors
                            messages shouldBe empty
                            val labels = new RISCLabels
                            val transformer = new RISCTransformer(analyser, labels)
                            tester(title, config.output(), transformer.code(ast))
                        case Right(msgs) =>
                            msgs shouldBe empty
                    }
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

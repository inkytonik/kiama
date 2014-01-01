/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2014 Anthony M Sloane, Macquarie University.
 * Copyright (C) 2010-2014 Dominic Verity, Macquarie University.
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
package example.obr

import ObrTree.ObrInt
import org.kiama.util.TestCompilerWithConfig

/**
 * A driver which compiles a file and allows a test to be run on the resulting
 * target tree.
 */
class TreeTestDriver extends Driver with TestCompilerWithConfig[ObrInt,ObrConfig] {

    import RISCTree._
    import org.kiama.attribution.Attribution.initTree
    import org.kiama.example.obr.RISCTransformation
    import org.kiama.util.{Config, Emitter, Messaging}
    import org.kiama.util.IO.{filereader, FileNotFoundException}
    import org.kiama.rewriting.Rewriter._
    import scala.collection.immutable.Seq

    /**
     * Method to compile an Obr program and to apply a specified test to
     * the resulting target tree.
     */
    def targettreetest (name : String, dirname : String, obrfile : String,
                        tester : (String, Emitter, RISCTree) => Unit) {
        val title = s"$name processing $obrfile"

        test(title) {
            // Initialise compiler state
            SymbolTable.reset ()
            RISCLabels.reset ()

            val filename = dirname + obrfile
            val config = createConfig (Seq (filename))

            try {
                val reader = filereader (filename)
                makeast (reader, filename, config) match {
                    case Left (ast) =>
                        val messaging = new Messaging
                        val analysis = new SemanticAnalysis (messaging)
                        initTree (ast)
                        analysis.errors (ast)
                        if (messaging.messagecount > 0) {
                            messaging.report (config.emitter)
                            fail (s"$title emitted a semantic error.")
                        } else {
                            val transformer = new RISCTransformation (analysis)
                            tester (title, config.emitter, transformer.code (ast))
                        }
                    case Right (msg) =>
                        config.emitter.emitln (msg)
                        fail (s"$title emitted a parse error.")
                }
            } catch {
                case e : FileNotFoundException =>
                    config.emitter.emitln (e.getMessage)
                    info (s"$title failed with an exception.")
                    throw (e)
            }
        }
    }

    /**
     * Test a target tree by collecting together its IntDatum leaves and checking the resulting
     * sequence of integers to see if it contains an expected sequence of integers as a slice.
     */
    def checkintdatums (expected : Seq[Int]) (title : String, emitter : Emitter, code : RISCTree) {
        val realised = Seq.newBuilder[Int]
        bottomup (query {
            case IntDatum(num) =>
                realised += num
            case n : RISCProg  =>
                val found = realised.result
                if (!(found containsSlice expected))
                    fail (s"$title unexpected IntDatum leaves, found $found expected slice $expected")
        }) (code)
    }
}

/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2013 Anthony M Sloane, Macquarie University.
 * Copyright (C) 2010-2013 Dominic Verity, Macquarie University.
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
import org.kiama.util.TestCompiler

/**
 * A driver which compiles a file and allows a test to be run on the resulting
 * target tree.
 */
class TreeTestDriver extends Driver with TestCompiler[ObrInt] {

    import RISCTree._
    import SemanticAnalysis._
    import org.kiama.attribution.Attribution.initTree
    import org.kiama.example.obr.RISCTransformation
    import org.kiama.util.Console
    import org.kiama.util.Emitter
    import org.kiama.util.IO._
    import org.kiama.util.Messaging._
    import org.kiama.rewriting.Rewriter._

    /**
     * Method to compile an Obr program and to apply a specified test to
     * the resulting target tree.
     */
    def targettreetest (name : String, dirname : String, obrfile : String,
                        tester : (String, Emitter, RISCNode) => Unit, emitter : Emitter = new Emitter) {
        val title = s"$name processing $obrfile"

        test(title) {
            // Initialise compiler state
            SymbolTable.reset ()
            RISCLabels.reset ()
            resetmessages ()

            try {
                val reader = filereader (dirname + obrfile)
                makeast (reader, dirname + obrfile, emitter) match {
                    case Left (ast) =>
                        initTree (ast)
                        ast->errors
                        if (messagecount > 0) {
                            report(emitter)
                            fail (s"$title emitted a semantic error.")
                        } else {
                            val transformer = new RISCTransformation
                            tester (title, emitter, transformer.code (ast))
                        }
                    case Right (msg) =>
                        emitter.emitln (msg)
                        fail (s"$title emitted a parse error.")
                }
            } catch {
                case e : FileNotFoundException =>
                    emitter.emitln (e.getMessage)
                    info (s"$title failed with an exception.")
                    throw (e)
            }
        }
    }

    /**
     * Test a target tree by collecting together its IntDatum leaves and checking the resulting
     * sequence of integers to see if it contains an expected sequence of integers as a slice.
     */
    def checkintdatums (expected : List[Int]) (title : String, emitter : Emitter, code : RISCNode) {
        var realised : List[Int] = Nil
        bottomup (query {
            case IntDatum(num) =>
                realised = num :: realised
            case n : RISCProg  =>
                realised = realised.reverse
                if (!(realised containsSlice expected))
                    fail (s"$title unexpected IntDatum leaves, found $realised expected slice $expected")
        }) (code)
    }
}

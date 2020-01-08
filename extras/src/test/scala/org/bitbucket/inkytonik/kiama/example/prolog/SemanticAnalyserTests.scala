/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.prolog

import PrologTree.{PrologNode, Program}
import org.bitbucket.inkytonik.kiama.util.{Compiler, TestCompiler}

/**
 * Tests that check that the semantic analyser works correctly.  I.e., it correctly
 * diagnoses errors where they are present, and passes correct code.
 */
class SemanticAnalyserTests extends Compiler[PrologNode, Program] with TestCompiler[PrologNode, Program] {

    import PrologTree.{PrologTree, Program}
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.{emptyDocument, Document}
    import org.bitbucket.inkytonik.kiama.parsing.ParseResult
    import org.bitbucket.inkytonik.kiama.util.{Config, Source}

    filetests("Prolog", "example/prolog/tests", ".pl", ".sem")

    val name = "prolog"

    def parse(source : Source) : ParseResult[Program] = {
        val parsers = new SyntaxAnalyser(positions)
        parsers.parseAll(parsers.program, source)
    }

    /**
     * Process the tree by conducting semantic analysis and reporting any errors.
     */
    def process(source : Source, ast : Program, config : Config) : Unit = {
        val tree = new PrologTree(ast)
        val analyser = new SemanticAnalyser(tree)
        val messages = analyser.errors
        if (messages.length > 0)
            report(source, messages, config)
    }

    def format(m : Program) : Document =
        emptyDocument

}

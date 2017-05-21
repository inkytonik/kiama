/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.transform

import TransformTree.Program
import org.bitbucket.inkytonik.kiama.util.Compiler

/**
 * Main program for transformation compiler.
 */
class Driver extends Compiler[Program] {

    import TransformTree.TransformTree
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.{emptyDocument, Document}
    import org.bitbucket.inkytonik.kiama.util.{Config, Source}

    val parsers = new SyntaxAnalyser(positions)
    val parser = parsers.program

    def process(source : Source, program : Program, config : Config) {

        // Print original program and obtain "no priority" expression
        config.output().emitln(program)
        val expr = program.expr

        // Check for semantic errors on the original expression.  This
        // will cause a translation to a priority-correct representation
        // and error computation on that rep.
        val tree = new TransformTree(program)
        val analyser = new SemanticAnalyser(tree)
        val messages = analyser.errors

        // For testing, print the priority-correct representation
        config.output().emitln(analyser.ast(expr))

        // Report any semantic errors
        if (messages.length > 0)
            report(messages, config.output())

    }

    def format(ast : Program) : Document =
        emptyDocument

}

/**
 * Transformation compiler main program.
 */
object Main extends Driver

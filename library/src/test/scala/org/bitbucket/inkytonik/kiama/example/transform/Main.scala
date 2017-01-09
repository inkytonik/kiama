/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2017 Anthony M Sloane, Macquarie University.
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

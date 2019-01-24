/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2012-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.minijava

import MiniJavaTree.{MiniJavaNode, Program}
import org.bitbucket.inkytonik.kiama.util.{Compiler, Config}

/**
 * Compile the MiniJava program in the file given as the first command-line
 * argument.
 */
trait Driver extends Compiler[MiniJavaNode, Program] with Server {

    import org.bitbucket.inkytonik.kiama.example.minijava.CodeGenerator.{any, classFileToDoc, generate, hcat, pretty}
    import org.bitbucket.inkytonik.kiama.example.minijava.MiniJavaTree.MiniJavaTree
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
    import org.bitbucket.inkytonik.kiama.parsing.ParseResult
    import org.bitbucket.inkytonik.kiama.util.Messaging.Messages
    import org.bitbucket.inkytonik.kiama.util.Source

    val name = "minijava"

    /**
     * Whether this is a test run or not. Test runs generate all of their
     * code using a single emitter so it can be easily compared to what
     * we expect. A normal run sends the code for each class to a separate
     * file so that they can be compiled by Jasmin.
     */
    def isTest = true

    /**
     * Reset the server's analyser before making the AST in the standard
     * way so that erroneous parses can't reuse an analyser from an older
     * successful run.
     */
    override def makeast(source : Source, config : Config) : Either[Program, Messages] = {
        if (source.optName.isDefined)
            analysers -= source.optName.get
        super.makeast(source, config)
    }

    /**
     * Parse a MiniJava program.
     */
    def parse(source : Source) : ParseResult[Program] = {
        val parsers = new SyntaxAnalyser(positions)
        parsers.parseAll(parsers.program, source)
    }

    /**
     * Process the source tree by analysing it to check for semantic
     * errors. If any messages are produced, print them. If all is ok,
     * translate the program and generate code for the translation.
     */
    def process(source : Source, ast : Program, config : Config) {

        // Perform the semantic checks
        val tree = new MiniJavaTree(ast)
        val analyser = new SemanticAnalyser(tree)

        // Save for server use, clearing out previous position information
        // Other semantic information should go via the analyser replacement
        if (source.optName.isDefined) {
            analysers.get(source.optName.get) match {
                case Some(prevAnalyser) =>
                    positions.resetAllAt(prevAnalyser.tree.nodes)
                case _ =>
                // Do nothing
            }
            analysers(source.optName.get) = analyser
        }

        // Publish the outline and name analysis products
        if (config.server()) {
            publishOutlineProduct(source, outlineDocument(ast))
            publishNameProduct(source, nameDocument(tree, analyser))
        }

        // Report any messages that were produced
        val messages = analyser.errors
        if (messages.length > 0) {

            report(source, messages, config)

        } else {

            // Make a translator for this tree
            val translator = new Translator(tree)

            // Translate the source tree to JVM
            val filename = source.optName.getOrElse("")
            val targettree = translator.translate(ast, filename, analyser)

            // Debugging output of target tree
            if (config.server() || config.debug()) {
                val targetTreeDocument = pretty(any(targettree))
                if (config.server())
                    publishTargetTreeProduct(source, targetTreeDocument)
                else if (config.debug())
                    config.output().emitln(targetTreeDocument.layout)
            }

            // Output code for the target tree
            if (config.server()) {
                val targetDocument = pretty(hcat(targettree.map(classFileToDoc)))
                publishTargetProduct(source, targetDocument)
            } else
                targettree.map(generate(isTest, _, config.output()))

        }

    }

    override def clearSemanticMessages(source : Source, config : Config) {
        super.clearSemanticMessages(source, config)
        if (config.server()) {
            publishTargetProduct(source)
            publishTargetTreeProduct(source)
            publishOutlineProduct(source)
            publishNameProduct(source)
        }
    }

    /**
     * Pretty printer to use to print minijava ASTs.
     */
    override def format(ast : Program) : Document =
        PrettyPrinter.format(ast)

}

/**
 * Main program for MiniJava compiler.
 */
object Main extends Driver {
    override def isTest = false
}

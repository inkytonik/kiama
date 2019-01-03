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

import MiniJavaTree.Program
import org.bitbucket.inkytonik.kiama.util.Compiler
import org.bitbucket.inkytonik.kiama.util.Config

/**
 * Compile the MiniJava program in the file given as the first command-line
 * argument.
 */
trait Driver extends Compiler[Program] {

    import CodeGenerator.generate
    import MiniJavaTree.MiniJavaTree
    import Monto.{nameDocument, outlineDocument}
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinter.{any, layout}
    import org.bitbucket.inkytonik.kiama.util.Messaging.Messages
    import org.bitbucket.inkytonik.kiama.util.{Source, StringEmitter}

    val name = "minijava"

    val parsers = new SyntaxAnalyser(positions)
    val parser = parsers.program

    /**
     * Whether this is a test run or not. Test runs generate all of their
     * code using a single emitter so it can be easily compared to what
     * we expect. A normal run sends the code for each class to a separate
     * file so that they can be compiled by Jasmin.
     */
    def isTest = true

    /**
     * Process the source tree by analysing it to check for semantic
     * errors. If any messages are produced, print them. If all is ok,
     * translate the program and generate code for the translation.
     */
    def process(source : Source, ast : Program, config : Config) {

        // Perform the semantic checks
        val tree = new MiniJavaTree(ast)
        val analyser = new SemanticAnalyser(tree)
        val messages = analyser.errors

        // Publish the outline and name analysis products
        if (config.server()) {
            publishOutlineProduct(source, outlineDocument(ast).layout)
            publishNameProduct(source, nameDocument(tree, analyser).layout)
        }

        // Report any messages that were produced
        if (messages.length > 0) {

            report(source, messages, config)

        } else {

            // Make a translator for this tree
            val translator = new Translator(tree)

            // Translate the source tree to JVM
            val filename = source.optName.getOrElse("")
            val targettree = translator.translate(ast, filename, analyser)

            // Pretty print the target tree
            if (config.server() || config.debug()) {
                val targetTreeText = layout(any(targettree))
                if (config.server()) {
                    publishTargetTreeProduct(source, targetTreeText)
                } else if (config.debug())
                    config.output().emitln(targetTreeText)
            }

            // Output code for the target tree
            if (config.server()) {
                val target = new StringEmitter
                targettree.map(generate(true, _, target))
                publishTargetProduct(source, target.result())
            } else
                targettree.map(generate(isTest, _, config.output()))

        }

    }

    override def report(source : Source, messages : Messages, config : Config) {
        super.report(source, messages, config)
        if (config.server()) {
            publishTargetProduct(source)
            publishTargetTreeProduct(source)
        }
    }

    /**
     * Pretty printer to use to print minijava ASTs.
     */
    override def format(ast : Program) : Document =
        PrettyPrinter.format(ast)

    // Monto product publishing

    def publishTargetProduct(source : Source, content : String = "") {
        publishProduct(
            source.optName.getOrElse("unknown"),
            "target", "jasmin", content
        )
    }

    def publishTargetTreeProduct(source : Source, content : String = "") {
        publishProduct(
            source.optName.getOrElse("unknown"),
            "targettree", "scala", content
        )
    }

    def publishOutlineProduct(source : Source, content : String = "") {
        publishProduct(
            source.optName.getOrElse("unknown"),
            "outline", "minijava", content
        )
    }

    def publishNameProduct(source : Source, content : String = "") {
        publishProduct(
            source.optName.getOrElse("unknown"),
            "name", "minijava", content
        )
    }

}

/**
 * Main program for MiniJava compiler.
 */
object Main extends Driver {
    override def isTest = false
}

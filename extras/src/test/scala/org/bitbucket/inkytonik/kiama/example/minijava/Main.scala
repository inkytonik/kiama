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
import org.bitbucket.inkytonik.kiama.util.{Compiler, Config, Position}

/**
 * Compile the MiniJava program in the file given as the first command-line
 * argument.
 */
trait Driver extends Compiler[MiniJavaNode, Program] {

    import CodeGenerator.{any, classFileToDoc, generate, hcat, pretty}
    import MiniJavaTree.{IdnTree, MiniJavaTree}
    import Monto.{hoverDocument, nameDocument, outlineDocument}
    import SymbolTable.MiniJavaEntity
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.{Document, emptyDocument}
    import org.bitbucket.inkytonik.kiama.parsing.ParseResult
    import org.bitbucket.inkytonik.kiama.util.Messaging.Messages
    import org.bitbucket.inkytonik.kiama.util.{MultipleEntity, Source, UnknownEntity}
    import scala.collection.mutable

    val name = "minijava"

    /**
     * The analysers previously used by the semantic analysis phase of this
     * compiler, indexed by source name.
     */
    val analysers = mutable.Map[String, SemanticAnalyser]()

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

            // Pretty print the target tree
            if (config.server() || config.debug()) {
                val targetTreeDocument = pretty(any(targettree))
                if (config.server()) {
                    publishTargetTreeProduct(source, targetTreeDocument)
                } else if (config.debug())
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
     * Find nodes that overlap the given position and return the
     * first value that `f` produces when applied to the appropriate
     * analyser and an overlapping node.
     */
    def getRelevantInfo[I](
        position : Position,
        f : (SemanticAnalyser, Vector[MiniJavaNode]) ==> Option[I]
    ) : Option[I] =
        for (
            name <- position.source.optName;
            analyser <- analysers.get(name);
            nodes = analyser.tree.nodes;
            relevantNodes = positions.findNodesContaining(nodes, position);
            info <- f((analyser, relevantNodes))
        ) yield info

    /**
     * Hover information is provided for identifier defs and uses.
     * It consists of a short description of the associated entity
     * and a pretty-printed version of the corresponding declaration.
     */
    override def getHover(position : Position) : Option[String] =
        getRelevantInfo(position, {
            case (analyser, nodes) =>
                nodes.collectFirst {
                    case n : IdnTree =>
                        analyser.entity(n) match {
                            case e : MiniJavaEntity =>
                                val p = hoverDocument(e.decl).layout
                                s"${e.desc} ${n.idn}\n\n```\n$p```"
                            case MultipleEntity() =>
                                s"multiply-defined ${n.idn}"
                            case UnknownEntity() =>
                                s"unknown ${n.idn}"
                        }
                }
        })

    /**
     * Definitions are provided for defined identifiers and point
     * to the corresponding declaration node.
     */
    override def getDefinition(position : Position) : Option[MiniJavaNode] =
        getRelevantInfo(position, {
            case (analyser, nodes) =>
                nodes.collectFirst {
                    case n : IdnTree => n
                }.collectFirst(n =>
                    analyser.entity(n) match {
                        case e : MiniJavaEntity =>
                            e.decl
                    })
        })

    /**
     * Pretty printer to use to print minijava ASTs.
     */
    override def format(ast : Program) : Document =
        PrettyPrinter.format(ast)

    // Monto product publishing

    def publishTargetProduct(source : Source, document : Document = emptyDocument) {
        if (setting("showTarget"))
            publishProduct(source, "target", "jasmin", document)
    }

    def publishTargetTreeProduct(source : Source, document : Document = emptyDocument) {
        if (setting("showTargetTree"))
            publishProduct(source, "targettree", "scala", document)
    }

    def publishOutlineProduct(source : Source, document : Document = emptyDocument) {
        if (setting("showOutline"))
            publishProduct(source, "outline", "minijava", document)
    }

    def publishNameProduct(source : Source, document : Document = emptyDocument) {
        if (setting("showNameAnalysisStructure"))
            publishProduct(source, "name", "minijava", document)
    }

}

/**
 * Main program for MiniJava compiler.
 */
object Main extends Driver {
    override def isTest = false
}

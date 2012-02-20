package org.kiama
package example.oberon0
package base

import base.source.ModuleDecl
import org.clapper.argot.{ArgotParser, ArgotUsageException}
import org.clapper.argot.ArgotConverters._
import org.kiama.util.Compiler
import org.kiama.attribution.Attribution.initTree
import org.kiama.attribution.Attribution.resetMemo
import org.kiama.util.Console
import org.kiama.util.Emitter
import org.kiama.util.PrettyPrinter
import scala.util.parsing.combinator.RegexParsers

/**
 * A driver for an artefact that parses, pretty prints and performs semantic 
 * analysis.
 */
trait Driver extends Compiler[ModuleDecl] with PrettyPrinter {

    this : RegexParsers with source.SourcePrettyPrinter with SymbolTable
        with Analyser =>

    import java.io.FileReader
    import java.io.FileNotFoundException
    import org.kiama.util.Messaging.{message, messagecount, report,
        resetmessages, sortedmessages}

    /**
     * The name of this artefact.
     */
    def artefact : String

    // Command-line arguments

    val argparser = new ArgotParser ("oberon0c (" + artefact + ")")
    val help = argparser.flag[Boolean] (List ("h", "help"), "print this message")

    val printast = argparser.flag[Boolean] (List ("a", "ast"), "print the AST")
    val pprintast = argparser.flag[Boolean] (List ("A", "past"), "pretty-print the AST")

    val challenge = argparser.flag[Boolean] (List ("x", "challenge"), "run in challenge mode")

    val input = argparser.parameter[String] ("input", "Oberon0 file", false)

    /**
     * Process the command-line arguments and return Some(filename) of the input
     * file if processing should continue, None otherwise.
     */
    override def checkargs (args : Array[String], emitter : Emitter) : Array[String] = {
        try {
            argparser.parse (args)
        } catch {
            case e : ArgotUsageException =>
                println (e.message)
                return Array.empty
        }
        if (help.value isDefined) {
            println (argparser.usageString ())
            Array.empty
        } else
            Array (input.value.get)
    }

    /**
     * Output a section heading so that the output can be split later.
     */
    def section (emitter : Emitter, name : String) =
        emitter.emitln ("* " + name)

    /**
     * Custom driver for section tagging and challenge mode for errors.  If 
     * a parse error occurs: in challenge mode, just send "parse failed" to
     * standard output, otherwise send the message to the errors file.
     */
    override def driver (args : Array[String], console : Console, emitter : Emitter) {
        val newargs = checkargs (args, emitter)
        for (arg <- newargs) {
            try {
                val reader = new FileReader (newargs (0))
                makeast (reader, newargs (0), emitter) match {
                    case Left (ast) =>
                        process (ast, console, emitter)
                    case Right (msg) =>
                        if (challenge.value isDefined) {
                            section (emitter, "stdout")
                            emitter.emitln ("parse failed")
                        }
                        section (emitter, "errors")
                        emitter.emitln (msg)
                }
            } catch {
                case e : FileNotFoundException =>
                    emitter.emitln (e.getMessage)
            }
        }
    }

    /**
     * Perform initialisation of semantic analysis that is necessary before
     * processing an AST.
     */
    def initialiseSemanticAnalysis {
        resetEnvironments
        resetmessages
        resetMemo
    }

    /**
     * Process the given abstract syntax tree.  Send output to emitter,
     * marking sections so that we can split things later.
     */
    override def process (ast : ModuleDecl, console : Console, emitter : Emitter) : Boolean = {

        // Perform default processing
        super.process (ast, console, emitter)

        if (printast.value isDefined) {
            section (emitter, "ast")
            emitter.emitln (pretty (product (ast)))
        }
        if (pprintast.value isDefined) {
            section (emitter, "_pp.ob")
            emitter.emitln (pretty (toDoc (ast)))
        }

        // Perform semantic analysis
        initialiseSemanticAnalysis
        check (ast)
        if (messagecount == 0) {

            // No semantic errors, go on to process the AST as appropriate
            val nast = processast (ast, console, emitter)
            
            // Consume the processed AST (e.g., by generating code from it)
            consumeast (nast, console, emitter)
            
            true

        } else {

            // Semantic analysis failed, abort.  If in challenge mode, report
            // line number of first error to standard output.  Make full report
            // to errors file.
            if (challenge.value isDefined) {
                section (emitter, "stdout")
                emitter.emitln ("line " + sortedmessages (0).pos.line)
            }
            section (emitter, "errors")
            report (emitter)
            false

        }

    }

    /**
     * Process the AST, returning the new one.  By default, return the AST unchanged.
     */
    def processast (ast : ModuleDecl, console : Console, emitter : Emitter) : ModuleDecl =
        ast

    /**
     * Process the AST, returning the new one.  By default, do nothing.
     */
    def consumeast (ast : ModuleDecl, console : Console, emitter : Emitter) {
    }

}

/**
 * A driver for an artefact that parses, pretty prints, performs semantic 
 * analysis and transforms.
 */
trait TransformingDriver extends Driver {

    this : RegexParsers with source.SourcePrettyPrinter with SymbolTable
        with Analyser with Transformer =>

    val printiast = argparser.flag[Boolean] (List ("i", "iast"), "print the intermediate AST")
    val pprintiast = argparser.flag[Boolean] (List ("I", "piast"), "pretty-print the intermediate AST")

    /**
     * Process the AST by transforming it.  Then apply higher-level transformations.
     */
    override def processast (ast : ModuleDecl, console : Console, emitter : Emitter) : ModuleDecl = {
        initialiseSemanticAnalysis
        val nast = transform (ast)
        if (printiast.value isDefined) {
            section (emitter, "iast")
            emitter.emitln (pretty (product (nast)))
        }
        if (pprintiast.value isDefined)
            section (emitter, "_ipp.ob")
        if (challenge.value isDefined)
            section (emitter, "_lifted.ob")
        if ((pprintiast.value isDefined) || (challenge.value isDefined))
            emitter.emitln (pretty (toDoc (nast)))
        initTree (nast)
        nast
    }

}

/**
 * A driver for an artefact that parses, pretty prints, performs semantic 
 * analysis, transforms and translates.
 */
trait TranslatingDriver extends TransformingDriver {

    this : RegexParsers with source.SourcePrettyPrinter with SymbolTable
        with Analyser with Transformer with Translator with c.CPrettyPrinter =>

    val printcast = argparser.flag[Boolean] (List ("c", "cast"), "print the C AST")
    val pprintcast = argparser.flag[Boolean] (List ("C", "pcast"), "pretty-print the C AST")

    /**
     * Consume the AST by translating it to C.
     */
    override def consumeast (ast : ModuleDecl, console : Console, emitter : Emitter) {
        initialiseSemanticAnalysis
        val nast = translate (ast)
        if (printcast.value isDefined) {
            section (emitter, "cast")
            emitter.emitln (pretty (product (nast)))
        }
        if ((pprintcast.value isDefined) || (challenge.value isDefined)) {
            section (emitter, "c")
            emitter.emitln (pretty (toDoc (nast)))
        }
    }

}

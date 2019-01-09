/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package util

import org.eclipse.lsp4j.{Position => LSPPosition, Range => LSPRange, _}

/**
 * A language server that is mixed with a compiler that provide the basis
 * for its services. Allows specialisation of configuraiton via `C`.
 */
trait ServerWithConfig[T, C <: Config] {

    this : CompilerBase[T, C] =>

    import java.io.PrintWriter
    import java.lang.System.{in, out}
    import java.util.Collections
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.{Document, Link}
    import org.bitbucket.inkytonik.kiama.util.Messaging.Messages
    import org.bitbucket.inkytonik.kiama.util.Severities._
    import org.eclipse.lsp4j.jsonrpc.Launcher
    import scala.collection.JavaConverters._

    // Client saving

    private[this] var client : Client = _

    def connect(aClient : Client) {
        client = aClient
    }

    // Launching

    def launch(config : C) {
        val services = new Services(this, config)
        val launcherBase =
            new Launcher.Builder[Client]()
                .setLocalService(services)
                .setRemoteInterface(classOf[Client])
                .setInput(in)
                .setOutput(out)
        val launcherFull =
            if (config.debug()) {
                val writer = new PrintWriter(System.err, true)
                launcherBase.traceMessages(writer)
            } else {
                launcherBase
            }
        val launcher = launcherFull.create()
        val client = launcher.getRemoteProxy()
        connect(client)
        launcher.startListening()
    }

    // User messages

    def showMessage(tipe : MessageType, msg : String) {
        client.showMessage(new MessageParams(tipe, msg))
    }

    // Dynamic capabilities

    def registerCapability(id : String, method : String, options : Object) {
        val registration = new Registration(id, method, options)
        val params = new RegistrationParams(Collections.singletonList(registration))
        client.registerCapability(params)
    }

    // Diagnostics

    def publishMessages(messages : Messages) {
        val groups = messages.groupBy(name(_).getOrElse(""))
        for ((uri, msgs) <- groups) {
            publishDiagnostics(uri, msgs.map(messageToDiagnostic))
        }
    }

    def publishDiagnostics(uri : String, diagnostics : Vector[Diagnostic]) {
        val params = new PublishDiagnosticsParams(uri, diagnostics.asJava)
        client.publishDiagnostics(params)
    }

    def clearDiagnostics(uri : String) {
        publishDiagnostics(uri, Vector())
    }

    def messageToDiagnostic(message : Message) : Diagnostic = {
        val s = convertPosition(start(message))
        val f = convertPosition(finish(message))
        val range = new LSPRange(s, f)
        val severity = convertSeverity(message.severity)
        new Diagnostic(range, message.label, severity, name)
    }

    def convertPosition(optPos : Option[Position]) : LSPPosition =
        optPos match {
            case Some(p) => new LSPPosition(p.line - 1, p.column - 1)
            case None    => new LSPPosition(0, 0)
        }

    def convertSeverity(severity : Severity) : DiagnosticSeverity =
        severity match {
            case Error       => DiagnosticSeverity.Error
            case Warning     => DiagnosticSeverity.Warning
            case Information => DiagnosticSeverity.Information
            case Hint        => DiagnosticSeverity.Hint
        }

    // Monto

    case class RangePair(
        sstart : Int, send : Int,
        tstart : Int, tend : Int
    )

    def publishProduct(
        source : Source, name : String, language : String, document : Document
    ) {
        val uri = source.optName.getOrElse("unknown")
        val content = document.layout
        val pairs = positionsOfDocument(document)
        val rangeMap = sortBySourceRangeSize(pairsToMap(pairs, pairToSourceRange, pairToTargetRange))
        val rangeMapRev = sortBySourceRangeSize(pairsToMap(pairs, pairToTargetRange, pairToSourceRange))
        client.publishProduct(
            Product(uri, name, language, content, rangeMap, rangeMapRev)
        )
    }

    def sortBySourceRangeSize(pairs : Array[RangeEntry]) : Array[RangeEntry] =
        pairs.sortBy {
            entry => entry.source.end - entry.source.start
        }

    def pairsToMap(
        pairs : List[RangePair],
        key : RangePair => OffsetRange,
        value : RangePair => OffsetRange
    ) : Array[RangeEntry] = {
        pairs.groupBy(key).toArray.map {
            case (s, ts) =>
                RangeEntry(s, ts.map(value).toArray)
        }
    }

    def pairToSourceRange(pair : RangePair) : OffsetRange =
        OffsetRange(pair.sstart, pair.send)

    def pairToTargetRange(pair : RangePair) : OffsetRange =
        OffsetRange(pair.tstart, pair.tend)

    def positionsOfDocument(document : Document) : List[RangePair] =
        document.links.flatMap {
            case Link(n, r) =>
                val start = positions.getStart(n)
                val finish = positions.getFinish(n)
                positionOfStartFinish(start, finish) match {
                    case Some((s, f)) =>
                        Some(RangePair(s, f, r.start, r.end))
                    case None =>
                        None
                }
        }

    def positionOfStartFinish(optStart : Option[Position], optFinish : Option[Position]) : Option[(Int, Int)] =
        (optStart, optFinish) match {
            case (Some(start), Some(finish)) =>
                (start.optOffset, finish.optOffset) match {
                    case (Some(s), Some(f)) =>
                        Some((s, f))
                    case _ =>
                        None
                }
            case _ =>
                None
        }

}

class Services[T, C <: Config](
    server : ServerWithConfig[T, C] with CompilerBase[T, C],
    config : C
) {

    import java.util.concurrent.CompletableFuture
    import org.eclipse.lsp4j.jsonrpc.services._

    // Life-cycle

    /**
     * Exit status to return when the server exits. `shutdown` sets this
     * to zero to reflect proper shutdown protocol.
     */
    var exitStatus = 1

    @JsonRequest("initialize")
    def initialize(params : InitializeParams) : CompletableFuture[InitializeResult] =
        CompletableFuture.completedFuture {
            val serverCapabilities = new ServerCapabilities
            serverCapabilities.setTextDocumentSync(TextDocumentSyncKind.Full)
            new InitializeResult(serverCapabilities)
        }

    @JsonNotification("initialized")
    def initialized(params : InitializedParams) {
        val saveOptions = new TextDocumentSaveRegistrationOptions()
        saveOptions.setIncludeText(true)
        server.registerCapability("kiama/textDocument/didSave", "textDocument/didSave", saveOptions)
    }

    @JsonNotification("exit")
    def exit() {
        sys.exit(exitStatus)
    }

    @JsonRequest("shutdown")
    def shutdown() : CompletableFuture[Object] = {
        CompletableFuture.completedFuture {
            exitStatus = 0
            new Object
        }
    }

    // Text document services

    @JsonNotification("textDocument/didOpen")
    def didOpen(params : DidOpenTextDocumentParams) {
        val document = params.getTextDocument
        process(document.getUri, document.getText)
    }

    @JsonNotification("textDocument/didChange")
    def didChange(params : DidChangeTextDocumentParams) {
        // Do nothing
    }

    @JsonNotification("textDocument/didSave")
    def didSave(params : DidSaveTextDocumentParams) {
        val documentId = params.getTextDocument
        process(documentId.getUri, params.getText)
    }

    @JsonNotification("textDocument/didClose")
    def didClose(params : DidCloseTextDocumentParams) {
        val uri = params.getTextDocument.getUri
        server.clearDiagnostics(uri)
    }

    def process(uri : String, text : String) {
        server.clearDiagnostics(uri)
        server.compileString(uri, text, config)
    }

}

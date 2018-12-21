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

import org.eclipse.lsp4j.{Position => LSPPosition, _}
import org.eclipse.lsp4j.services.LanguageClient

/**
 * A language server that is mixed with a compiler that provide the basis
 * for its services. Allows specialisation of configuraiton via `C`.
 */
trait ServerWithConfig[T, C <: Config] {

    this : CompilerBase[T, C] =>

    import java.io.PrintWriter
    import java.lang.System.{in, out}
    import java.util.Collections
    import org.bitbucket.inkytonik.kiama.util.Messaging.Messages
    import org.eclipse.lsp4j.jsonrpc.Launcher
    import scala.collection.JavaConverters._

    /**
     * The name of this server which will be shown to the user.
     */
    def name : String

    // Client saving

    private[this] var client : LanguageClient = _

    def connect(aClient : LanguageClient) {
        client = aClient
    }

    // Launching

    override def run(config : C) {
        val services = new Services(this, config)
        val launcherBase =
            new Launcher.Builder[LanguageClient]()
                .setLocalService(services)
                .setRemoteInterface(classOf[LanguageClient])
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

    // Messages

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

    def publishDiagnostics(uri : String, diagnostics : Vector[Diagnostic]) {
        val params = new PublishDiagnosticsParams(uri, diagnostics.asJava)
        client.publishDiagnostics(params)
    }

    override def report(messages : Messages, config : C) {
        val groups = messages.groupBy(name(_).getOrElse(""))
        for ((uri, msgs) <- groups) {
            publishDiagnostics(uri, msgs.map(messageToDiagnostic))
        }
    }

    def clearDiagnostics(uri : String) {
        publishDiagnostics(uri, Vector())
    }

    def messageToDiagnostic(message : Message) : Diagnostic = {
        val s = convertPosition(start(message))
        val f = convertPosition(finish(message))
        val range = new Range(s, f)
        new Diagnostic(range, message.label, DiagnosticSeverity.Error, name)
    }

    def convertPosition(optPos : Option[Position]) : LSPPosition =
        optPos match {
            case Some(p) => new LSPPosition(p.line - 1, p.column - 1)
            case None    => new LSPPosition(0, 0)
        }

}

/**
 * A server that uses the given compiler to provide its services.
 * Uses the default configuration type.
 */
trait Server[T] extends ServerWithConfig[T, Config] {

    this : CompilerBase[T, Config] =>

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
            val capabilities = new ServerCapabilities
            capabilities.setTextDocumentSync(TextDocumentSyncKind.Full)
            new InitializeResult(capabilities)
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
        val document = params.getTextDocument()
        process(document.getUri(), document.getText())
    }

    @JsonNotification("textDocument/didChange")
    def didChange(params : DidChangeTextDocumentParams) {
        // Do nothing
    }

    @JsonNotification("textDocument/didSave")
    def didSave(params : DidSaveTextDocumentParams) {
        val documentId = params.getTextDocument()
        process(documentId.getUri(), params.getText())
    }

    @JsonNotification("textDocument/didClose")
    def didClose(params : DidCloseTextDocumentParams) {
        val uri = params.getTextDocument().getUri()
        server.clearDiagnostics(uri)
    }

    def process(uri : String, text : String) {
        server.clearDiagnostics(uri)
        server.compileString(uri, text, config)
    }

}

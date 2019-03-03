/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package util

/**
 * Class of objects that can emit text.
 */
abstract class Emitter {

    /**
     * Emit `any`.
     */
    def emit(any : Any)

    /**
     * Emit `any` and start a new line.
     */
    def emitln(any : Any)

    /**
     * Emit a new line.
     */
    def emitln()

    /**
     * Close this emitter. Default: do nothing.
     */
    def close() {
    }

}

/**
 * Class of objects that can emit arbitrary output.  The output is sent
 * to standard output. Use an `ErrorEmitter` if your output is signalling
 * errors, warnings, log messages or similar.
 */
class OutputEmitter extends Emitter {

    /**
     * Emit `any`.
     */
    def emit(any : Any) {
        print(any.toString)
    }

    /**
     * Emit `any` and start a new line.
     */
    def emitln(any : Any) {
        println(any.toString)
    }

    /**
     * Emit a new line.
     */
    def emitln() {
        println
    }

}

/**
 * An emitter that records the output in a string that can be accessed
 * via the result method.
 */
class StringEmitter extends Emitter {
    val b = new StringBuilder
    override def emit(any : Any) { b.append(any.toString) }
    override def emitln(any : Any) { b.append(any.toString).append('\n') }
    override def emitln() { b.append('\n') }
    def clear() { b.clear }
    def result() : String = b.result
}

/**
 * A string emitter that also provides a `close` method to send the
 * result to the named UTF-8 encoded file.
 */
class FileEmitter(filename : String) extends StringEmitter {
    import org.bitbucket.inkytonik.kiama.util.IO.filewriter

    override def close() {
        val out = filewriter(filename)
        out.write(result())
        out.close()
    }
}

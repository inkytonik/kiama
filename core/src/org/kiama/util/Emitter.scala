/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2015 Anthony M Sloane, Macquarie University.
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

package org.kiama
package util

/**
 * Class of objects that can emit text.
 */
abstract class Emitter {

    /**
     * Emit `any`.
     */
    def emit (any : Any)

    /**
     * Emit `any` and start a new line.
     */
    def emitln (any : Any)

    /**
     * Emit a new line.
     */
    def emitln ()

    /**
     * Close this emitter. Default: do nothing.
     */
    def close () {
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
    def emit (any : Any) {
        print (any.toString)
    }

    /**
     * Emit `any` and start a new line.
     */
    def emitln (any : Any) {
        println (any.toString)
    }

    /**
     * Emit a new line.
     */
    def emitln () {
        println
    }

}

/**
 * Class of objects that can emit arbitrary error-like text such as program
 * errors, warnings or log messages.  The messages are sent to standard error.
 * Use an `OutputEmitter` if you just want to output normal program output.
 */
class ErrorEmitter extends Emitter {

    /**
     * Emit `any`.
     */
    def emit (any : Any) {
        print (any.toString)
    }

    /**
     * Emit `any` and start a new line.
     */
    def emitln (any : Any) {
        println (any.toString)
    }

    /**
     * Emit a new line.
     */
    def emitln () {
        println
    }

}

/**
 * An emitter that records the output in a string that can be accessed
 * via the result method.
 */
class StringEmitter extends Emitter {
    val b = new StringBuilder
    override def emit (any : Any) { b.append (any.toString) }
    override def emitln (any : Any) { b.append (any.toString).append ('\n') }
    override def emitln () { b.append ('\n') }
    def clear () { b.clear }
    def result () : String = b.result
}

/**
 * A string emitter that also provides a `close` method to send the
 * result to the named UTF-8 encoded file.
 */
class FileEmitter (filename : String) extends StringEmitter {
    import org.kiama.util.IO.filewriter

    override def close () {
        val out = filewriter (filename)
        out.write (result ())
        out.close ()
    }
}

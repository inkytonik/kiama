/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2013 Anthony M Sloane, Macquarie University.
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
 * A message record consisting of a coordinate position `pos` and a label string
 */
case class Message (line : Int, column : Int, label : String) {
    override def toString : String =
        s"$line.$column: $label"
}

/**
 * Facility for buffering of messages associated with positioned values.
 */
class Messaging {

    import scala.util.parsing.input.Positional

    /**
     * Buffer of messages.
     */
    val messages = scala.collection.mutable.ListBuffer[Message] ()

    /**
     * The messages sorted by increasing position.
     */
    def sortedmessages : Seq[Message] =
        messages.toList.sortWith {
            case (msg1, msg2) =>
                (msg1.line < msg2.line) ||
                ((msg1.line == msg2.line) && (msg1.column < msg2.column))
        }

    /**
     * Buffer a new message associated with the given `Positional` value.
     */
    def message (value : Positional, label : String) {
        messages += Message (value.pos.line, value.pos.column, label)
    }

    /**
     * Buffer a new message associated with the given `Positioned` value.
     * The `finish` position is ignored at present.
     */
    def message (value : Positioned, label : String) {
        messages += Message (value.start.line, value.start.column, label)
    }

    /**
     * Return the number of messages that are buffered.
     */
    def messagecount : Int =
        messages.size

    /**
     * Output the messages in order of position using the given emitter, which
     * defaults to standard output.
     */
    def report (emitter : Emitter = new Emitter) {
        for (m <- sortedmessages)
            emitter.emitln (m)
    }

    /**
     * Reset the message buffer to empty.
     */
    def resetmessages () {
        messages.clear
    }

}

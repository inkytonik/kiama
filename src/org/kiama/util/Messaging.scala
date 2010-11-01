/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2010 Anthony M Sloane, Macquarie University.
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
 * Facility for buffering of messages associated with positioned values.
 */
object Messaging {

    import scala.collection.mutable.ListBuffer
    import scala.collection.mutable.StringBuilder
    import scala.util.parsing.input.Positional
    import scala.util.parsing.input.Position

    /**
     * A message record.
     */
    case class Record (pos : Position, message : String) {
        override def toString () = pos.line + "." + pos.column + ": " + message
    }

    /**
     * Buffer of messages.
     */
    val messages = new ListBuffer[Record] ()

    /**
     * The messages sorted by position.
     */
    def sortedmessages : Seq[Record] =
        messages.toList.sortWith (_.pos < _.pos)

    /**
     * Buffer a new message associated with the given positioned value.
     */
    def message (value : Positional, message : String) =
        messages += Record (value.pos, message)

    /**
     * Return the number of messages that are buffered.
     */
    def messagecount : Int =
        messages.size

    /**
     * Output the messages to standard output in order of position.
     */
    def report () =
        for (m <- sortedmessages)
            println (m)

    /**
     * Output the messages in order of position using the given emitter.
     */
    def report (emitter : Emitter) =
        for (m <- sortedmessages)
            emitter.emitln (m)

    /**
     * Return the sorted messages as a single string.
     */
    override def toString () = {
        val b = new StringBuilder
        for (m <- sortedmessages) {
            b.append (m)
            b.append ('\n')
        }
        b.result
    }

    /**
     * Reset the message buffer to empty.
     */
    def resetmessages () =
        messages.clear

}

/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2014 Anthony M Sloane, Macquarie University.
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
 * A message record consisting of a positioned value and a label string.
 * Currently, just the start position of the value is used when reporting
 * this message.
 */
case class Message (value : Positioned, label : String) {

    /**
     * Return the start line of this message's position.
     */
    def line : Int =
        value.start.line

    /**
     * Return the start column of this message's position.
     */
    def column : Int =
        value.start.column

    /**
     * Format the message for reporting as a line containing the start
     * position and the label, followed by lines containing the input
     * text and a pointer to the message location.
     */
    def format : String =
        s"[${value.start}] $label\n\n${value.start.longString}"

}

/**
 * Facility for building messages associated with positioned values.
 */
object Messaging {

    import org.kiama.util.{Entity, ErrorEntity}
    import scala.collection.immutable.{IndexedSeq, Seq}
    import scala.util.parsing.input.Positional

    /**
     * The type of a sequence of messages.
     */
    type Messages = Seq[Message]

    /**
     * A value representing no messages.
     */
    val noMessages = IndexedSeq.empty

    /**
     * Make a sequence of messages from a single message.
     */
    def aMessage (message : Message) =
        IndexedSeq (message)

    /**
     * If `f` is defined at `t` apply it and return the resulting sequence
     * of messages. Otherwise, return an empty sequence.
     */
    def check[T] (t : T) (f : T ==> Messages) : Messages =
        f.applyOrElse (t, (_ : T) => noMessages)

    /**
     * Check that the entity `e` is used legally and return appropriate
     * messages if not. If the entity is an error entity (unknown or multiply
     * defined, keep silent on the grounds that the error has already been
     *reported elsewhere (e.g., at the declaration site of the entity).
     * Otherwise, if `f` is defined at `e` return the messages that `f (e)`
     * evaluates to. If `f` is not defined at `e`, keep silent.
     */
    def checkuse (e : Entity) (f : Entity ==> Messages) : Messages =
        e match {
            case _ : ErrorEntity =>
                noMessages
            case _ =>
                check (e) (f)
        }

    /**
     * If `cond` is true make a singleton message list that associates the
     * message `msg` with the `Positioned` `value`. `cond` can be omitted
     * and defaults to true. The `finish` position is ignored at present.
     */
    def message (value : Positioned, msg : String, cond : Boolean = true) : Messages =
        if (cond)
            aMessage (Message (value, msg))
        else
            noMessages

    /**
     * Output the messages in order of position using the given emitter, which
     * defaults to standard output.
     */
    def report (messages : Messages, emitter : Emitter = new Emitter) {
        sortmessages (messages).map (msg => emitter.emitln (msg.format))
    }

    /**
     * Sort the messages by increasing position.
     */
    def sortmessages (messages : Messages) : Messages =
        messages.sortWith {
            case (msg1, msg2) =>
                (msg1.line < msg2.line) ||
                ((msg1.line == msg2.line) && (msg1.column < msg2.column))
        }

}

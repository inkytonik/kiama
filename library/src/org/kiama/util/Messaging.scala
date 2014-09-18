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

import scala.util.parsing.input.{Position, NoPosition}

/**
 * A message record consisting of a position and a label string. If the
 * position is not specified, it defaults to `NoPosition`.
 */
case class Message (label : String, pos : Position = NoPosition) {

    /**
     * Return the start line of this message's position.
     */
    def line : Int =
        pos.line

    /**
     * Return the start column of this message's position.
     */
    def column : Int =
        pos.column

    /**
     * Format the message for reporting as a line containing the start
     * position and the label, followed by lines containing the input
     * text and a pointer to the message location.
     */
    def format : String =
        s"[${pos}] $label\n\n${pos.longString}"

}

/**
 * Facility for building messages associated with positioned values.
 */
object Messaging {

    import org.kiama.relation.Tree
    import org.kiama.util.{Entity, ErrorEntity}
    import scala.collection.immutable.{IndexedSeq, Seq}

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
     * message `msg` with the start position recorded for `value` (if any).
     * `cond` can be omitted and defaults to true. Any `finish` position
     * that is recorded for `value` is ignored at present.
     */
    def message[T] (value : T, msg : String, cond : Boolean = true) : Messages =
        if (cond)
            aMessage (Message (msg, Positions.getStart (value)))
        else
            noMessages

    /**
     * Output the messages in order of position using the given emitter, which
     * defaults to standard error.
     */
    def report (messages : Messages, emitter : Emitter = new ErrorEmitter) {
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


    /**
     * Recursively collect all messages in the given tree using the partial
     * function `messages` at all nodes where it is defined.
     */
    def collectmessages[T <: Product,U <: T] (tree : Tree[T,U]) (messages : T ==> Messages) : Messages =
        tree.nodes.flatMap (messages.orElse { case _ => noMessages })

}

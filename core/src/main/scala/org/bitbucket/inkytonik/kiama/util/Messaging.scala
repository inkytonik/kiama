/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package util

/**
 * A message record consisting of a value with which the message is associated
 * and a label string.
 */
case class Message(value : AnyRef, label : String)

/**
 * Shared definitions for all messaging.
 */
object Messaging {

    import org.bitbucket.inkytonik.kiama.relation.Tree
    import org.bitbucket.inkytonik.kiama.util.{Entity, ErrorEntity}

    /**
     * The type of a sequence of messages.
     */
    type Messages = Vector[Message]

    /**
     * Return a value representing no messages.
     */
    def noMessages = Vector[Message]()

    /**
     * If `f` is defined at `t` apply it and return the resulting sequence
     * of messages. Otherwise, return an empty sequence.
     */
    def check[T](t : T)(f : T ==> Messages) : Messages =
        f.applyOrElse(t, (_ : T) => noMessages)

    /**
     * Check that the entity `e` is used legally and return appropriate
     * messages if not. If the entity is an error entity (unknown or multiply
     * defined, keep silent on the grounds that the error has already been
     * reported elsewhere (e.g., at the declaration site of the entity).
     * Otherwise, if `f` is defined at `e` return the messages that `f (e)`
     * evaluates to. If `f` is not defined at `e`, keep silent.
     */
    def checkUse(e : Entity)(f : Entity ==> Messages) : Messages =
        e match {
            case _ : ErrorEntity =>
                noMessages
            case _ =>
                check(e)(f)
        }

    /**
     * Recursively collect all messages in the given tree using the partial
     * function `messages` at all nodes where it is defined.
     */
    def collectMessages[T <: Product, U <: T](tree : Tree[T, U])(messages : T ==> Messages) : Messages =
        tree.nodes.flatMap(messages.orElse { case _ => noMessages }).toVector

    /**
     * If `cond` is true make a singleton message list that associates the
     * label with the start position recorded for `value` (if any). `cond`
     * can be omitted and defaults to true.
     */
    def message(value : AnyRef, label : String, cond : Boolean = true) : Messages =
        if (cond)
            Vector(Message(value, label))
        else
            noMessages

}

/**
 * General facility for processing messages relative to positioned values.
 */
trait Messaging {

    self : PositionStore =>

    import org.bitbucket.inkytonik.kiama.util.Messaging._
    import scala.math.Ordering._

    /**
     * An ordering on messages that uses starting position and prioritises
     * line over column. The messages are assumed to refer to the same
     * source.
     */
    implicit object messageOrdering extends Ordering[Message] {
        def compare(m1 : Message, m2 : Message) =
            Ordering[(Option[Int], Option[Int], String)].compare(
                (startLine(m1), startColumn(m1), m1.label),
                (startLine(m2), startColumn(m2), m2.label)
            )
    }

    /**
     * A message's finishing position as determined from the finishing position
     * of the message's value. Will be `None` if the value has no position.
     */
    def finish(message : Message) : Option[Position] =
        positions.getFinish(message.value)

    /**
     * Return the optional finishing column number of a message.
     */
    def finishColumn(message : Message) : Option[Int] =
        finish(message).map(_.column)

    /**
     * Return the optional finishing line number of a message.
     */
    def finishLine(message : Message) : Option[Int] =
        finish(message).map(_.line)

    /**
     * Format the message for reporting as a line containing the starting
     * position and label, the input text line and line(s) containing the
     * context of the position. If no position is associated with this
     * message just format as a line containing the label.
     */
    def formatMessage(message : Message) : String =
        start(message) match {
            case Some(pos) =>
                val context = pos.optContext.getOrElse("")
                s"${pos.format} ${message.label}\n$context\n"
            case None =>
                s"${message.label}\n"
        }

    /**
     * Return a string containing all the given messages sorted and formatted.
     */
    def formatMessages(messages : Messages) : String =
        messages.sorted.map(formatMessage).mkString("")

    /**
     * A message's source name as determined from the source of the
     * message's value's position. Will be `None` if the value has no
     * position or that position's source has no name.
     */
    def name(message : Message) : Option[String] =
        positions.getStart(message.value).map(_.source.optName).map(_.get)

    /**
     * A message's starting position as determined from the starting position
     * of the message's value. Will be `None` if the value has no position.
     */
    def start(message : Message) : Option[Position] =
        positions.getStart(message.value)

    /**
     * Return the optional starting column number of a message.
     */
    def startColumn(message : Message) : Option[Int] =
        start(message).map(_.column)

    /**
     * Return the optional starting line number of a message.
     */
    def startLine(message : Message) : Option[Int] =
        start(message).map(_.line)

    /**
     * Output the messages in order of position using the given emitter, which
     * defaults to terminal output.
     */
    def report(messages : Messages, emitter : Emitter = new OutputEmitter) {
        emitter.emit(formatMessages(messages))
    }

}

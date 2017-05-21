/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package util

/**
 * Record of a source position at a particular line and column relative to
 * a given source.
 */
case class Position(line : Int, column : Int, source : Source) {

    /**
     * Format this position. The result is of the form `/foo/bar.txt:2:10:` if
     * a source is associated with the position and the source has a name, or
     * of the form `2:10:` otherwise. In each case the numbers are line followed
     * by column.
     */
    lazy val format : String = {
        val name = source.optName.map(_ + ":").getOrElse("")
        s"$name$line:$column:"
    }

    /**
     * Turn this position into a string that summarises the context of the input
     * referred to by the position. If the position has a source that provides
     * access to its lines then the context is the line containing the position
     * followed by a line containing a caret pointer. Otherwise, return `None`.
     */
    lazy val optContext : Option[String] =
        source.optLineContents(line).map(s => s"$s\n${" " * (column - 1)}^")

    /**
     * Return the offset that this position refers to in its source. `None`
     * is returned if the position is not valid for its source.
     */
    lazy val optOffset : Option[Int] =
        source.positionToOffset(this)

    /**
     * Does this position occur at least as late as `p`? The two positions
     * are assumed to refer to the same source. False is returned if one
     * of the positions is invalid.
     */
    def <=(p : Position) : Boolean =
        (optOffset, p.optOffset) match {
            case (Some(l), Some(r)) =>
                l <= r
            case (l, r) =>
                false
        }

}

/**
 * Interface for objects that have a position store.
 */
trait PositionStore {
    val positions = new Positions
}

/**
 * Record of source positions that correspond to program elements.
 */
class Positions {

    import org.bitbucket.inkytonik.kiama.util.Memoiser.makeIdMemoiser

    /**
     * Map between a value and a source code position.
     */
    type PositionMap = Memoiser[Any, Position]

    /**
     * Map between value and starting position.
     */
    private val startMap = makeIdMemoiser[Any, Position]()

    /**
     * Map between value and finishing position.
     */
    private val finishMap = makeIdMemoiser[Any, Position]()

    /**
     * Get the optional start position of `t`. If it doesn't have
     * one, return `None`.
     */
    def getStart[T](t : T) : Option[Position] =
        startMap.get(t)

    /**
     * Get the optional finish position of `t`. If it doesn't have one,
     * return `None`.
     */
    def getFinish[T](t : T) : Option[Position] =
        finishMap.get(t)

    /**
     * Set the start position of `t` to `p` if it has not already been set.
     */
    def setStart[T](t : T, p : Position) {
        startMap.putIfAbsent(t, p)
    }

    /**
     * Set the `finish` position of `t` to `p` if it has not already been set.
     */
    def setFinish[T](t : T, p : Position) {
        finishMap.putIfAbsent(t, p)
    }

    /**
     * Set all positions of `t` to `p`.
     */
    def setAllPositions[T](t : T, p : Position) {
        setStart(t, p)
        setFinish(t, p)
    }

    /**
     * Set the start and finish positions of `t` to the positions of `a`
     * if it has them. Return `t`.
     */
    def dupPos[T](a : Any, t : T) : T = {
        startMap.dup(a, t)
        finishMap.dup(a, t)
        t
    }

    /**
     * Set the start and finish positions of `t` to the start positions of `a`
     * and the finish position of `b` if they have them. Return `t`.
     */
    def dupRangePos[T](a : Any, b : Any, t : T) : T = {
        startMap.dup(a, t)
        finishMap.dup(b, t)
        t
    }

    /**
     * Reset the position maps to be empty.
     */
    def reset() {
        startMap.reset()
        finishMap.reset()
    }

    /**
     * Get the source text associated with the substring of a source
     * between given starting and finishing positions. The two positions
     * are assumed to reference the same source. If either of the
     * positions doesn't refer to a valid offset in the source then
     * `None` is returned.
     */
    def substring(s : Position, f : Position) : Option[String] =
        (s.optOffset, f.optOffset) match {
            case (Some(soffset), Some(foffset)) =>
                Some(s.source.content.substring(soffset, foffset))
            case _ =>
                None
        }

    /**
     * If `t` has valid start and finish positions, return the source text
     * associated with `t`. Otherwise, return `None`. It is assumed that
     * the start and finish positions (if present) both refer to the same
     * source.
     */
    def textOf[T](t : T) : Option[String] = {
        (getStart(t), getFinish(t)) match {
            case (Some(start), Some(finish)) =>
                substring(start, finish)
            case _ =>
                None
        }
    }

}

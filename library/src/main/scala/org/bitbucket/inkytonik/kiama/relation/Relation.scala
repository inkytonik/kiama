/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014-2016 Anthony M Sloane, Macquarie University.
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

package org.bitbucket.inkytonik.kiama
package relation

/**
 * A binary relation between values of type `T` and values of type `U`.
 * Constructed from a vector of pairs that constitute the relation's
 * graph.
 */
class Relation[T, U](val graph : Vector[(T, U)]) extends RelationLike[T, U, Relation] {
    val companion = Relation
}

/**
 * Factory for binary relations.
 */
object Relation extends RelationFactory[Relation] {

    import scala.annotation.tailrec
    import scala.collection.immutable.Queue

    /**
     * Make a binary relation from its graph.
     */
    def fromGraph[T, U](graph : Vector[(T, U)]) : Relation[T, U] =
        new Relation[T, U](graph)

    /**
     * Make a graph from the repeated application of `onestep` to `t` and
     * the results that it produces.
     */
    def fromOneStepGraph[T](t : T, onestep : T => Vector[T]) : Vector[(T, T)] = {

        @tailrec
        def loop(pending : Queue[T], graph : Vector[(T, T)]) : Vector[(T, T)] =
            if (pending.isEmpty)
                graph
            else {
                val l = pending.front
                val next = onestep(l)
                val pairs = next.map { case r => (l, r) }
                loop(pending.tail ++ next, graph ++ pairs)
            }

        loop(Queue(t), Vector())

    }

    /**
     * Make a binary relation using the graph produced by `fromOneStepGraph`
     * applied to `t` and `onestep`.
     */
    def fromOneStep[T](t : T, onestep : T => Vector[T]) : Relation[T, T] =
        new Relation[T, T](fromOneStepGraph(t, onestep))

}

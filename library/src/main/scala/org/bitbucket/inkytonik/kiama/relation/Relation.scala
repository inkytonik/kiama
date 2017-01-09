/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014-2017 Anthony M Sloane, Macquarie University.
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
 * Constructed from a memoised cache that maps `T` values to their image.
 */
class Relation[T, U](val graph : RelationGraph[T, U]) extends RelationLike[T, U] {

    /**
     * Invert this relation. In other words, if `(t,u)` is in the relation,
     * then `(u,t)` is in the inverted relation.
     */
    lazy val inverse : RelationLike[U, T] =
        Relation.fromGraph(graph.inverse)

}

/**
 * Factory for binary relations.
 */
object Relation {

    import scala.annotation.tailrec
    import scala.collection.immutable.Queue

    /**
     * Make a graph from a sequence of pairs that describe the mappings.
     */
    def graphFromImages[T, U](pairs : Vector[(T, Vector[U])]) : RelationGraph[T, U] = {
        val graph = new RelationGraph[T, U]
        for ((t, us) <- pairs)
            graph.putAll(t, us)
        graph
    }

    /**
     * Make a graph from the repeated application of `onestep` to `t` and
     * the results that it produces.
     */
    def graphFromOneStep[T](t : T, onestep : T => Vector[T]) : RelationGraph[T, T] = {

        val graph = new RelationGraph[T, T]

        @tailrec
        def loop(pending : Queue[T]) : RelationGraph[T, T] =
            if (pending.isEmpty)
                graph
            else {
                val l = pending.front
                val next = onestep(l)
                graph.putAll(l, next)
                loop(pending.tail ++ next)
            }

        loop(Queue(t))

    }

    /**
     * Make a graph from a sequence of pairs that describe the mappings.
     */
    def graphFromPairs[T, U](pairs : Vector[(T, U)]) : RelationGraph[T, U] = {
        val graph = new RelationGraph[T, U]
        for ((t, u) <- pairs)
            graph.put(t, u)
        graph
    }

    /**
     * Make a graph from a sequence of pairs that describe the inverse mappings.
     */
    def graphFromInversePairs[T, U](pairs : Vector[(T, U)]) : RelationGraph[U, T] = {
        val graph = new RelationGraph[U, T]
        for ((t, u) <- pairs)
            graph.put(u, t)
        graph
    }

    /**
     * Make a relation from the repeated application of `onestep` to `t` and
     * the results that it produces.
     */
    def fromOneStep[T](t : T, onestep : T => Vector[T]) : Relation[T, T] =
        fromGraph(graphFromOneStep(t, onestep))

    /**
     * Make a relation from a sequence of pairs that describe the mappings.
     */
    def fromPairs[T, U](pairs : Vector[(T, U)]) : Relation[T, U] =
        fromGraph(graphFromPairs(pairs))

    /**
     * Make a relation from its graph.
     */
    def fromGraph[V, W](graph : RelationGraph[V, W]) : Relation[V, W] =
        new Relation[V, W](graph)

}

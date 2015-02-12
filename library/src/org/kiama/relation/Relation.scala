/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014-2015 Anthony M Sloane, Macquarie University.
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
package relation

import scala.collection.immutable.Seq

/**
 * A binary relation between values of type `T` and values of type `U`.
 * Constructed from a sequence of pairs that constitute the relation's
 * graph.
 */
class Relation[T,U] (val graph : Seq[(T,U)]) extends RelationLike[T,U,Relation] {
    val companion = Relation
}

/**
 * Factory for binary relations.
 */
object Relation extends RelationFactory[Relation] {

    import scala.collection.mutable.ListBuffer

    /**
     * Make a binary relation from its graph.
     */
    def fromGraph[T,U] (graph : Seq[(T,U)]) : Relation[T,U] =
        new Relation[T,U] (graph)

    /**
     * Make a graph from the repeated application of `onestep` to `t` and
     * the results that it produces.
     */
    def fromOneStepGraph[T] (t : T, onestep : T => Seq[T]) : Seq[(T,T)] = {
        val pending = ListBuffer[T] (t)
        val result = ListBuffer[(T,T)] ()
        while (!pending.isEmpty) {
            val l = pending.remove (0)
            val cs = onestep (l)
            pending.appendAll (cs)
            result.appendAll (cs.map { case r => (l, r) })
        }
        result.toList
    }

    /**
     * Make a binary relation using the graph produced by `fromOneStepGraph`
     * applied to `t` and `onestep`.
     */
    def fromOneStep[T] (t : T, onestep : T => Seq[T]) : Relation[T,T] =
        new Relation[T,T] (fromOneStepGraph (t, onestep))

}
